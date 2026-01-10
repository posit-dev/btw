local_enable_tools()
local_sessioninfo_quarto_version()
withr::local_options(btw.client.quiet = TRUE)

describe("btw_client() chat client", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  it("works with `btw.client` option", {
    local_options(
      btw.client = ellmer::chat_anthropic(
        system_prompt = "I like to have my own system prompt."
      )
    )

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_btw = FALSE)
    })

    expect_match(
      chat$get_system_prompt(),
      "I like to have my own system prompt"
    )
    expect_match(chat$get_system_prompt(), "You have access to tools")
    expect_no_match(
      getOption("btw.client")$get_system_prompt(),
      "You have access to tools"
    )

    skip_if_not_snapshot_env()
    expect_snapshot(print(chat), transform = scrub_system_info)
  })

  it("works in the basic case", {
    data_foo <- mtcars

    expect_error(btw_client(data_foo), class = "rlib_error_dots_nonempty")

    chat <- btw_client(path_btw = FALSE)
    expect_s3_class(chat, "Chat")
  })

  it("modifies `client` argument in place", {
    client <- ellmer::chat_anthropic(
      system_prompt = "I like to make my own chat client.",
    )

    chat <- btw_client(client = client, path_btw = FALSE)
    # Modifies in place
    expect_identical(chat, client)
  })

  it("accepts a provider string", {
    expected_client <- ellmer::chat_anthropic()
    chat <- btw_client(client = "anthropic", path_btw = FALSE)
    expect_equal(chat$get_provider(), expected_client$get_provider())
  })

  it("accepts a provider/model string", {
    expected_client <- ellmer::chat_anthropic(
      model = "claude-3-7-sonnet-20250219"
    )
    chat <- btw_client(client = "anthropic/claude-3-7-sonnet-20250219")
    expect_equal(chat$get_provider(), expected_client$get_provider())
  })
})

test_that("btw_client() adds `btw.md` context file to system prompt", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  wd <- withr::local_tempdir(
    tmpdir = file.path(tempdir(), "btw-proj", "subtask")
  )
  withr::local_dir(wd)

  writeLines(
    con = file.path(wd, "..", "btw.md"),
    c(
      "* Prefer solutions that use {tidyverse}",
      "* Always use `=` for assignment",
      "* Always use the native base-R pipe `|>` for piped expressions"
    )
  )

  with_mocked_platform(ide = "rstudio", {
    chat <- btw_client(
      client = ellmer::chat_anthropic(
        system_prompt = "I like to have my own system prompt."
      )
    )
  })

  expect_match(chat$get_system_prompt(), "# Project Context", fixed = TRUE)
  expect_match(
    chat$get_system_prompt(),
    "Always use `=` for assignment",
    fixed = TRUE
  )

  skip_if_not_snapshot_env()
  expect_snapshot(print(chat), transform = scrub_system_info)
})

describe("btw_client() with context files", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep"))

  wd <- withr::local_tempdir(
    tmpdir = file.path(tempdir(), "btw-proj", "subtask")
  )
  withr::local_dir(wd)

  writeLines(
    con = file.path(wd, "..", "btw.md"),
    c(
      "---",
      "client:",
      "  provider: openai",
      "  model: gpt-4o",
      "  system_prompt: I like to have my own system prompt",
      "tools: docs",
      "---",
      "",
      "* Prefer solutions that use {tidyverse}",
      "* Always use `=` for assignment",
      "* Always use the native base-R pipe `|>` for piped expressions"
    )
  )

  it("uses `btw.md` for client settings and system prompt", {
    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client()
    })

    expect_equal(chat$get_model(), "gpt-4o")
    expect_true(inherits(
      chat$.__enclos_env__$private$provider,
      "ellmer::ProviderOpenAI"
    ))

    expect_match(chat$get_system_prompt(), "# Project Context", fixed = TRUE)
    expect_match(
      chat$get_system_prompt(),
      "Always use `=` for assignment",
      fixed = TRUE
    )
  })

  it("uses AGENTS.md as an alias of btw.md", {
    with_mocked_platform(ide = "rstudio", {
      chat_btw <- btw_client()
    })

    fs::file_move(fs::path(wd, "../btw.md"), fs::path(wd, "../AGENTS.md"))
    withr::defer(
      fs::file_move(fs::path(wd, "../AGENTS.md"), fs::path(wd, "../btw.md"))
    )

    with_mocked_platform(ide = "rstudio", {
      chat_agents <- btw_client()
    })

    expect_equal(chat_agents, chat_btw)
  })

  it("uses CLAUDE.md but ignores its YAML frontmatter", {
    # Move btw.md out of the way so CLAUDE.md is found
    fs::file_move(fs::path(wd, "../btw.md"), fs::path(wd, "../btw-backup.md"))
    withr::defer(
      fs::file_move(fs::path(wd, "../btw-backup.md"), fs::path(wd, "../btw.md"))
    )

    # Create CLAUDE.md with YAML frontmatter that has client config
    writeLines(
      con = file.path(wd, "..", "CLAUDE.md"),
      c(
        "---",
        "client:",
        "  provider: openai",
        "  model: gpt-5",
        "tools: btw_tool_docs_vignette",
        "---",
        "",
        "* Use CLAUDE.md style guidelines",
        "* YAML config should be ignored"
      )
    )
    withr::defer(unlink(file.path(wd, "..", "CLAUDE.md")))

    with_mocked_platform(ide = "rstudio", {
      chat_claude <- btw_client()
    })

    # Body content should be in system prompt
    expect_match(
      chat_claude$get_system_prompt(),
      "Use CLAUDE.md style guidelines",
      fixed = TRUE
    )
    expect_match(
      chat_claude$get_system_prompt(),
      "YAML config should be ignored",
      fixed = TRUE
    )

    # YAML config should be ignored
    client_default <- btw_default_chat_client()
    expect_equal(chat_claude$get_model(), client_default$get_model())
    expect_equal(
      chat_claude$get_provider()@name,
      client_default$get_provider()@name
    )
    expect_gt(length(chat_claude$get_tools()), 1)
  })

  it("includes llms.txt content in system prompt", {
    writeLines(
      con = file.path(wd, "llms.txt"),
      "EXTRA CONTEXT FROM llms.txt"
    )

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client()
    })

    expect_match(chat$get_system_prompt(), "# Project Context", fixed = TRUE)
    expect_match(
      chat$get_system_prompt(),
      "Always use `=` for assignment",
      fixed = TRUE
    )
    expect_match(
      chat$get_system_prompt(),
      "EXTRA CONTEXT FROM llms.txt",
      fixed = TRUE
    )
  })

  it("finds `btw.md` in parent directories", {
    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_llms_txt = FALSE)
    })

    fs::file_move("../btw.md", "../btw-context.md")
    with_mocked_platform(ide = "rstudio", {
      chat_parent <-
        btw_client(path_btw = "../btw-context.md", path_llms_txt = FALSE)
    })

    expect_equal(
      chat_parent$get_system_prompt(),
      chat$get_system_prompt()
    )

    skip_if_not_snapshot_env()
    expect_snapshot(print(chat), transform = scrub_system_info)
  })

  it("uses `llms.txt` in wd and `btw.md` from parent", {
    fs::file_move("../btw-context.md", "../btw.md")

    with_mocked_platform(ide = "rstudio", {
      chat_parent_llms <- btw_client()
    })

    skip_if_not_snapshot_env()
    expect_snapshot(print(chat_parent_llms), transform = scrub_system_info)
  })

  it("accepts a string for `client`", {
    btw_md <- withr::local_tempfile(fileext = ".md")

    writeLines(
      con = btw_md,
      c(
        "---",
        "client: openai/gpt-4.1-nano",
        "tools: docs",
        "---",
        "",
        "* Prefer solutions that use {tidyverse}",
        "* Always use `=` for assignment",
        "* Always use the native base-R pipe `|>` for piped expressions"
      )
    )

    expected_client <- ellmer::chat_openai(model = "gpt-4.1-nano")
    chat <- btw_client(path_btw = btw_md)
    expect_equal(chat$get_provider(), expected_client$get_provider())
  })

  it("throws if `path_btw` is provided but doesn't exist", {
    expect_error(
      btw_client(path_btw = tempfile())
    )
  })
})

describe("btw_client() project vs user settings", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep", ANTHROPIC_API_KEY = "boop"))

  project_dir <- withr::local_tempdir("btw-test-project-")
  withr::local_dir(project_dir)

  path_user_btw <- withr::local_tempfile(fileext = ".md")
  local_mocked_bindings(
    path_find_user = function(filename) {
      if (filename == "btw.md") path_user_btw else NULL
    }
  )

  it("falls through to use client settings from user-level btw.md", {
    writeLines(
      con = path_user_btw,
      c(
        "---",
        "client:",
        "  provider: openai",
        "  model: gpt-4o",
        "---",
        "User level context"
      )
    )
    withr::defer(unlink(path_user_btw))

    # Create project-level btw.md with different client settings
    writeLines(
      con = "btw.md",
      c(
        "---",
        "client:",
        "  provider: anthropic",
        "  model: claude-3-5-sonnet-20241022",
        "---",
        "Project level context"
      )
    )
    withr::defer(unlink("btw.md"))

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_llms_txt = FALSE)
    })

    # Should use project's client settings
    expect_equal(chat$get_model(), "claude-3-5-sonnet-20241022")
    expect_s3_class(chat$get_provider(), "ellmer::ProviderAnthropic")

    skip_if_not_snapshot_env()
    expect_snapshot(print(chat), transform = scrub_system_info)
  })

  it("falls back to user client settings when project has no client", {
    # User-level btw.md with client settings
    writeLines(
      c(
        "---",
        "client:",
        "  provider: openai",
        "  model: gpt-4o",
        "---",
        "User level context"
      ),
      path_user_btw
    )
    withr::defer(unlink(path_user_btw))

    # Project-level btw.md WITHOUT client field
    writeLines(
      con = "btw.md",
      c(
        "---",
        "tools: docs",
        "---",
        "Project level context only"
      )
    )
    withr::defer(unlink("btw.md"))

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_llms_txt = FALSE)
    })

    # Should fall back to user's client settings
    expect_equal(chat$get_model(), "gpt-4o")
    expect_s3_class(chat$get_provider(), "ellmer::ProviderOpenAI")

    skip_if_not_snapshot_env()
    expect_snapshot(print(chat), transform = scrub_system_info)
  })

  it("concatenates user and project prompts with separator", {
    # User-level btw.md with prompt
    writeLines(
      c(
        "---",
        "client:",
        "  provider: openai",
        "---",
        "USER_GLOBAL_RULES"
      ),
      path_user_btw
    )
    withr::defer(unlink(path_user_btw))

    # Project-level AGENTS.md with prompt
    writeLines(
      con = "AGENTS.md",
      c(
        "PROJECT_SPECIFIC_RULES"
      )
    )
    withr::defer(unlink("AGENTS.md"))

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_llms_txt = FALSE)
    })

    system_prompt <- chat$get_system_prompt()

    # Should contain both prompts
    expect_match(system_prompt, "USER_GLOBAL_RULES", fixed = TRUE)
    expect_match(system_prompt, "PROJECT_SPECIFIC_RULES", fixed = TRUE)

    # Should have separator between them
    expect_match(system_prompt, "\n\n---\n\n", fixed = TRUE)

    # User prompt should come first
    user_pos <- gregexpr("USER_GLOBAL_RULES", system_prompt)[[1]][1]
    project_pos <- gregexpr("PROJECT_SPECIFIC_RULES", system_prompt)[[1]][1]
    expect_true(user_pos < project_pos)
  })

  it("deep merges options from user and project", {
    writeLines(
      c(
        "---",
        "client:",
        "  provider: openai",
        "options:",
        "  cache_size: 100",
        "  timeout: 30",
        "---",
        "User level"
      ),
      path_user_btw
    )
    withr::defer(unlink(path_user_btw))

    writeLines(
      con = "btw.md",
      c(
        "---",
        "client:",
        "  provider: openai",
        "options:",
        "  timeout: 60",
        "  feature_x: true",
        "---",
        "Project level"
      )
    )
    withr::defer(unlink("btw.md"))

    config <- read_btw_file()

    # Should have all options, with project overriding user
    expect_equal(config$options$btw.cache_size, 100) # From user
    expect_equal(config$options$btw.timeout, 60) # From project (overrides user)
    expect_equal(config$options$btw.feature_x, TRUE) # From project
  })

  it("uses only project tools when defined", {
    writeLines(
      c(
        "---",
        "client:",
        "  provider: openai",
        "tools: [env, files]",
        "---",
        "User level"
      ),
      path_user_btw
    )
    withr::defer(unlink(path_user_btw))

    writeLines(
      con = "btw.md",
      c(
        "---",
        "client:",
        "  provider: openai",
        "tools: docs",
        "---",
        "Project level"
      )
    )
    withr::defer(unlink("btw.md"))

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_llms_txt = FALSE)
    })

    tool_names <- names(chat$get_tools())
    # Should have docs tools from project
    expect_true(any(grepl("btw_tool_docs", tool_names)))
    # Should NOT have env/files tools from user
    expect_false(any(grepl("btw_tool_env", tool_names)))
    expect_false(any(grepl("btw_tool_files", tool_names)))
  })

  it("uses user tools when project has no tools", {
    writeLines(
      c(
        "---",
        "client:",
        "  provider: openai",
        "tools: docs",
        "---",
        "User level"
      ),
      path_user_btw
    )
    withr::defer(unlink(path_user_btw))

    writeLines(
      con = "btw.md",
      c(
        "---",
        "client:",
        "  provider: openai",
        "---",
        "Project level"
      )
    )
    withr::defer(unlink("btw.md"))

    with_mocked_platform(ide = "rstudio", {
      chat <- btw_client(path_llms_txt = FALSE)
    })

    tool_names <- names(chat$get_tools())
    # Should have docs tools from user
    expect_true(any(grepl("btw_tool_docs", tool_names)))
  })
})

test_that("btw_client() throws for deprecated `model` and `provider` fields in btw.md", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep"))

  wd <- withr::local_tempdir(
    tmpdir = file.path(tempdir(), "btw-test-client-deprecated")
  )
  withr::local_dir(wd)

  writeLines(
    con = "btw.md",
    c(
      "---",
      "provider: openai",
      "---",
      "",
      "* Prefer solutions that use {tidyverse}",
      "* Always use `=` for assignment",
      "* Always use the native base-R pipe `|>` for piped expressions"
    )
  )

  expect_error(
    btw_client(),
    "provider"
  )

  writeLines(
    con = "btw.md",
    c(
      "---",
      "model: gpt-4.1-mini",
      "---",
      "",
      "* Prefer solutions that use {tidyverse}",
      "* Always use `=` for assignment",
      "* Always use the native base-R pipe `|>` for piped expressions"
    )
  )

  expect_error(
    btw_client(),
    "model"
  )
})

describe("remove_hidden_content()", {
  it("removes content after single HIDE comment", {
    expect_equal(
      remove_hidden_content(c("one", "<!-- HIDE -->", "two")),
      "one"
    )
  })

  it("removes content after multiple HIDE comments", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- HIDE -->",
        "two",
        "<!-- HIDE -->",
        "three"
      )),
      "one"
    )
  })

  it("removes content between HIDE and /HIDE with no closing", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- HIDE -->",
        "two",
        "<!-- HIDE -->",
        "three",
        "<!-- /HIDE -->"
      )),
      "one"
    )
  })

  it("removes content with nested HIDE comments and single /HIDE", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- HIDE -->",
        "two",
        "<!-- HIDE -->",
        "three",
        "<!-- /HIDE -->",
        "four"
      )),
      "one"
    )
  })

  it("handles properly nested HIDE/HIDE blocks", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- HIDE -->",
        "two",
        "<!-- HIDE -->",
        "three",
        "<!-- /HIDE -->",
        "four",
        "<!-- /HIDE -->",
        "five"
      )),
      c("one", "five")
    )
  })

  it("removes all content when HIDE blocks are not properly closed", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- HIDE -->",
        "two",
        "<!-- HIDE -->",
        "three",
        "<!-- /HIDE -->",
        "four",
        "<!-- /HIDE -->"
      )),
      "one"
    )
  })

  it("returns empty vector when input is empty", {
    expect_equal(
      remove_hidden_content(character(0)),
      character(0)
    )
  })

  it("returns original content when no HIDE comments present", {
    expect_equal(
      remove_hidden_content(c("one", "two", "three", "four")),
      c("one", "two", "three", "four")
    )
  })

  it("handles single /HIDE without opening HIDE", {
    expect_equal(
      remove_hidden_content(c("one", "two", "<!-- /HIDE -->", "three")),
      c("one", "two", "<!-- /HIDE -->", "three")
    )
  })

  it("removes everything when HIDE is at the beginning", {
    expect_equal(
      remove_hidden_content(c("<!-- HIDE -->", "one", "two", "three")),
      character(0)
    )
  })

  it("handles multiple separate HIDE/HIDE blocks", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- HIDE -->",
        "hidden1",
        "<!-- /HIDE -->",
        "two",
        "<!-- HIDE -->",
        "hidden2",
        "<!-- /HIDE -->",
        "three"
      )),
      c("one", "two", "three")
    )
  })

  it("handles HIDE comment as last element", {
    expect_equal(
      remove_hidden_content(c("one", "two", "<!-- HIDE -->")),
      c("one", "two")
    )
  })

  it("handles /HIDE comment as first element", {
    expect_equal(
      remove_hidden_content(c("<!-- /HIDE -->", "one", "two")),
      c("<!-- /HIDE -->", "one", "two")
    )
  })

  it("handles unmatched /HIDE comment", {
    expect_equal(
      remove_hidden_content(c(
        "one",
        "<!-- /HIDE -->",
        "two",
        "<!-- HIDE -->",
        "three"
      )),
      c("one", "<!-- /HIDE -->", "two")
    )
  })

  it("preserves content between multiple closed HIDE blocks", {
    expect_equal(
      remove_hidden_content(c(
        "start",
        "<!-- HIDE -->",
        "hidden1",
        "<!-- /HIDE -->",
        "middle",
        "<!-- HIDE -->",
        "hidden2",
        "<!-- /HIDE -->",
        "end"
      )),
      c("start", "middle", "end")
    )
  })
})

test_that("btw_client() accepts a list of tools in `tools` argument", {
  withr::local_dir(withr::local_tempdir()) # avoid any user/global btw.md files
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  chat <- btw_client(tools = btw_tools("docs"))

  expect_true(
    all(vapply(
      chat$get_tools(),
      inherits,
      logical(1),
      "ellmer::ToolDef"
    ))
  )

  expect_true(
    all(grepl("btw_tool_docs", names(chat$get_tools())))
  )

  tool <- ellmer::tool(
    function(x) x + 1,
    name = "add_one",
    description = "Add one",
    arguments = list(
      x = ellmer::type_number("A number")
    )
  )
  expect_error(
    btw_client(tools = tool)
  )

  chat <- btw_client(tools = list(tool))
  expect_identical(chat$get_tools()[[1]], tool)

  chat_combo <- btw_client(tools = list("docs_vignette", tool))
  expect_identical(chat_combo$get_tools()[[1]], btw_tools("docs_vignette")[[1]])
  expect_identical(chat_combo$get_tools()[[2]], tool)

  chat_no_tools <- btw_client(tools = FALSE)
  expect_identical(chat_no_tools$get_tools(), list())

  chat_btw_tools <- btw_client(tools = "docs")
  expect_identical(chat_btw_tools$get_tools(), btw_tools("docs"))
})

test_that("btw_client() throws for invalid `tools` argument", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  expect_error(
    btw_client(tools = "not_a_tool")
  )

  expect_error(
    btw_client(tools = 42)
  )

  expect_error(
    btw_client(tools = c(btw_tools()[[1]], list(42))),
    "tools\\[\\[2\\]\\]"
  )
})
