test_that("btw_client() works with `btw.client` option", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  local_options(
    btw.client = ellmer::chat_anthropic(
      system_prompt = "I like to have my own system prompt."
    )
  )

  with_mocked_platform(ide = "rstudio", {
    chat <- btw_client(path_btw = FALSE)
  })

  expect_match(chat$get_system_prompt(), "I like to have my own system prompt")
  expect_match(chat$get_system_prompt(), "You have access to tools")
  expect_no_match(
    getOption("btw.client")$get_system_prompt(),
    "You have access to tools"
  )

  skip_if_not_macos()
  expect_snapshot(print(chat), transform = scrub_system_info)
})

test_that("btw_client() works basic case", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  data_foo <- mtcars

  expect_error(btw_client(data_foo), class = "rlib_error_dots_nonempty")

  chat <- btw_client()
  expect_s3_class(chat, "Chat")
})

test_that("btw_client() modifies `client` argument in place", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  client <- ellmer::chat_anthropic(
    system_prompt = "I like to make my own chat client.",
  )

  chat <- btw_client(client = client)
  # Modifies in place
  expect_identical(chat, client)
})

test_that("btw_client() accepts a provider string", {
  withr::local_envvar(ANTHROPIC_API_KEY = "beep")

  expected_client <- ellmer::chat_anthropic()
  chat <- btw_client(client = "anthropic")
  expect_equal(chat$get_provider(), expected_client$get_provider())
})

test_that("btw_client() accepts a provider/model string", {
  withr::local_envvar(ANTHROPIC_API_KEY = "beep")

  expected_client <- ellmer::chat_anthropic(
    model = "claude-3-7-sonnet-20250219"
  )
  chat <- btw_client(client = "anthropic/claude-3-7-sonnet-20250219")
  expect_equal(chat$get_provider(), expected_client$get_provider())
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

  skip_if_not_macos()
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

    skip_if_not_macos()
    expect_snapshot(print(chat), transform = scrub_system_info)
  })

  it("uses `llms.txt` in wd and `btw.md` from parent", {
    fs::file_move("../btw-context.md", "../btw.md")

    with_mocked_platform(ide = "rstudio", {
      chat_parent_llms <- btw_client()
    })

    skip_if_not_macos()
    expect_snapshot(print(chat_parent_llms), transform = scrub_system_info)
  })
})


test_that("btw_client() uses `btw.md` with client string", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep"))

  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  writeLines(
    con = "btw.md",
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
  chat <- btw_client()
  expect_equal(chat$get_provider(), expected_client$get_provider())
})

test_that("btw_client() throws if `path_btw` is provided but doesn't exist", {
  expect_error(
    btw_client(path_btw = tempfile())
  )
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
