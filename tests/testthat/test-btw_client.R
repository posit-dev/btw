test_that("btw_client() works with `btw.client` option", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  local_options(
    btw.client = ellmer::chat_anthropic(
      system_prompt = "I like to have my own system prompt."
    )
  )

  with_mocked_platform(ide = "rstudio", {
    chat <- btw_client()
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

test_that("btw_client() uses `btw.md` context file for client settings", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep"))

  wd <- withr::local_tempdir(
    tmpdir = file.path(tempdir(), "btw-proj", "subtask")
  )
  withr::local_dir(wd)

  writeLines(
    con = file.path(wd, "..", "btw.md"),
    c(
      "---",
      "provider: openai",
      "model: gpt-4o",
      "system_prompt: I like to have my own system prompt",
      "tools: docs",
      "---",
      "",
      "* Prefer solutions that use {tidyverse}",
      "* Always use `=` for assignment",
      "* Always use the native base-R pipe `|>` for piped expressions"
    )
  )

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

  fs::file_move("../btw.md", "../btw-context.md")
  with_mocked_platform(ide = "rstudio", {
    chat_parent <-
      btw_client(path_btw = "../btw-context.md")
  })
  expect_equal(
    chat_parent$get_system_prompt(),
    chat$get_system_prompt()
  )

  skip_if_not_macos()
  expect_snapshot(print(chat), transform = scrub_system_info)
})

test_that("btw_client() throws if `path_btw` is provided but doesn't exist", {
  expect_error(
    btw_client(path_btw = tempfile())
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
