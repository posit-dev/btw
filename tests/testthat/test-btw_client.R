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

  chat <- btw_client(data_foo)
  expect_s3_class(chat, "Chat")
  expect_equal(
    chat$get_turns()[[1]]@contents[[1]],
    btw(data_foo)
  )
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
