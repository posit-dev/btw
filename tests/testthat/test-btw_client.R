test_that("btw_client() works with `btw.chat_client` option", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  local_options(
    btw.chat_client = ellmer::chat_claude(
      system_prompt = "I like to have my own system prompt."
    )
  )

  chat <- btw_client()
  expect_match(chat$get_system_prompt(), "I like to have my own system prompt")
  expect_match(chat$get_system_prompt(), "You have access to tools")
  expect_no_match(
    getOption("btw.chat_client")$get_system_prompt(),
    "You have access to tools"
  )

  skip_if_not_macos()
  expect_snapshot(print(chat))
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

  client <- ellmer::chat_claude(
    system_prompt = "I like to make my own chat client.",
  )

  chat <- btw_client(client = client)
  # Modifies in place
  expect_identical(chat, client)
})

test_that("btw_client() adds `.btw` context file to system prompt", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))
  wd <- withr::local_tempdir(
    tmpdir = file.path(tempdir(), "btw-proj", "subtask")
  )
  withr::local_dir(wd)

  writeLines(
    con = file.path(wd, "..", ".btw"),
    c(
      "* Prefer solutions that use {tidyverse}",
      "* Always use `=` for assignment",
      "* Always use the native base-R pipe `|>` for piped expressions"
    )
  )

  chat <- btw_client(
    client = ellmer::chat_claude(
      system_prompt = "I like to have my own system prompt."
    )
  )

  expect_match(chat$get_system_prompt(), "# Project Context", fixed = TRUE)
  expect_match(
    chat$get_system_prompt(),
    "Always use `=` for assignment",
    fixed = TRUE
  )

  skip_if_not_macos()
  expect_snapshot(print(chat))
})
