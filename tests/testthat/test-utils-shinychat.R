test_that("btw_shinychat_version() returns the installed version", {
  skip_if_not_installed("shinychat")

  expect_identical(
    as.character(btw_shinychat_version()),
    as.character(utils::packageVersion("shinychat"))
  )
})

test_that("btw_shinychat_050_object() errors clearly on old shinychat", {
  local_mocked_bindings(
    btw_shinychat_version = function() numeric_version("0.4.0")
  )

  expect_snapshot(
    error = TRUE,
    btw_shinychat_050_object("chat_server")
  )
  expect_error(
    shinychat_chat_attachment(path = tempfile()),
    "requires shinychat 0.5.0"
  )
})

test_that("shinychat 0.5.0 proxies resolve the namespace objects", {
  skip_if_no_shinychat_v05()

  for (fn in c(
    "chat_server",
    "page_chat",
    "chat_attachment",
    "history_options",
    "ConversationStore"
  )) {
    expect_identical(
      btw_shinychat_050_object(fn),
      asNamespace("shinychat")[[fn]]
    )
  }

  # the thin wrappers call through to the resolved objects
  path <- withr::local_tempfile(lines = "hello", fileext = ".md")
  att <- shinychat_chat_attachment(path, name = "hello.md")
  expect_identical(att$name, "hello.md")
  expect_identical(att$mime, "text/markdown")
  expect_identical(
    att$data_url,
    shinychat::chat_attachment(path, name = "hello.md")$data_url
  )
})
