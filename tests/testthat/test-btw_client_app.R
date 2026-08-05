test_that("app_set_disabled() namespaces controls and preserves an array payload", {
  message <- NULL
  session <- list(
    ns = function(id) paste0("module-", id),
    sendCustomMessage = function(type, value) {
      message <<- list(type = type, value = value)
    }
  )

  app_set_disabled(session, c("model", "clear_chat"), TRUE)

  expect_equal(message$type, "btw_set_disabled")
  expect_equal(
    message$value,
    list(
      ids = list("module-model", "module-clear_chat"),
      disabled = TRUE
    )
  )

  app_set_disabled(session, "tools_controls", FALSE)

  expect_equal(
    message$value,
    list(
      ids = list("module-tools_controls"),
      disabled = FALSE
    )
  )
})

test_that("app_set_client_tools() updates the active client", {
  active_tools <- NULL
  active_client <- list(
    set_tools = function(tools) active_tools <<- tools
  )
  chat <- list(client = active_client)
  available <- list(one = "first", two = "second")

  app_set_client_tools(chat, "two", available)
  expect_equal(active_tools, list(two = "second"))

  app_set_client_tools(chat, character(), available)
  expect_equal(active_tools, list())
})

test_that("app_toggle_tool_group() ignores toggles while streaming", {
  tools <- c("one", "two")

  expect_null(app_toggle_tool_group("one", tools, "streaming"))
  expect_equal(app_toggle_tool_group("one", tools, "idle"), tools)
  expect_equal(app_toggle_tool_group(tools, tools, "idle"), character())
})
