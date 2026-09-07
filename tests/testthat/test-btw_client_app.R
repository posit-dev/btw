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

test_that("app_replay_messages() forwards messages with roles", {
  appended <- list()
  chat <- list(append = function(content, role = "assistant") {
    appended[[length(appended) + 1]] <<- list(content = content, role = role)
  })

  app_replay_messages(chat, list("hello", list(role = "user", content = "hi")))

  expect_equal(
    appended,
    list(
      list(content = "hello", role = "assistant"),
      list(content = "hi", role = "user")
    )
  )
})

test_that("btw_app shells render both the page_chat and sidebar layouts", {
  withr::local_envvar(BTW_IN_TESTING = "true")
  fake <- list(get_tools = function() list(), get_model = function() {
    "test-model"
  })

  app <- btw_app_from_client(client = fake, page_style = "page_chat")
  html <- as.character(app$ui(list()))
  expect_match(html, "shiny-chat-page", fixed = TRUE)
  expect_match(html, "show_tools", fixed = TRUE)
  expect_match(html, "show_sys_prompt", fixed = TRUE)
  expect_match(html, "status_bar", fixed = TRUE)
  expect_match(html, "tools_offcanvas", fixed = TRUE)
  expect_no_match(html, "clear_chat", fixed = TRUE)

  app_legacy <- btw_app_from_client(client = fake, page_style = "sidebar")
  html_legacy <- suppressWarnings(as.character(app_legacy$ui(list())))
  expect_match(html_legacy, "tools_sidebar", fixed = TRUE)
  expect_match(html_legacy, "chat-chat", fixed = TRUE)
})
