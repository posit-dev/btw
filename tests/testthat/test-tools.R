test_that("btw_tools works", {
  local_enable_tools()

  # returns .btw_tools as-is with no `tools` argument
  expect_equal(btw_tools(), as_ellmer_tools(.btw_tools))
  expect_equal(btw_tools(NULL), as_ellmer_tools(.btw_tools))

  # can filter by group
  btw_tools_docs <- btw_tools("docs")
  expect_true(length(btw_tools_docs) < length(btw_tools()))
  expect_true(all(grepl("btw_tool_docs", names(btw_tools_docs))))
  expect_true(all(vapply(
    btw_tools_docs,
    inherits,
    logical(1),
    "ellmer::ToolDef"
  )))

  # can filter by name
  btw_tools_data_frame <- btw_tools("btw_tool_env_describe_data_frame")
  expect_equal(length(btw_tools_data_frame), 1)
  expect_equal(names(btw_tools_data_frame), "btw_tool_env_describe_data_frame")
  expect_true(all(vapply(
    btw_tools_docs,
    inherits,
    logical(1),
    "ellmer::ToolDef"
  )))

  # can filter by a combo
  btw_tools_ad_hoc <- btw_tools(
    "docs",
    "btw_tool_env_describe_data_frame"
  )
  expect_equal(
    length(btw_tools_ad_hoc),
    length(btw_tools_docs) + length(btw_tools_data_frame)
  )
  expect_true(all(vapply(
    btw_tools_docs,
    inherits,
    logical(1),
    "ellmer::ToolDef"
  )))
})

test_that("btw_tools() with character vector", {
  expect_equal(
    btw_tools(c("docs", "env", "files_list")),
    btw_tools("docs", "env", "files_list")
  )
})

test_that("btw_tools can be registered with a chat", {
  withr::local_envvar(ANTHROPIC_API_KEY = "boop")
  local_enable_tools()

  # would need to poke inside of the private env to ensure the tools
  # were actually registered, so just check that there are no conditions raised
  ch <- ellmer::chat_anthropic()
  expect_silent(ch$set_tools(btw_tools()))
})

test_that("All btw tools have annotations", {
  # Force conditional tools to always be included in this test
  local_enable_tools()

  tools <- btw_tools()
  expect_setequal(names(tools), names(.btw_tools))

  for (name in names(tools)) {
    expect(
      !is.null(tools[[name]]@annotations$title),
      paste0("`", name, "` tool definition is missing annotations")
    )
  }
})

test_that("All btw tools have a btw_can_register() annotation function", {
  # Force conditional tools to always be included in this test
  local_enable_tools()

  tools <- btw_tools()
  expect_setequal(names(tools), names(.btw_tools))

  missing <- c()

  for (name in names(tools)) {
    if (!is.function(tools[[name]]@annotations$btw_can_register)) {
      missing <- c(missing, name)
    }
  }

  missing_btw_can_register <- missing
  expect_equal(missing_btw_can_register, c())
})

test_that("built_in_tool_info() returns metadata for known tools", {
  info <- built_in_tool_info("web_search")
  expect_equal(info$title, "Web Search")
  expect_true(nzchar(info$description))
  expect_true(info$read_only_hint)
  expect_true(info$open_world_hint)

  info <- built_in_tool_info("web_fetch")
  expect_equal(info$title, "Web Fetch")
  expect_true(nzchar(info$description))
  expect_true(info$read_only_hint)
  expect_false(info$open_world_hint)
})

test_that("built_in_tool_info() returns sensible defaults for unknown tools", {
  info <- built_in_tool_info("code_execution")
  expect_equal(info$title, "Code execution")
  expect_equal(info$description, "A provider built-in code_execution tool.")
  expect_null(info$read_only_hint)
  expect_null(info$open_world_hint)
})

test_that("wrap_built_in_tools() wraps ToolBuiltIn tools", {
  skip_if(is.null(ellmer_ToolBuiltIn()), "ellmer ToolBuiltIn not available")
  skip_if(is.null(BtwToolBuiltIn), "BtwToolBuiltIn class not available")

  withr::local_envvar(ANTHROPIC_API_KEY = "beep")
  ch <- ellmer::chat_anthropic()

  ToolBuiltIn <- ellmer_ToolBuiltIn()
  mock_tool <- ToolBuiltIn(name = "web_search", json = list(type = "test"))
  ch$set_tools(list(mock_tool))

  wrap_built_in_tools(ch)
  tools <- ch$get_tools()

  expect_length(tools, 1)
  expect_true(S7::S7_inherits(tools[[1]], BtwToolBuiltIn))
  expect_equal(tools[[1]]@title, "Web Search")
  expect_equal(tools[[1]]@annotations$btw_group, "built-in")
  expect_true(tools[[1]]@annotations$read_only_hint)
})

test_that("wrap_built_in_tools() does not modify non-built-in tools", {
  skip_if(is.null(ellmer_ToolBuiltIn()), "ellmer ToolBuiltIn not available")
  skip_if(is.null(BtwToolBuiltIn), "BtwToolBuiltIn class not available")

  withr::local_envvar(ANTHROPIC_API_KEY = "beep")
  ch <- ellmer::chat_anthropic()

  local_enable_tools()
  btw_tool <- btw_tools("docs_help_page")[[1]]
  ch$set_tools(list(btw_tool))

  wrap_built_in_tools(ch)
  tools <- ch$get_tools()

  expect_length(tools, 1)
  expect_s3_class(tools[[1]], "ellmer::ToolDef")
  expect_false(S7::S7_inherits(tools[[1]], BtwToolBuiltIn))
})

test_that("wrap_built_in_tools() is idempotent", {
  skip_if(is.null(ellmer_ToolBuiltIn()), "ellmer ToolBuiltIn not available")
  skip_if(is.null(BtwToolBuiltIn), "BtwToolBuiltIn class not available")

  withr::local_envvar(ANTHROPIC_API_KEY = "beep")
  ch <- ellmer::chat_anthropic()

  ToolBuiltIn <- ellmer_ToolBuiltIn()
  mock_tool <- ToolBuiltIn(name = "web_search", json = list(type = "test"))
  ch$set_tools(list(mock_tool))

  wrap_built_in_tools(ch)
  first_pass <- ch$get_tools()

  wrap_built_in_tools(ch)
  second_pass <- ch$get_tools()

  expect_equal(length(first_pass), length(second_pass))
  expect_equal(first_pass[[1]]@title, second_pass[[1]]@title)
})

test_that("wrap_built_in_tools() degrades gracefully when BtwToolBuiltIn is NULL", {
  withr::local_envvar(ANTHROPIC_API_KEY = "beep")
  ch <- ellmer::chat_anthropic()
  ch$set_tools(list())

  local_mocked_bindings(BtwToolBuiltIn = NULL)
  expect_no_error(wrap_built_in_tools(ch))
  expect_length(ch$get_tools(), 0)
})

test_that("$tool() returns a tool definition for all .btw_tools", {
  local_enable_tools(
    has_chromote = FALSE,
    rstudioapi_has_source_editor_context = FALSE,
    btw_can_register_git_tool = FALSE
  )

  for (name in names(.btw_tools)) {
    expect_s3_class(.btw_tools[[!!name]]$tool(), "ellmer::ToolDef")
    expect_equal(.btw_tools[[!!name]]$tool()@name, name)
  }
})
