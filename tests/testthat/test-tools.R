test_that("btw_tools works", {
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
    btw_tools(c("docs", "env", "files_list_files")),
    btw_tools("docs", "env", "files_list_files")
  )
})

test_that("btw_tools can be registered with a chat", {
  withr::local_envvar(ANTHROPIC_API_KEY = "boop")

  # would need to poke inside of the private env to ensure the tools
  # were actually registered, so just check that there are no conditions raised
  ch <- ellmer::chat_anthropic()
  expect_silent(ch$set_tools(btw_tools()))
})

test_that("All btw tools have annotations", {
  local_mocked_bindings(
    # Force conditional tools to always be included in this test
    rstudioapi_has_source_editor_context = function() TRUE
  )

  tools <- btw_tools()
  expect_setequal(names(tools), names(.btw_tools))

  for (name in names(tools)) {
    expect(
      !is.null(tools[[name]]@annotations$title),
      paste0("`", name, "` tool definition is missing annotations")
    )
  }
})
