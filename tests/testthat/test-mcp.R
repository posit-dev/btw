test_that("btw_mcp_server errors informatively with bad `tools`", {
  local_mocked_bindings(
    mcp_server = function(...) {
      "The test failed, an error should have been raised."
    },
    .package = "mcptools"
  )

  expect_error(
    class = "btw_unmatched_tool_error",
    btw_mcp_server(tools = "bop")
  )
})

test_that("btw_mcp_server works with a character vector of multiple tool groups", {
  local_enable_tools()
  local_mocked_bindings(
    mcp_server = function(...) "mocked",
    .package = "mcptools"
  )

  expect_equal(btw_mcp_server(tools = c("docs", "env")), "mocked")
})

test_that("btw_mcp_tools() excludes skills group by default", {
  local_enable_tools()
  tools <- btw_mcp_tools()
  tool_names <- vapply(tools, function(t) t@name, character(1))
  expect_false("btw_tool_skill" %in% tool_names)
})
