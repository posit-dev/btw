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
