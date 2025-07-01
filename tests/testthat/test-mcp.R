test_that("btw_mcp_server errors informatively with bad `tools`", {
  expect_error(
    class = "btw_unmatched_tool_error",
    btw_mcp_server(tools = "bop")
  )
})
