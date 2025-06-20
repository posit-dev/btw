test_that("btw_mcp_server errors informatively with bad `tools`", {
  expect_snapshot(error = TRUE, btw_mcp_server(tools = "bop"))
})
