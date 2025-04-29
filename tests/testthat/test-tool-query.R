test_that("btw_tool_env_query_data_frame() works", {
  # can run a simple query
  expect_snapshot(
    btw_tool_env_query_data_frame(
      "SELECT mpg FROM mtcars LIMIT 5;",
      "mtcars"
    )
  )

  # can run a query against the same table twice
  expect_snapshot(
    btw_tool_env_query_data_frame(
      "SELECT mpg FROM mtcars LIMIT 5;",
      "mtcars"
    )
  )
})
