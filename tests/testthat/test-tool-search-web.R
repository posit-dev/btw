test_that("btw_tool_files_search_ddg() works", {
  skip_if_offline()
  skip_if_not_installed("ddgsearch")
  skip_if_not_installed("ragnar")

  result <- btw_tool_files_search_ddg("dplyr", max_results = 2)

  expect_btw_tool_result(result)

  # Should have 2 rows
  expect_equal(nrow(result@extra$data), 2)
  expect_named(result@extra$data, c("title", "href", "summary", "body"))

  result_full <- btw_tool_files_search_ddg(
    "R programming language",
    max_results = 1
  )
  expect_named(result_full@extra$data, c("title", "href", "summary", "body"))
})
