test_that("btw_tool_search_packages()", {
  local_mocked_bindings(
    pkg_search = mock_pkgsearch
  )

  expect_btw_tool_result(
    btw_tool_search_packages("string interpolation"),
    has_data = TRUE
  )

  expect_snapshot(
    cli::cat_line(
      btw_tool_search_packages("string interpolation", format = "long")@value
    )
  )

  expect_snapshot(
    cli::cat_line(
      btw_tool_search_packages("string interpolation", format = "short")@value
    )
  )
})
