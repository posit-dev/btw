test_that("btw_tool_search_packages()", {
  local_mocked_bindings(
    pkg_search = mock_pkgsearch
  )

  expect_btw_tool_result(
    btw_tool_search_packages("string interpolation"),
    has_data = TRUE
  )

  expect_equal(
    btw_tool_search_packages("string interpolation", format = "long")@value,
    btw_this(mock_pkgsearch("string interpolation", format = "long"))
  )

  expect_equal(
    btw_tool_search_packages("string interpolation", format = "short")@value,
    btw_this(mock_pkgsearch("string interpolation", format = "short"))
  )
})

test_that("btw_tool_search_packages() snapshots", {
  skip_if_not_macos()
  local_mocked_bindings(
    pkg_search = mock_pkgsearch
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

test_that("btw_tool_search_packages() warns for too many results", {
  skip_if_offline()

  expect_warning(
    btw(pkgsearch::pkg_search("data API"))
  )

  expect_match(
    btw_tool_search_packages("data API")@value,
    "QUERY IS TOO BROAD"
  )
})

test_that("btw_tool_search_package_info()", {
  skip_if_offline()

  search_result <- pkgsearch::cran_package("anyflights")
  tool_result <- btw_tool_search_package_info("anyflights")

  expect_equal(
    tool_result@value,
    btw_this(search_result)
  )

  expect_equal(
    tool_result@extra,
    search_result
  )
})

test_that("btw_tool_search_package_info() snapshots", {
  skip_if_not_macos()

  local_mocked_bindings(
    cran_package = mock_cran_package
  )

  expect_snapshot(
    cli::cat_line(
      btw_tool_search_package_info("anyflights")@value
    )
  )

  expect_snapshot(
    cli::cat_line(btw_this(mock_cran_package("anyflights")))
  )
})
