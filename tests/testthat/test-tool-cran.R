test_that("btw_tool_cran_search()", {
  local_mocked_bindings(
    pkg_search = mock_pkgsearch
  )

  expect_btw_tool_result(
    btw_tool_cran_search("string interpolation"),
    has_data = TRUE
  )

  expect_equal(
    btw_tool_cran_search("string interpolation", format = "long")@value,
    btw_this(mock_pkgsearch("string interpolation", format = "long"))
  )

  expect_equal(
    btw_tool_cran_search("string interpolation", format = "short")@value,
    btw_this(mock_pkgsearch("string interpolation", format = "short"))
  )
})

test_that("btw_tool_cran_search() snapshots", {
  skip_if_not_snapshot_env()
  local_mocked_bindings(
    pkg_search = mock_pkgsearch
  )

  expect_snapshot(
    cli::cat_line(
      btw_tool_cran_search("string interpolation", format = "long")@value
    )
  )

  expect_snapshot(
    cli::cat_line(
      btw_tool_cran_search("string interpolation", format = "short")@value
    )
  )
})

test_that("btw_tool_cran_search() warns for too many results", {
  skip_if_offline()

  expect_warning(
    btw(pkgsearch::pkg_search("data API"))
  )

  expect_match(
    btw_tool_cran_search("data API")@value,
    "QUERY IS TOO BROAD"
  )
})

test_that("btw_tool_cran_package()", {
  skip_if_offline()

  search_result <- pkgsearch::cran_package("anyflights")
  tool_result <- btw_tool_cran_package("anyflights")

  expect_equal(
    tool_result@value,
    btw_this(search_result)
  )

  expect_equal(
    tool_result@extra$info,
    search_result
  )
})

test_that("btw_tool_cran_package() snapshots", {
  skip_if_not_snapshot_env()

  local_mocked_bindings(
    cran_package = mock_cran_package
  )

  expect_snapshot(
    cli::cat_line(
      btw_tool_cran_package("anyflights")@value
    )
  )

  expect_snapshot(
    cli::cat_line(btw_this(mock_cran_package("anyflights")))
  )
})
