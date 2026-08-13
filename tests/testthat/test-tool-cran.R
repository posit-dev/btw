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
  skip_on_cran()
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
  skip_on_cran()
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

test_that("btw_can_register_cran_versions() requires an internet connection", {
  local_mocked_bindings(btw_has_internet = function() FALSE)
  expect_false(btw_can_register_cran_versions())
})

test_that("btw_tool_cran_versions registration requires internet", {
  local_mocked_bindings(btw_can_register_cran_versions = function() FALSE)
  expect_false("btw_tool_cran_versions" %in% names(btw_tools()))
  expect_false("btw_tool_cran_versions" %in% names(btw_tools("cran")))

  local_mocked_bindings(btw_can_register_cran_versions = function() TRUE)
  expect_true("btw_tool_cran_versions" %in% names(btw_tools("cran")))
})

test_that("btw_tool_cran_versions() combines archive and current releases", {
  archive <- xml2::read_html(
    paste(
      "<table>",
      "<tr><td></td><td><a href='dplyr_1.0.0.tar.gz'>dplyr_1.0.0.tar.gz</a></td><td>2020-05-29 17:00</td><td></td></tr>",
      "<tr><td></td><td><a href='dplyr_1.1.0.tar.gz'>dplyr_1.1.0.tar.gz</a></td><td>2023-03-10 12:00</td><td></td></tr>",
      "</table>"
    )
  )
  local_mocked_bindings(
    cran_archive_page = function(package_name) archive,
    cran_current_version = function(package_name) {
      cran_versions_data(
        version = "1.1.4",
        released = as.Date("2023-11-17"),
        released_at = "2023-11-17T00:00:00Z",
        current = TRUE,
        tarball_url = "https://cran.r-project.org/src/contrib/dplyr_1.1.4.tar.gz"
      )
    }
  )

  result <- btw_tool_cran_versions("dplyr")
  expect_btw_tool_result(result, has_data = TRUE)
  expect_equal(result@extra$data$version, c("1.1.4", "1.1.0", "1.0.0"))
  expect_equal(
    as.character(result@extra$data$released),
    c("2023-11-17", "2023-03-10", "2020-05-29")
  )
  expect_equal(
    result@extra$data$released_at,
    c("2023-11-17T00:00:00Z", "2023-03-10T12:00:00Z", "2020-05-29T17:00:00Z")
  )
  expect_equal(result@extra$data$current, c(TRUE, FALSE, FALSE))
  expect_equal(
    result@extra$data$tarball_url,
    c(
      "https://cran.r-project.org/src/contrib/dplyr_1.1.4.tar.gz",
      "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.1.0.tar.gz",
      "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.0.0.tar.gz"
    )
  )
  expect_match(result@value, "CRAN releases for dplyr")
})

test_that("cran_versions() supports archived packages", {
  local_mocked_bindings(
    cran_archive_versions = function(package_name) {
      cran_versions_data("0.1.0", as.Date("2020-01-01"))
    },
    cran_current_version = function(package_name) cran_versions_data()
  )

  expect_equal(cran_versions("archivedpkg")$version, "0.1.0")
})

test_that("cran_versions_data() returns an empty, typed result", {
  result <- cran_versions_data()
  expect_equal(nrow(result), 0)
  expect_named(
    result,
    c("version", "released", "released_at", "current", "tarball_url")
  )
  expect_s3_class(result$released, "Date")
  expect_type(result$current, "logical")
})

test_that("cran_versions() filters releases by inclusive date range", {
  local_mocked_bindings(
    cran_current_version = function(package_name) {
      cran_versions_data("1.2.0", as.Date("2024-01-15"))
    },
    cran_archive_versions = function(package_name) {
      cran_versions_data(
        c("1.1.0", "1.0.0"),
        as.Date(c("2023-06-01", "2022-12-31"))
      )
    }
  )

  expect_equal(
    cran_versions("dplyr", after = "2023-06-01", before = "2024-01-15")$version,
    c("1.2.0", "1.1.0")
  )
  expect_equal(nrow(cran_versions("dplyr", after = "2025-01-01")), 0)
  expect_error(cran_versions("dplyr", after = "not-a-date"), "ISO date")
  expect_error(
    cran_versions("dplyr", after = "2024-01-02", before = "2024-01-01"),
    "on or before"
  )
})
