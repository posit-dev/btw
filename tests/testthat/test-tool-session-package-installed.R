test_that("btw_tool_session_check_package_installed()", {
  local_mocked_bindings(
    is_installed = function(package_name) {
      package_name == "dplyr"
    },
    package_version = function(package_name) {
      "1.0.0"
    }
  )

  res_installed <- btw_tool_session_check_package_installed("dplyr")
  expect_btw_tool_result(res_installed, has_data = FALSE)
  expect_equal(res_installed@extra$package, "dplyr")
  expect_equal(res_installed@extra$version, "1.0.0")

  expect_error(
    btw_tool_session_check_package_installed("skibidi")
  )
  expect_error(
    btw_tool_session_check_package_installed("Dplyr"),
    "dplyr",
    ignore.case = FALSE
  )
})

test_that("package_version() returns a string", {
  expect_true(is_string(package_version("dplyr")))
})

test_that("find_package_candidates() aborts if it takes longer than 5s to complete", {
  skip_on_cran()

  local_mocked_bindings(
    dir = function(...) {
      Sys.sleep(7)
      c("dplyr", "ggplot2")
    },
    .package = "base"
  )

  start <- as.integer(Sys.time())
  expect_equal(
    find_package_candidates("skibidi"),
    character()
  )
  expect_lt(as.integer(Sys.time()) - start, 6)
})
