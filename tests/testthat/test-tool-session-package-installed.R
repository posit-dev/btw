test_that("btw_tool_sessioninfo_is_package_installed()", {
  local_mocked_bindings(
    is_installed = function(package_name) {
      package_name == "dplyr"
    },
    package_version = function(package_name) {
      "1.0.0"
    }
  )

  res_installed <- btw_tool_sessioninfo_is_package_installed("dplyr")
  expect_btw_tool_result(res_installed, has_data = FALSE)
  expect_equal(res_installed@extra$package, "dplyr")
  expect_equal(res_installed@extra$version, "1.0.0")

  expect_error(
    btw_tool_sessioninfo_is_package_installed("skibidi")
  )
  expect_error(
    btw_tool_sessioninfo_is_package_installed("Dplyr"),
    "dplyr",
    ignore.case = FALSE
  )
})

test_that("package_version() returns a string", {
  expect_true(is_string(package_version("dplyr")))
})
