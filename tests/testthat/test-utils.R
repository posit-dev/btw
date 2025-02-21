test_that("check_installed() works", {
  # returns NULL invisibly if installed
  testthat::local_mocked_bindings(is_installed = function(x) TRUE)
  expect_invisible(check_installed("somepackage"))
  expect_null(check_installed("somepackage"))

  # informative error if not installed
  testthat::local_mocked_bindings(is_installed = function(x) FALSE)
  expect_snapshot(check_installed("somepackage"), error = TRUE)
})
