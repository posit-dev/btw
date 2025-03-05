test_that("check_installed() works", {
  # returns NULL invisibly if installed
  testthat::local_mocked_bindings(is_installed = function(x) TRUE)
  expect_invisible(check_installed("somepackage"))
  expect_null(check_installed("somepackage"))

  skip_if_not_macos()

  # informative error if not installed
  testthat::local_mocked_bindings(is_installed = function(x) FALSE)
  expect_snapshot(check_installed("somepackage"), error = TRUE)
})

test_that("check_inherits() works", {
  expect_null(check_inherits("true", "character"))
  expect_null(check_inherits(as_btw_capture("foo"), "btw_captured"))
  expect_error(check_inherits(as_btw_docs_package("btw"), "not_this_class"))
})
