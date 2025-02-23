test_that("get_installed_package() works", {
  res <- get_installed_packages()
  
  expect_type(res, "character")
  expect_match(res, '"Package": "utils"', fixed = TRUE, all = FALSE)
})

test_that("get_package_help() works", {
  res <- get_package_help("stats")

  expect_type(res, "character")
  expect_match(res, '"topic_id": "Normal"', fixed = TRUE, all = FALSE)
})

test_that("get_help_page() works", {
  skip_if_not_macos()
  
  res <- get_help_page("stats", "rnorm")
  
  expect_snapshot(res)
})

test_that("get_package_vignettes() works", {
  skip_on_cran()

  res <- get_package_vignettes("dplyr")

  expect_type(res, "character")
  expect_match(res, '"Title": "Programming with dplyr"', fixed = TRUE, all = FALSE)
})

test_that("get_package_vignette() works", {
  skip_on_cran()

  res <- get_package_vignette("dplyr")

  expect_type(res, "character")
  expect_match(res, "Introduction to dplyr", fixed = TRUE, all = FALSE)

  res <- get_package_vignette("dplyr", "programming")

  expect_type(res, "character")
  expect_match(res, "Programming", fixed = TRUE, all = FALSE)
})
