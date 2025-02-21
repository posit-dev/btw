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
  res <- get_help_page("stats", "rnorm")
  
  expect_snapshot(res)
})
