test_that("btw_tool_get_installed_packages() works", {
  res <- btw_tool_get_installed_packages()

  expect_type(res, "character")
  expect_match(res, '"Package": "utils"', fixed = TRUE, all = FALSE)
})

test_that("btw_tool_get_package_help_topics() works", {
  res <- btw_tool_get_package_help_topics("stats")

  expect_type(res, "character")
  expect_match(res, '"topic_id": "Normal"', fixed = TRUE, all = FALSE)
})

test_that("btw_tool_get_help_page() works", {
  skip_if_not_macos()

  res <- btw_tool_get_help_page("stats", "rnorm")

  expect_snapshot(res)
})

test_that("btw_tool_get_available_vignettes_in_package() works", {
  skip_on_cran()

  res <- btw_tool_get_available_vignettes_in_package("dplyr")

  expect_type(res, "character")
  expect_match(res, '"vignette":"dplyr"', fixed = TRUE, all = FALSE)
  expect_match(
    res,
    '"title":"Programming with dplyr"',
    fixed = TRUE,
    all = FALSE
  )

  expect_equal(
    btw_tool_get_available_vignettes_in_package("dplyr"),
    btw_this(vignette(package = "dplyr"))
  )

  expect_true(
    jsonlite::validate(btw_tool_get_available_vignettes_in_package("dplyr"))
  )
})

test_that("btw_tool_get_vignette_from_package() works", {
  skip_on_cran()

  res <- btw_tool_get_vignette_from_package("dplyr")

  expect_type(res, "character")
  expect_match(res, "Introduction to dplyr", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("dplyr", "dplyr")), res)

  res <- btw_tool_get_vignette_from_package("dplyr", "programming")

  expect_type(res, "character")
  expect_match(res, "Programming", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("programming", "dplyr")), res)
})
