use_latest_pandoc()

test_that("btw_tool_docs_package_help_topics() works", {
  res <- btw_tool_docs_package_help_topics("stats")

  expect_type(res, "character")
  expect_match(res, '"topic_id":"Normal"', fixed = TRUE, all = FALSE)
})

test_that("btw_tool_docs_help_page() works", {
  skip_if_not_macos()

  res <- btw_tool_docs_help_page(package_name = "stats", topic = "rnorm")

  expect_snapshot(res)
})

test_that("btw_tool_docs_available_vignettes() works", {
  skip_on_cran()

  res <- btw_tool_docs_available_vignettes("dplyr")

  expect_type(res, "character")
  expect_match(res, '"vignette":"dplyr"', fixed = TRUE, all = FALSE)
  expect_match(
    res,
    '"title":"Programming with dplyr"',
    fixed = TRUE,
    all = FALSE
  )

  expect_equal(
    btw_tool_docs_available_vignettes("dplyr"),
    btw_this(vignette(package = "dplyr"))
  )

  expect_true(
    jsonlite::validate(btw_tool_docs_available_vignettes(
      "dplyr"
    ))
  )
})

test_that("btw_tool_docs_vignette() works", {
  skip_on_cran()

  res <- btw_tool_docs_vignette("dplyr")

  expect_type(res, "character")
  expect_match(res, "Introduction to dplyr", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("dplyr", "dplyr")), res)

  res <- btw_tool_docs_vignette("dplyr", "programming")

  expect_type(res, "character")
  expect_match(res, "Programming", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("programming", "dplyr")), res)
})
