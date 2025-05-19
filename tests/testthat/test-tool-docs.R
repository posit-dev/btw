use_latest_pandoc()

test_that("btw_tool_docs_package_help_topics() works", {
  res <- btw_tool_docs_package_help_topics("stats")

  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_type(res@value, "character")
  expect_s3_class(res@extra$data, "data.frame")
  expect_match(res@value, '"topic_id":"Normal"', fixed = TRUE, all = FALSE)
})

test_that("btw_tool_docs_help_page() works", {
  res <- btw_tool_docs_help_page(package_name = "stats", topic = "rnorm")

  expect_s3_class(res, "btw::BtwHelpPageToolResult")
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_equal(res@extra$topic, "Normal")
  expect_equal(res@extra$package, "stats")
  expect_type(res@extra$help_text, "character")

  skip_if_not_macos()
  expect_snapshot(cli::cat_line(res@value))
})

test_that("btw_tool_docs_available_vignettes() works", {
  skip_on_cran()

  res <- btw_tool_docs_available_vignettes("dplyr")

  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_type(res@value, "character")
  expect_s3_class(res@extra$data, "data.frame")

  expect_match(res@value, '"vignette":"dplyr"', fixed = TRUE, all = FALSE)
  expect_match(
    res@value,
    '"title":"Programming with dplyr"',
    fixed = TRUE,
    all = FALSE
  )

  expect_equal(
    btw_tool_docs_available_vignettes("dplyr"),
    btw_this(vignette(package = "dplyr"))
  )

  expect_true(
    jsonlite::validate(
      btw_tool_docs_available_vignettes("dplyr")@value
    )
  )
})

test_that("btw_tool_docs_vignette() works", {
  skip_on_cran()

  res_dplyr <- btw_tool_docs_vignette("dplyr")
  expect_s3_class(res_dplyr, "ellmer::ContentToolResult")
  expect_type(res_dplyr@value, "character")
  expect_s3_class(res_dplyr@extra$data, "data.frame")
  expect_match(
    res_dplyr@value,
    "Introduction to dplyr",
    fixed = TRUE,
    all = FALSE
  )
  expect_equal(btw_this(vignette("dplyr", "dplyr")), res_dplyr)

  res_prog <- btw_tool_docs_vignette("dplyr", "programming")
  expect_s3_class(res_prog, "ellmer::ContentToolResult")
  expect_type(res_prog@value, "character")
  expect_s3_class(res_prog@extra$data, "data.frame")
  expect_match(res_prog@value, "Programming", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("programming", "dplyr")), res_prog)
})

test_that("btw_tool_docs_help_page() with multiple help topics", {
  skip_if_not_installed("dplyr")

  expect_snapshot(
    error = TRUE,
    btw_tool_docs_help_page("filter")
  )
})
