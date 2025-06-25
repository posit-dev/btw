use_latest_pandoc()

test_that("btw_tool_docs_package_help_topics() works", {
  res <- btw_tool_docs_package_help_topics("stats")

  expect_btw_tool_result(res)
  expect_match(res@value, '"topic_id":"Normal"', fixed = TRUE, all = FALSE)
})

test_that("btw_tool_docs_help_page() works", {
  res <- btw_tool_docs_help_page(package_name = "stats", topic = "rnorm")

  expect_btw_tool_result(res, has_data = FALSE)
  expect_equal(res@extra$topic, "Normal")
  expect_equal(res@extra$package, "stats")
  expect_type(res@extra$help_text, "character")

  skip_if_not_macos()
  expect_snapshot(cli::cat_line(res@value))
})

test_that("btw_tool_docs_help_page() with unknown topic", {
  expect_snapshot(error = TRUE, {
    btw_tool_docs_help_page("unknown-topic", "ggplot2")
    btw_tool_docs_help_page("unknown-topic")
  })
})

test_that("btw_tool_docs_available_vignettes() works", {
  skip_on_cran()

  res <- btw_tool_docs_available_vignettes("dplyr")

  expect_btw_tool_result(res)

  expect_match(res@value, '"vignette":"dplyr"', fixed = TRUE, all = FALSE)
  expect_match(
    res@value,
    '"title":"Programming with dplyr"',
    fixed = TRUE,
    all = FALSE
  )

  expect_equal(
    btw_tool_docs_available_vignettes("dplyr")@value,
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
  expect_btw_tool_result(res_dplyr)
  expect_match(
    res_dplyr@value,
    "Introduction to dplyr",
    fixed = TRUE,
    all = FALSE
  )
  expect_equal(btw_this(vignette("dplyr", "dplyr")), res_dplyr@value)

  res_prog <- btw_tool_docs_vignette("dplyr", "programming")
  expect_btw_tool_result(res_prog)
  expect_match(res_prog@value, "Programming", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("programming", "dplyr")), res_prog@value)
})

test_that("btw_tool_docs_help_page() with multiple help topics", {
  skip_if_not_installed("dplyr")

  expect_snapshot(
    error = TRUE,
    btw_tool_docs_help_page("filter")
  )
})
