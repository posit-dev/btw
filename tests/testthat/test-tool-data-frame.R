skip_if_not_macos()

test_that("btw_this.data.frame() works", {
  expect_snapshot(cli::cat_line(btw_this(mtcars)))
  expect_snapshot(cli::cat_line(btw_this(
    mtcars,
    max_rows = Inf,
    max_cols = Inf
  )))

  expect_snapshot(cli::cat_line(btw_this(mtcars, format = "glimpse")))
  expect_snapshot(
    cli::cat_line(btw_this(
      mtcars,
      format = "glimpse",
      max_rows = Inf,
      max_cols = Inf
    ))
  )

  expect_snapshot(cli::cat_line(btw_this(mtcars, format = "print")))
  expect_snapshot(
    cli::cat_line(btw_this(
      mtcars,
      format = "print",
      max_rows = Inf,
      max_cols = Inf
    ))
  )

  expect_snapshot(cli::cat_line(btw_this(mtcars, format = "json")))
  expect_snapshot(
    cli::cat_line(btw_this(
      mtcars,
      format = "json",
      max_rows = Inf,
      max_cols = Inf
    ))
  )
})

test_that("btw_tool_env_describe_data_frame() handles namespaced datasets", {
  expect_equal(
    btw_tool_env_describe_data_frame("dplyr::storms"),
    btw_tool_env_describe_data_frame("storms", package = "dplyr")
  )

  expect_equal(
    btw_this.data.frame("dplyr::storms"),
    btw_this.data.frame("storms", package = "dplyr")
  )
})

test_that("btw_tool_env_describe_data_frame() checks that the package is installed", {
  local_mocked_bindings(
    find_package_candidates = function(...) {
      c("Kifidi", "simIDM", "bib2df", "BiBitR", "Bioi")
    }
  )

  expect_snapshot(
    error = TRUE,
    btw_tool_env_describe_data_frame("skibidi::ohio"),
  )
})
