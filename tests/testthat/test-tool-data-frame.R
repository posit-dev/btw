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

test_that("btw_this.tbl() works", {
  tbl_data <- tibble::tibble(x = 1:5, y = letters[1:5])

  expect_snapshot(cli::cat_line(btw_this(tbl_data)))
  expect_snapshot(cli::cat_line(btw_this(tbl_data, format = "glimpse")))
  expect_snapshot(cli::cat_line(btw_this(tbl_data, format = "print")))
  expect_snapshot(cli::cat_line(btw_this(tbl_data, format = "json")))
})

test_that("btw_tool_env_describe_data_frame() errors when data frame not found", {
  expect_snapshot(
    error = TRUE,
    btw_tool_env_describe_data_frame("nonexistent_dataframe")
  )
})

test_that("btw_this() returns markdown table for small data frames", {
  small_df <- data.frame(a = 1:3, b = c("x", "y", "z"))

  result <- btw_this(small_df, format = "skim")
  expect_true(grepl("\\|", result[1])) # Check for markdown table pipes
})

test_that("get_dataset_from_package returns missing_arg when package is NULL", {
  result <- btw:::get_dataset_from_package("some_data", package = NULL)
  expect_true(rlang::is_missing(result))
})

test_that("describe_data_frame_skim handles character columns with few unique values", {
  df_few_unique <- data.frame(
    x = 1:10,
    category = rep(c("A", "B", "C"), length.out = 10)
  )

  result <- btw_this(df_few_unique)
  expect_snapshot(cli::cat_line(result))
})

test_that("describe_data_frame_skim truncates long character values", {
  # df has to be long enough to make it past the small data frame check
  df_long_chars <- data.frame(
    id = 8 * 4 + 2,
    text = c(
      paste(rep("a", 150), collapse = ""),
      paste(rep("b", 200), collapse = ""),
      rep(letters[1:8], 4)
    )
  )

  result <- btw_this(df_long_chars)
  expect_snapshot(cli::cat_line(result))
})

test_that("btw_this() handles factor columns", {
  df_with_factor <- data.frame(
    x = 1:5,
    category = factor(c("low", "medium", "high", "medium", "low"))
  )

  result <- btw_this(df_with_factor)
  expect_snapshot(cli::cat_line(result))
})
