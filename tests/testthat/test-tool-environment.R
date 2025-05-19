test_that("btw_this.environment() works", {
  skip_if_not_macos()
  env <- new_environment(list(mtcars = mtcars, boop = "bop"))

  expect_snapshot(cli::cat_line(btw_this(env)))
  expect_snapshot(cli::cat_line(btw_this(env, items = "mtcars")))
  expect_snapshot(cli::cat_line(btw_this(env, items = "boop")))
  expect_snapshot(cli::cat_line(btw_this(env, items = character(0))))
})

test_that("btw_this.environment() does not support entire namespaces (yet)", {
  expect_error(btw_this(asNamespace("dplyr")))
})

test_that("btw_this.environment() correctly separates items", {
  expect_snapshot(
    cli::cat_line(
      btw(
        letters[1],
        "one thing",
        "two thing",
        letters[2],
        "red thing",
        letters[4],
        "blue thing",
        clipboard = FALSE
      )
    )
  )
})

test_that("btw_tool_env_describe_environment()", {
  env <- new_environment(list(mtcars = mtcars, boop = "bop"))

  res <- btw_tool_env_describe_environment(items = "mtcars", environment = env)
  expect_s3_class(res, "ellmer::ContentToolResult")
  expect_type(res@value, "character")
})
