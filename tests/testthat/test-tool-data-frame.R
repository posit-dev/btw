skip_if_not_macos()

test_that("btw_this.data.frame() works", {
  expect_snapshot(cli::cat_line(btw_this(mtcars)))
  expect_snapshot(cli::cat_line(btw_this(mtcars, dims = c(Inf, Inf))))

  expect_snapshot(cli::cat_line(btw_this(mtcars, format = "glimpse")))
  expect_snapshot(
    cli::cat_line(btw_this(mtcars, format = "glimpse", dims = c(Inf, Inf)))
  )

  expect_snapshot(cli::cat_line(btw_this(mtcars, format = "print")))
  expect_snapshot(
    cli::cat_line(btw_this(mtcars, format = "print", dims = c(Inf, Inf)))
  )

  expect_snapshot(cli::cat_line(btw_this(mtcars, format = "json")))
  expect_snapshot(
    cli::cat_line(btw_this(mtcars, format = "json", dims = c(Inf, Inf)))
  )
})
