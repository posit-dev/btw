test_that("btw() formats output as expected", {
  skip_if_not_snapshot_env()

  expect_snapshot(cli::cat_line(btw(mtcars)))
})

test_that("btw() preserves order", {
  one <- "apple"
  two <- "banana"
  expect_snapshot(cli::cat_line(btw(two, one)))
})

test_that("btw() can include prompt strings", {
  expect_equal(
    format(btw("first thing", "second thing")),
    "## User\nfirst thing\nsecond thing"
  )
})

test_that("btw() works with vars that return characters of the same name", {
  beep <- "beep"
  expect_equal(
    format(btw(beep)),
    "## Context\n\n```r\nbeep\n#> [1] \"beep\"\n```"
  )
})

test_that("btw() allows injection", {
  expect_equal(
    format(btw(!!sprintf("{%s}", "tibble"))),
    format(btw("{tibble}"))
  )

  x <- "{tibble}"
  expect_equal(
    format(btw({{ x }})),
    format(btw("{tibble}"))
  )
})
