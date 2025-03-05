test_that("btw works", {
  skip_if_not_macos()

  expect_snapshot(cli::cat_line(btw(mtcars)))
})

test_that("btw() preserves order", {
  one <- "apple"
  two <- "banana"
  expect_snapshot(cli::cat_line(btw(two, one)))
})

test_that("btw() with prompt stings", {
  expect_equal(
    format(btw("first thing", "second thing")),
    "## User\nfirst thing\nsecond thing"
  )
})
