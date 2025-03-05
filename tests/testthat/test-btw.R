test_that("btw works", {
  skip_if_not_macos()

  expect_snapshot(writeLines(btw(mtcars)))
})

test_that("btw() preserves order", {
  one <- "apple"
  two <- "banana"
  expect_snapshot(writeLines(btw(two, one)))
})

test_that("btw() with prompt stings", {
  expect_equal(
    btw("first thing", "second thing"),
    "## User\nfirst thing\nsecond thing"
  )
})
