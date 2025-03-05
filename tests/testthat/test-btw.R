test_that("btw works", {
  skip_if_not_macos()

  local_mocked_bindings(interactive = function() FALSE)
  expect_snapshot(cat(btw(mtcars)))
})

test_that("btw() preserves order", {
  one <- "apple"
  two <- "banana"
  expect_snapshot(btw(two, one))
})

test_that("btw() with prompt stings", {
  expect_equal(
    btw("first thing", "second thing"),
    "## User\nfirst thing\nsecond thing"
  )
})
