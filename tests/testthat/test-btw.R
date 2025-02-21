test_that("btw works", {
  # have to `print()` as the result is returned invisibly
  expect_snapshot(print(btw(mtcars)))
})
