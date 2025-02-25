test_that("btw_this.function()", {
  expect_equal(btw_this('dplyr::mutate'), btw_this(dplyr::mutate))
})

test_that("btw() with package functions", {
  expect_equal(btw(dplyr::mutate), btw('dplyr::mutate'))
})
