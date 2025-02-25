test_that("btw_this.function()", {
  expect_equal(btw_this('dplyr::mutate'), btw_this(dplyr::mutate))
})

test_that("btw() with package functions", {
  expect_output(
    expect_equal(btw(dplyr::mutate)[-1], btw('dplyr::mutate')[-1])
  )
})
