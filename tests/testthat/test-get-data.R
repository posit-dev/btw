test_that("get_data_frame works", {
  expect_snapshot(get_data_frame(mtcars))
  expect_snapshot(get_data_frame(mtcars, dims = c(Inf, Inf)))

  expect_snapshot(get_data_frame(mtcars, format = "print"))
  expect_snapshot(get_data_frame(mtcars, format = "print", dims = c(Inf, Inf)))

  expect_snapshot(get_data_frame(mtcars, format = "json"))
  expect_snapshot(get_data_frame(mtcars, format = "json", dims = c(Inf, Inf)))
})
