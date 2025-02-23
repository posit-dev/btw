test_that("get_data_frame works", {
  expect_snapshot(cat(get_data_frame(mtcars)))
  expect_snapshot(cat(get_data_frame(mtcars, dims = c(Inf, Inf))))

  expect_snapshot(cat(get_data_frame(mtcars, format = "glimpse")))
  expect_snapshot(
    cat(get_data_frame(mtcars, format = "glimpse", dims = c(Inf, Inf)))
  )

  expect_snapshot(cat(get_data_frame(mtcars, format = "print")))
  expect_snapshot(
    cat(get_data_frame(mtcars, format = "print", dims = c(Inf, Inf)))
  )

  expect_snapshot(cat(get_data_frame(mtcars, format = "json")))
  expect_snapshot(
    cat(get_data_frame(mtcars, format = "json", dims = c(Inf, Inf)))
  )
})
