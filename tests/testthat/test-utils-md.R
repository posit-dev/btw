test_that("md_table()", {
  expect_snapshot(cat(md_table(mtcars[1:4, 1:4])))
})
