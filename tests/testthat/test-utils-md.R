test_that("md_table()", {
  expect_snapshot(cat(md_table(mtcars[1:4, 1:4])))
})

test_that("md_code_block()", {
  expect_snapshot(md_code_block("markdown", c("```r", "runif(1)", "```")))

  expect_snapshot(md_code_block("markdown", c("````r", "runif(1)", "````")))
})
