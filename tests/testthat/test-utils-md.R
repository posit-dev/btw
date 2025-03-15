test_that("md_table()", {
  expect_snapshot(cat(md_table(mtcars[1:4, 1:4])))
})

test_that("md_code_block()", {
  expect_equal(
    md_code_block("markdown", c("```r", "runif(1)", "```")),
    c("````markdown", "```r", "runif(1)", "```", "````")
  )

  expect_equal(
    md_code_block("markdown", c("````r", "runif(1)", "````")),
    c("`````markdown", "````r", "runif(1)", "````", "`````")
  )
})
