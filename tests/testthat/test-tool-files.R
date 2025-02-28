test_that("btw_tool_list_files() works", {
  withr::local_dir(withr::local_tempdir())

  writeLines("test", "test.csv")
  writeLines("test", "test.R")

  expect_equal(
    btw_tool_list_files(),
    btw_this("./")
  )

  expect_no_match(
    btw_tool_list_files(regexp = "[.]R$"),
    "test\\.csv"
  )

  skip_if_not_macos()

  expect_snapshot(
    btw_tool_list_files(),
    transform = function(x) {
      sub("\\d{4}-[0-9-]+ [0-9:]+", "MODIFIED TIME", x)
    }
  )

  expect_snapshot(
    btw_tool_list_files("/"),
    error = TRUE
  )

  expect_snapshot(
    btw_tool_list_files("../"),
    error = TRUE
  )
})

test_that("btw_tool_read_text_file() works", {
  withr::local_dir(withr::local_tempdir())

  write.csv(mtcars, "mtcars.csv")
  saveRDS(mtcars, "mtcars.rds")

  expect_equal(
    btw_tool_read_text_file("mtcars.csv"),
    btw_this("./mtcars.csv")
  )

  skip_if_not_macos()

  expect_snapshot(
    btw_tool_read_text_file("mtcars.rds"),
    error = TRUE
  )

  expect_snapshot(
    btw_tool_read_text_file("../mtcars.rds"),
    error = TRUE
  )
})
