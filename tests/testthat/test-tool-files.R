test_that("btw_tool_files_list_files() works", {
  withr::local_dir(withr::local_tempdir())

  writeLines("test", "test.csv")
  writeLines("test", "test.R")

  expect_btw_tool_result(btw_tool_files_list_files())

  expect_equal(
    btw_tool_files_list_files()@value,
    btw_this("./")
  )

  expect_no_match(
    btw_tool_files_list_files(regexp = "[.]R$")@value,
    "test\\.csv"
  )

  skip_if_not_macos()

  expect_snapshot(
    writeLines(btw_tool_files_list_files()@value),
    transform = function(x) {
      sub("\\d{4}-[0-9-]+ [0-9:]+", "MODIFIED TIME", x)
    }
  )

  expect_snapshot(
    btw_tool_files_list_files("/"),
    error = TRUE
  )

  expect_snapshot(
    btw_tool_files_list_files("../"),
    error = TRUE
  )
})

test_that("btw_tool_files_read_text_file() works", {
  withr::local_dir(withr::local_tempdir())

  write.csv(mtcars, "mtcars.csv")
  saveRDS(mtcars, "mtcars.rds")

  expect_btw_tool_result(
    btw_tool_files_read_text_file("mtcars.csv"),
    has_data = FALSE
  )

  expect_equal(
    btw_tool_files_read_text_file("mtcars.csv")@extra$path,
    "mtcars.csv",
    ignore_attr = TRUE
  )

  expect_equal(
    btw_tool_files_read_text_file("mtcars.csv")@value,
    btw_this("./mtcars.csv")
  )

  skip_if_not_macos()

  expect_snapshot(
    btw_tool_files_read_text_file("mtcars.rds"),
    error = TRUE
  )

  expect_snapshot(
    btw_tool_files_read_text_file("../mtcars.rds"),
    error = TRUE
  )
})
