test_that("btw_tool_files_list() works", {
  withr::local_dir(withr::local_tempdir())

  writeLines("test", "test.csv")
  writeLines("test", "test.R")

  expect_btw_tool_result(btw_tool_files_list())

  expect_equal(
    btw_tool_files_list()@value,
    btw_this("./")
  )

  expect_no_match(
    btw_tool_files_list(regexp = "[.]R$")@value,
    "test\\.csv"
  )

  skip_if_not_snapshot_env()

  expect_snapshot(
    writeLines(btw_tool_files_list()@value),
    transform = function(x) {
      sub("\\d{4}-[0-9-]+ [0-9:]+", "MODIFIED TIME", x)
    }
  )

  expect_snapshot(
    btw_tool_files_list("/"),
    error = TRUE
  )

  expect_snapshot(
    btw_tool_files_list("../"),
    error = TRUE
  )
})

test_that("btw_tool_files_read() works", {
  withr::local_dir(withr::local_tempdir())

  write.csv(mtcars, "mtcars.csv", row.names = FALSE)
  saveRDS(mtcars, "mtcars.rds")

  expect_btw_tool_result(
    btw_tool_files_read("mtcars.csv"),
    has_data = FALSE
  )

  expect_equal(
    btw_tool_files_read("mtcars.csv")@extra$path,
    "mtcars.csv",
    ignore_attr = TRUE
  )

  expect_equal(
    btw_tool_files_read("mtcars.csv")@value,
    btw_this("./mtcars.csv")
  )

  expect_equal(
    btw_tool_files_read(
      "mtcars.csv",
      line_start = 1,
      line_end = 1
    )@value,
    '```csv\n"mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"\n```'
  )

  expect_equal(
    btw_tool_files_read(
      "mtcars.csv",
      line_start = 32,
      line_end = 35
    )@value,
    "```csv\n15,8,301,335,3.54,3.57,14.6,0,1,5,8\n21.4,4,121,109,4.11,2.78,18.6,1,1,4,2\n```"
  )

  skip_if_not_snapshot_env()

  expect_snapshot(
    btw_tool_files_read("mtcars.rds"),
    error = TRUE
  )

  expect_snapshot(
    btw_tool_files_read("../mtcars.rds"),
    error = TRUE
  )
})


test_that("is_common_ignorable_files identifies ignorable files by name", {
  expect_true(is_common_ignorable_files(".DS_Store"))
  expect_true(is_common_ignorable_files("Thumbs.db"))

  expect_false(is_common_ignorable_files("data.csv"))
  expect_false(is_common_ignorable_files("script.R"))

  expect_equal(
    is_common_ignorable_files(c(".DS_Store", "data.csv", "Thumbs.db")),
    c(TRUE, FALSE, TRUE)
  )
})

test_that("is_common_ignorable_files identifies files in ignorable directories", {
  # files in ignorable directories
  expect_true(is_common_ignorable_files(".git/config"))
  expect_true(is_common_ignorable_files("node_modules/react/index.js"))
  expect_true(is_common_ignorable_files(".svn/entries"))
  expect_true(is_common_ignorable_files(".hg/dirstate"))
  expect_true(is_common_ignorable_files(".venv/bin/python"))
  expect_true(is_common_ignorable_files("venv/bin/activate"))

  # nested paths with ignorable directories
  expect_true(is_common_ignorable_files("project/.git/HEAD"))
  expect_true(is_common_ignorable_files("src/node_modules/lodash/index.js"))
  expect_true(is_common_ignorable_files("path/to/.venv/lib/python3.9"))

  # renv/library special case
  expect_true(is_common_ignorable_files("renv/library/R-4.1/packages"))
  expect_true(is_common_ignorable_files("project/renv/library/something"))

  # regular directories
  expect_false(is_common_ignorable_files("src/components/Button.js"))
  expect_false(is_common_ignorable_files("data/processed/results.csv"))

  # The directory itself is not ignorable
  expect_false(is_common_ignorable_files("node_modules/"))
  expect_false(is_common_ignorable_files(".git/"))
  expect_false(is_common_ignorable_files("renv/library"))
})

test_that("is_common_ignorable_files handles edge cases correctly", {
  # empty string
  expect_false(is_common_ignorable_files(""))

  # both ignorable file name and directory
  expect_true(is_common_ignorable_files("node_modules/.DS_Store"))
  expect_true(is_common_ignorable_files(".git/Thumbs.db"))

  # similar but not exact matches
  expect_false(is_common_ignorable_files("my.DS_Store"))
  expect_false(is_common_ignorable_files("fake_node_modules/file.js"))
  expect_false(is_common_ignorable_files("renv/not_library/file.txt"))
})

test_that("is_common_ignorable_files works with different path formats", {
  # forward slashes
  expect_true(is_common_ignorable_files("path/to/node_modules/file.js"))

  # backslashes (Windows-style paths)
  expect_true(
    is_common_ignorable_files(
      # The paths are already normalized by fs::dir_info()
      fs::path_norm("C:\\path\\to\\node_modules\\file.js")
    )
  )

  # mixed slashes
  expect_true(is_common_ignorable_files("path/to\\node_modules/file.js"))
})

test_that("btw_tool_files_write() works", {
  withr::local_dir(withr::local_tempdir())

  res_write_data <- btw_tool_files_write("test.txt", "Hello\nWorld!")
  expect_btw_tool_result(res_write_data, has_data = FALSE)

  expect_equal(res_write_data@extra$path, "test.txt")
  expect_equal(res_write_data@extra$content, "Hello\nWorld!")
  expect_null(res_write_data@extra$previous_content, NULL)

  expect_equal(
    read_lines("test.txt"),
    c("Hello", "World!")
  )

  # Test overwriting
  res_write_data2 <- btw_tool_files_write("test.txt", "New content")
  expect_equal(
    read_file("test.txt"),
    "New content"
  )

  expect_equal(res_write_data2@extra$previous_content, "Hello\nWorld!")

  expect_snapshot(
    btw_tool_files_write("../test.txt", "content"),
    error = TRUE
  )
})
