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
