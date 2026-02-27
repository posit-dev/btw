test_that("btw_tool_files_replace replaces a unique string", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("hello world", "foo bar", "baz qux"), "test.txt")

  result <- btw_tool_files_replace("test.txt", "foo bar", "replaced")
  expect_btw_tool_result(result, has_data = FALSE)
  expect_equal(read_lines("test.txt"), c("hello world", "replaced", "baz qux"))
  expect_match(result@value, "Replaced 1 occurrence")
})

test_that("btw_tool_files_replace deletes text with empty new_string", {
  withr::local_dir(withr::local_tempdir())
  writeLines("hello world", "test.txt")

  btw_tool_files_replace("test.txt", " world", "")
  expect_equal(read_lines("test.txt"), "hello")
})

test_that("btw_tool_files_replace errors when old_string not found", {
  withr::local_dir(withr::local_tempdir())
  writeLines("hello world", "test.txt")

  expect_error(
    btw_tool_files_replace("test.txt", "not here", "replacement"),
    "not found"
  )
})

test_that("btw_tool_files_replace errors when old_string is not unique", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "aaa"), "test.txt")

  expect_error(
    btw_tool_files_replace("test.txt", "aaa", "ccc"),
    "appears 2 times"
  )
})

test_that("btw_tool_files_replace with replace_all replaces all occurrences", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "aaa"), "test.txt")

  result <- btw_tool_files_replace(
    "test.txt",
    "aaa",
    "ccc",
    replace_all = TRUE
  )
  expect_equal(read_lines("test.txt"), c("ccc", "bbb", "ccc"))
  expect_match(result@value, "Replaced 2 occurrences")
})

test_that("btw_tool_files_replace errors when old_string equals new_string", {
  withr::local_dir(withr::local_tempdir())
  writeLines("hello", "test.txt")

  expect_error(
    btw_tool_files_replace("test.txt", "hello", "hello"),
    "must be different"
  )
})

test_that("btw_tool_files_replace errors on empty old_string", {
  withr::local_dir(withr::local_tempdir())
  writeLines("hello", "test.txt")

  expect_error(
    btw_tool_files_replace("test.txt", "", "replacement"),
    "old_string"
  )
})

test_that("btw_tool_files_replace rejects paths outside working directory", {
  expect_error(
    btw_tool_files_replace("../evil.txt", "a", "b"),
    "not allowed"
  )
})

test_that("btw_tool_files_replace preserves indentation and whitespace", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("  indented line", "  another line"), "test.txt")

  btw_tool_files_replace("test.txt", "  indented line", "  replaced line")
  expect_equal(
    read_lines("test.txt"),
    c("  replaced line", "  another line")
  )
})

test_that("btw_tool_files_replace works with multi-line old_string", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  # Use actual line separator from the file (handles both LF and CRLF)
  content <- brio::read_file("test.txt")
  line_sep <- if (grepl("\r\n", content)) "\r\n" else "\n"
  btw_tool_files_replace(
    "test.txt",
    paste0("line1", line_sep, "line2"),
    "replaced"
  )
  expect_equal(read_lines("test.txt"), c("replaced", "line3"))
})

test_that("btw_tool_files_replace stores previous_content for diffs", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("hello", "world"), "test.txt")

  result <- btw_tool_files_replace("test.txt", "hello", "goodbye")
  expect_match(result@extra$previous_content, "^hello\r?\nworld")
  expect_match(result@extra$content, "^goodbye\r?\nworld")
  expect_equal(result@extra$path, "test.txt")
})

test_that("btw_tool_files_replace errors on nonexistent file", {
  withr::local_dir(withr::local_tempdir())

  expect_error(
    btw_tool_files_replace("nonexistent.txt", "a", "b"),
    "does not exist"
  )
})
