# Helper: read a file via the tool and extract line refs
get_line_ref <- function(result, line_num) {
  lines <- strsplit(result@value, "\n")[[1]]
  target <- lines[grepl(paste0("^", line_num, ":"), lines)]
  sub("\\|.*$", "", target[1])
}

test_that("btw_tool_files_edit replace works", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref2, content = list("replaced"))
    )
  )

  expect_btw_tool_result(edit_result, has_data = FALSE)
  expect_equal(read_lines("test.txt"), c("line1", "replaced", "line3"))
})

test_that("btw_tool_files_edit delete works (replace with empty content)", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref2, content = list())
    )
  )

  expect_equal(read_lines("test.txt"), c("line1", "line3"))
})

test_that("btw_tool_files_edit insert_after works", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref1 <- get_line_ref(read_result, 1)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "insert_after", line = ref1, content = list("inserted"))
    )
  )

  expect_equal(read_lines("test.txt"), c("line1", "inserted", "line2", "line3"))
})

test_that("btw_tool_files_edit insert at top of file works", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2"), "test.txt")

  btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "insert_after", line = "0:000", content = list("header"))
    )
  )

  expect_equal(read_lines("test.txt"), c("header", "line1", "line2"))
})

test_that("btw_tool_files_edit replace_range works", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3", "line4", "line5"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)
  ref4 <- get_line_ref(read_result, 4)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "replace_range",
        line = paste0(ref2, ",", ref4),
        content = list("new2", "new3")
      )
    )
  )

  expect_equal(read_lines("test.txt"), c("line1", "new2", "new3", "line5"))
})

test_that("btw_tool_files_edit rejects stale hashes", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)

  # Modify file behind the tool's back
  writeLines(c("line1", "CHANGED", "line3"), "test.txt")

  expect_error(
    btw_tool_files_edit(
      "test.txt",
      list(
        list(action = "replace", line = ref2, content = list("new"))
      )
    ),
    "hash mismatch"
  )
})

test_that("btw_tool_files_edit rejects overlapping edits", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3", "line4", "line5"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)
  ref3 <- get_line_ref(read_result, 3)
  ref4 <- get_line_ref(read_result, 4)

  expect_error(
    btw_tool_files_edit(
      "test.txt",
      list(
        list(
          action = "replace_range",
          line = paste0(ref2, ",", ref4),
          content = list("new")
        ),
        list(action = "replace", line = ref3, content = list("also new"))
      )
    ),
    "overlapping"
  )
})

test_that("btw_tool_files_edit multiple edits applied correctly", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3", "line4", "line5"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref1 <- get_line_ref(read_result, 1)
  ref5 <- get_line_ref(read_result, 5)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref1, content = list("first")),
      list(action = "replace", line = ref5, content = list("last"))
    )
  )

  expect_equal(
    read_lines("test.txt"),
    c("first", "line2", "line3", "line4", "last")
  )
})

test_that("btw_tool_files_edit replace can expand one line to multiple", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "replace",
        line = ref2,
        content = list("new2a", "new2b", "new2c")
      )
    )
  )

  expect_equal(
    read_lines("test.txt"),
    c("line1", "new2a", "new2b", "new2c", "line3")
  )
})

test_that("btw_tool_files_edit works on empty content array for replace_range", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3", "line4", "line5"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)
  ref4 <- get_line_ref(read_result, 4)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "replace_range",
        line = paste0(ref2, ",", ref4),
        content = list()
      )
    )
  )

  expect_equal(read_lines("test.txt"), c("line1", "line5"))
})

test_that("btw_tool_files_edit rejects edits outside working directory", {
  expect_error(
    btw_tool_files_edit(
      "../evil.txt",
      list(
        list(action = "replace", line = "1:abc", content = list("hacked"))
      )
    ),
    "not allowed"
  )
})

test_that("btw_tool_files_edit replace works on last line of file", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2", "line3"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref3 <- get_line_ref(read_result, 3)

  btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref3, content = list("replaced"))
    )
  )

  expect_equal(read_lines("test.txt"), c("line1", "line2", "replaced"))
})

test_that("parse_line_ref rejects malformed references", {
  expect_error(parse_line_ref("abc"), "Invalid line reference")
  expect_error(parse_line_ref("1:ab:cd"), "Invalid line reference")
  expect_error(parse_line_ref(""), "Invalid line reference")
  expect_error(parse_line_ref(":abc"), "Invalid line number")
})

test_that("parse_edit_line_field rejects malformed line fields", {
  expect_error(parse_edit_line_field("1:ab,2:cd,3:ef"), "Invalid line field")
})

# --- Edit response format tests ---

test_that("edit response: 1:1 replace has hashlines, no shift hint", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "ccc", "ddd", "eee"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref3 <- get_line_ref(read_result, 3)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref3, content = list("CCC"))
    )
  )

  val <- edit_result@value

  # Header says same line count (no change)
  expect_match(val, "Applied 1 edit\\(s\\) to test\\.txt \\(5 lines\\)")

  # Contains hashlines for the edited region with context
  expect_match(val, "2:[a-f0-9]{3}\\|bbb") # context before
  expect_match(val, "3:[a-f0-9]{3}\\|CCC") # edited line
  expect_match(val, "4:[a-f0-9]{3}\\|ddd") # context after

  # No shift hint
  expect_no_match(val, "update line numbers")
})

test_that("edit response: multiple 1:1 replaces merged, no shift hint", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "ccc", "ddd", "eee"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref1 <- get_line_ref(read_result, 1)
  ref3 <- get_line_ref(read_result, 3)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref1, content = list("AAA")),
      list(action = "replace", line = ref3, content = list("CCC"))
    )
  )

  val <- edit_result@value

  expect_snapshot(writeLines(edit_result@value))

  # Header says same line count (no change)
  expect_match(val, "Applied 2 edit\\(s\\) to test\\.txt \\(5 lines\\)")

  # # Contains hashlines for the edited region with context
  expect_match(val, "1:[a-f0-9]{3}\\|AAA") # context before
  expect_match(val, "2:[a-f0-9]{3}\\|bbb") # context before
  expect_match(val, "3:[a-f0-9]{3}\\|CCC") # edited line
  expect_match(val, "4:[a-f0-9]{3}\\|ddd") # context after

  # No shift hint
  expect_no_match(val, "update line numbers")
})

test_that("edit response: insert adds shift hint", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "ccc", "ddd", "eee"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "insert_after",
        line = ref2,
        content = list("new1", "new2")
      )
    )
  )

  val <- edit_result@value
  expect_snapshot(writeLines(val))

  # Header shows line count change
  expect_match(val, "now 7 lines, previously 5")

  # Contains hashlines for inserted lines
  expect_match(val, "3:[a-f0-9]{3}\\|new1")
  expect_match(val, "4:[a-f0-9]{3}\\|new2")

  # Context: line before and after the insertion
  expect_match(val, "2:[a-f0-9]{3}\\|bbb") # context before
  expect_match(val, "5:[a-f0-9]{3}\\|ccc") # context after (was line 3)

  # Shift hint with +2 offset
  expect_match(val, "update line numbers by \\+2")
  # Concrete example: old line 3 → new line 5
  expect_match(val, "old line 3.*new line 5")
})

test_that("edit response: delete adds negative shift hint", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "ccc", "ddd", "eee"), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref2 <- get_line_ref(read_result, 2)
  ref3 <- get_line_ref(read_result, 3)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "replace_range",
        line = paste0(ref2, ",", ref3),
        content = list()
      )
    )
  )

  val <- edit_result@value
  expect_snapshot(writeLines(val))

  # Header shows line count change
  expect_match(val, "now 3 lines, previously 5")

  # Shift hint with -2 offset
  expect_match(val, "update line numbers by -2")
  # Concrete example: old line 4 → new line 2
  expect_match(val, "old line 4.*new line 2")
})

test_that("edit response: nearby edits merge into single region", {
  withr::local_dir(withr::local_tempdir())
  writeLines(paste0("line", sprintf("%02d", 1:30)), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref5 <- get_line_ref(read_result, 5)
  ref10 <- get_line_ref(read_result, 10)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(action = "replace", line = ref5, content = list("FIVE")),
      list(action = "replace", line = ref10, content = list("TEN"))
    )
  )

  val <- edit_result@value
  expect_snapshot(writeLines(val))

  # Both edits are within 10 lines, should merge into one region
  # showing continuous hashlines from context-before-first to context-after-last
  expect_match(val, "4:[a-f0-9]{3}\\|line04") # context before edit 1
  expect_match(val, "5:[a-f0-9]{3}\\|FIVE") # edit 1
  expect_match(val, "10:[a-f0-9]{3}\\|TEN") # edit 2
  expect_match(val, "11:[a-f0-9]{3}\\|line11") # context after edit 2

  # No "between here and the next" hint (single merged region)
  expect_no_match(val, "between here and the next")
})

test_that("edit response: distant edits produce multiple regions", {
  withr::local_dir(withr::local_tempdir())
  writeLines(paste0("line", sprintf("%02d", 1:50)), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref3 <- get_line_ref(read_result, 3)
  ref40 <- get_line_ref(read_result, 40)

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "replace",
        line = ref3,
        content = list("THREE-A", "THREE-B")
      ),
      list(action = "replace", line = ref40, content = list("FORTY"))
    )
  )

  val <- edit_result@value
  expect_snapshot(writeLines(val))

  # Header shows line count change (+1 from first edit)
  expect_match(val, "now 51 lines, previously 50")

  # First region
  expect_match(val, "3:[a-f0-9]{3}\\|THREE-A")
  expect_match(val, "4:[a-f0-9]{3}\\|THREE-B")

  # Second region (line 40 shifted to 41 due to +1 from first edit)
  expect_match(val, "41:[a-f0-9]{3}\\|FORTY")

  # Between-region hint
  expect_match(val, "between here and the next edit region")
})

test_that("edit response: distant edits with cumulative deltas", {
  withr::local_dir(withr::local_tempdir())
  writeLines(paste0("line", sprintf("%02d", 1:50)), "test.txt")

  read_result <- btw_tool_files_read("test.txt")
  ref3 <- get_line_ref(read_result, 3)
  ref40 <- get_line_ref(read_result, 40)

  # Edit 1: replace line 3 with 2 lines (delta +1, cumulative +1)
  # Edit 2: replace line 40 with 3 lines (delta +2, cumulative +3)
  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "replace",
        line = ref3,
        content = list("THREE-A", "THREE-B")
      ),
      list(
        action = "replace",
        line = ref40,
        content = list("FORTY-A", "FORTY-B", "FORTY-C")
      )
    )
  )

  val <- edit_result@value
  expect_snapshot(writeLines(val))

  # Total: +1 from edit 1, +2 from edit 2 = +3
  expect_match(val, "now 53 lines, previously 50")

  # Between-region hint should show cumulative delta from edit 1 only (+1)
  # old line 4 → new line 5
  expect_match(val, "between here and the next edit region")
  expect_match(val, "old line 4.*new line 5")

  # Below-region hint should show full cumulative delta (+3)
  # old line 41 → new line 44
  expect_match(val, "old line 41.*new line 44")
})

test_that("edit response: insert at top of file", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "ccc"), "test.txt")

  edit_result <- btw_tool_files_edit(
    "test.txt",
    list(
      list(
        action = "insert_after",
        line = "0:000",
        content = list("header1", "header2")
      )
    )
  )

  val <- edit_result@value
  expect_snapshot(writeLines(val))

  # Header shows line count change
  expect_match(val, "now 5 lines, previously 3")

  # Inserted lines
  expect_match(val, "1:[a-f0-9]{3}\\|header1")
  expect_match(val, "2:[a-f0-9]{3}\\|header2")

  # Context after insertion (old line 1 is now line 3)
  expect_match(val, "3:[a-f0-9]{3}\\|aaa")

  # Shift hint
  expect_match(val, "update line numbers by \\+2")
  expect_match(val, "old line 1.*new line 3")
})
