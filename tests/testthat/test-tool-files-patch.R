# ============================================================
# parse_patch — Parser tests
# ============================================================

test_that("parse_patch: valid envelope with Add, Update, Delete ops", {
  patch <- paste(
    "*** Begin Patch",
    "*** Add File: new.txt",
    "+hello",
    "+world",
    "*** Update File: existing.txt",
    "@@",
    " context",
    "-old line",
    "+new line",
    "*** Delete File: gone.txt",
    "*** End Patch",
    "",
    sep = "\n"
  )

  ops <- parse_patch(patch)
  expect_length(ops, 3L)

  expect_equal(ops[[1]]$op, "add")
  expect_equal(ops[[1]]$path, "new.txt")
  expect_equal(ops[[1]]$lines, c("hello", "world"))

  expect_equal(ops[[2]]$op, "update")
  expect_equal(ops[[2]]$path, "existing.txt")
  expect_null(ops[[2]]$move_to)
  expect_length(ops[[2]]$hunks, 1L)

  expect_equal(ops[[3]]$op, "delete")
  expect_equal(ops[[3]]$path, "gone.txt")
})

test_that("parse_patch: missing '*** Begin Patch' errors", {
  patch <- paste(
    "*** Update File: foo.txt",
    "@@",
    "+line",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "Begin Patch")
})

test_that("parse_patch: missing '*** End Patch' errors", {
  patch <- paste(
    "*** Begin Patch",
    "*** Add File: new.txt",
    "+line",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "End Patch")
})

test_that("parse_patch: unknown header inside envelope errors", {
  patch <- paste(
    "*** Begin Patch",
    "*** Unknown Header: foo",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "unknown patch header")
})

test_that("parse_patch: Add File body with non-'+' line errors", {
  patch <- paste(
    "*** Begin Patch",
    "*** Add File: new.txt",
    "+good line",
    "bad line",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "must start with '\\+'")
})

test_that("parse_patch: content lines outside any operation block errors", {
  patch <- paste(
    "*** Begin Patch",
    "some stray content",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "content outside any operation block")
})

test_that("parse_patch: '*** Move to:' outside Update File block errors", {
  patch <- paste(
    "*** Begin Patch",
    "*** Add File: new.txt",
    "+line",
    "*** Move to: other.txt",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "Move to.*only valid inside an Update File block")
})

test_that("parse_patch: Move to is captured inside Update File block", {
  patch <- paste(
    "*** Begin Patch",
    "*** Update File: old.txt",
    "*** Move to: new.txt",
    "@@",
    " context",
    "-old",
    "+new",
    "*** End Patch",
    "",
    sep = "\n"
  )

  ops <- parse_patch(patch)
  expect_length(ops, 1L)
  expect_equal(ops[[1]]$op, "update")
  expect_equal(ops[[1]]$path, "old.txt")
  expect_equal(ops[[1]]$move_to, "new.txt")
})

test_that("parse_patch: hunk line outside @@ boundary errors", {
  patch <- paste(
    "*** Begin Patch",
    "*** Update File: foo.txt",
    " context line without @@",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(parse_patch(patch), "hunk line outside of '@@' boundary")
})

# ============================================================
# validate_patch_ops — Filesystem validator tests
# ============================================================

test_that("validate_patch_ops: absolute path errors", {
  ops <- list(list(op = "add", path = "/absolute/path.txt", lines = character()))
  expect_error(validate_patch_ops(ops), "not allowed")
})

test_that("validate_patch_ops: '..' traversal errors", {
  ops <- list(list(op = "add", path = "../evil.txt", lines = character()))
  expect_error(validate_patch_ops(ops), "not allowed")
})

test_that("validate_patch_ops: empty path errors", {
  ops <- list(list(op = "add", path = "", lines = character()))
  expect_error(validate_patch_ops(ops), "path must not be empty")
})

test_that("validate_patch_ops: add over existing file errors", {
  withr::local_dir(withr::local_tempdir())
  writeLines("existing", "exists.txt")

  ops <- list(list(op = "add", path = "exists.txt", lines = c("new")))
  expect_error(validate_patch_ops(ops), "already exists")
})

test_that("validate_patch_ops: update missing file errors", {
  withr::local_dir(withr::local_tempdir())

  ops <- list(list(
    op = "update",
    path = "missing.txt",
    move_to = NULL,
    hunks = list(list(ops = list(list(type = "context", line = "x"))))
  ))
  expect_error(validate_patch_ops(ops), "does not exist")
})

test_that("validate_patch_ops: update with no hunks errors", {
  withr::local_dir(withr::local_tempdir())
  writeLines("content", "file.txt")

  ops <- list(list(op = "update", path = "file.txt", move_to = NULL, hunks = list()))
  expect_error(validate_patch_ops(ops), "no hunks")
})

test_that("validate_patch_ops: delete missing file errors", {
  withr::local_dir(withr::local_tempdir())

  ops <- list(list(op = "delete", path = "missing.txt"))
  expect_error(validate_patch_ops(ops), "does not exist")
})

test_that("validate_patch_ops: move to existing destination errors", {
  withr::local_dir(withr::local_tempdir())
  writeLines("source", "source.txt")
  writeLines("destination", "dest.txt")

  ops <- list(list(
    op = "update",
    path = "source.txt",
    move_to = "dest.txt",
    hunks = list(list(ops = list(list(type = "context", line = "source"))))
  ))
  expect_error(validate_patch_ops(ops), "already exists")
})

# ============================================================
# match_hunk — Hunk matcher tests
# ============================================================

test_that("match_hunk: exact match finds correct range", {
  hunk <- list(ops = list(
    list(type = "context", line = "line2"),
    list(type = "delete", line = "line3")
  ))
  file_lines <- c("line1", "line2", "line3", "line4")

  result <- match_hunk(hunk, file_lines, 1L, "test.txt")
  expect_equal(result$start, 2L)
  expect_equal(result$end, 3L)
})

test_that("match_hunk: hunk context not found errors", {
  hunk <- list(ops = list(
    list(type = "context", line = "not present")
  ))
  file_lines <- c("line1", "line2", "line3")

  expect_error(
    match_hunk(hunk, file_lines, 1L, "test.txt"),
    "Hunk context not found"
  )
})

test_that("parse_patch rejects pure-insert hunks (no context or delete)", {
  patch <- paste(
    "*** Begin Patch",
    "*** Update File: x.txt",
    "@@",
    "+only an insert",
    "*** End Patch",
    "",
    sep = "\n"
  )
  expect_error(parse_patch(patch), "no context or delete lines")
})

test_that("parse_patch rejects pure-insert hunks even when other hunks are valid", {
  patch <- paste(
    "*** Begin Patch",
    "*** Update File: x.txt",
    "@@",
    " a",
    "-b",
    "+B",
    "@@",
    "+trailing only",
    "*** End Patch",
    "",
    sep = "\n"
  )
  expect_error(parse_patch(patch), "no context or delete lines")
})

# ============================================================
# apply_patch_ops — Applier tests
# ============================================================

test_that("apply_patch_ops: add file creates file with correct content", {
  withr::local_dir(withr::local_tempdir())

  ops <- list(list(op = "add", path = "new.txt", lines = c("hello", "world")))
  apply_patch_ops(ops)

  expect_true(fs::file_exists("new.txt"))
  expect_equal(read_lines("new.txt"), c("hello", "world"))
})

test_that("apply_patch_ops: add file creates parent directories", {
  withr::local_dir(withr::local_tempdir())

  ops <- list(list(op = "add", path = "subdir/new.txt", lines = c("hello")))
  apply_patch_ops(ops)

  expect_true(fs::file_exists("subdir/new.txt"))
  expect_equal(read_lines("subdir/new.txt"), "hello")
})

test_that("apply_patch_ops: update file applies single hunk", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "old line", "line3"), "test.txt")

  ops <- list(list(
    op = "update",
    path = "test.txt",
    move_to = NULL,
    hunks = list(list(ops = list(
      list(type = "context", line = "line1"),
      list(type = "delete", line = "old line"),
      list(type = "insert", line = "new line")
    )))
  ))
  apply_patch_ops(ops)

  expect_equal(read_lines("test.txt"), c("line1", "new line", "line3"))
})

test_that("apply_patch_ops: update file applies multiple hunks", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("aaa", "bbb", "ccc", "ddd", "eee"), "test.txt")

  ops <- list(list(
    op = "update",
    path = "test.txt",
    move_to = NULL,
    hunks = list(
      list(ops = list(
        list(type = "delete", line = "aaa"),
        list(type = "insert", line = "AAA")
      )),
      list(ops = list(
        list(type = "delete", line = "eee"),
        list(type = "insert", line = "EEE")
      ))
    )
  ))
  apply_patch_ops(ops)

  expect_equal(read_lines("test.txt"), c("AAA", "bbb", "ccc", "ddd", "EEE"))
})

test_that("apply_patch_ops: delete file removes the file", {
  withr::local_dir(withr::local_tempdir())
  writeLines("content", "todelete.txt")

  ops <- list(list(op = "delete", path = "todelete.txt"))
  apply_patch_ops(ops)

  expect_false(fs::file_exists("todelete.txt"))
})

test_that("apply_patch_ops: move + update renames file and updates content", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("old content", "second line"), "old.txt")

  ops <- list(list(
    op = "update",
    path = "old.txt",
    move_to = "new.txt",
    hunks = list(list(ops = list(
      list(type = "delete", line = "old content"),
      list(type = "insert", line = "new content")
    )))
  ))
  apply_patch_ops(ops)

  expect_false(fs::file_exists("old.txt"))
  expect_true(fs::file_exists("new.txt"))
  expect_equal(read_lines("new.txt"), c("new content", "second line"))
})

test_that("apply_patch_ops: atomic failure — no filesystem changes when one op fails", {
  withr::local_dir(withr::local_tempdir())

  # good op: add a new file
  # bad op: update a missing file (will fail during dry-run)
  patch_str <- paste(
    "*** Begin Patch",
    "*** Add File: should_not_exist.txt",
    "+created",
    "*** Update File: does_not_exist.txt",
    "@@",
    "-missing line",
    "+replacement",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(btw_tool_files_patch_impl(patch_str))
  expect_false(fs::file_exists("should_not_exist.txt"))
})

# ============================================================
# btw_tool_files_patch_impl — Tool integration (snapshot) tests
# ============================================================

test_that("btw_tool_files_patch_impl: success output for mixed add/update/delete", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("context", "old line"), "update_me.txt")
  writeLines("to remove", "delete_me.txt")

  patch_str <- paste(
    "*** Begin Patch",
    "*** Add File: added.txt",
    "+new content",
    "*** Update File: update_me.txt",
    "@@",
    " context",
    "-old line",
    "+new line",
    "*** Delete File: delete_me.txt",
    "*** End Patch",
    "",
    sep = "\n"
  )

  result <- btw_tool_files_patch_impl(patch_str)
  expect_btw_tool_result(result, has_data = FALSE)
  expect_snapshot(cat(result@value))
})

test_that("btw_tool_files_patch_impl: failure output when hunk context not found", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("line1", "line2"), "target.txt")

  patch_str <- paste(
    "*** Begin Patch",
    "*** Update File: target.txt",
    "@@",
    "-nonexistent line",
    "+replacement",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_snapshot(
    btw_tool_files_patch_impl(patch_str),
    error = TRUE
  )
})

test_that("btw_tool_files_patch_impl: success output for single add", {
  withr::local_dir(withr::local_tempdir())

  patch_str <- paste(
    "*** Begin Patch",
    "*** Add File: hello.txt",
    "+Hello, world!",
    "*** End Patch",
    "",
    sep = "\n"
  )

  result <- btw_tool_files_patch_impl(patch_str)
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Applied patch: 1 operation\\.")
  expect_match(result@value, "Added: hello\\.txt")
  expect_equal(read_lines("hello.txt"), "Hello, world!")
})

test_that("btw_tool_files_patch_impl: success output for move + update", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("const oldName = 1"), "old.ts")

  patch_str <- paste(
    "*** Begin Patch",
    "*** Update File: old.ts",
    "*** Move to: new.ts",
    "@@",
    "-const oldName = 1",
    "+const newName = 1",
    "*** End Patch",
    "",
    sep = "\n"
  )

  result <- btw_tool_files_patch_impl(patch_str)
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Moved: old\\.ts")
  expect_match(result@value, "new\\.ts")
  expect_false(fs::file_exists("old.ts"))
  expect_equal(read_lines("new.ts"), "const newName = 1")
})

test_that("btw_tool_files_patch_impl: multi-file patch applies all ops", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("file1 line1", "file1 line2"), "file1.txt")
  writeLines(c("file2 line1", "file2 line2"), "file2.txt")

  patch_str <- paste(
    "*** Begin Patch",
    "*** Update File: file1.txt",
    "@@",
    "-file1 line1",
    "+file1 updated",
    "*** Update File: file2.txt",
    "@@",
    "-file2 line2",
    "+file2 updated",
    "*** End Patch",
    "",
    sep = "\n"
  )

  result <- btw_tool_files_patch_impl(patch_str)
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Applied patch: 2 operations\\.")
  expect_equal(read_lines("file1.txt"), c("file1 updated", "file1 line2"))
  expect_equal(read_lines("file2.txt"), c("file2 line1", "file2 updated"))
})

test_that("btw_tool_files_patch_impl: rejects paths outside working directory", {
  patch_str <- paste(
    "*** Begin Patch",
    "*** Add File: ../evil.txt",
    "+hacked",
    "*** End Patch",
    "",
    sep = "\n"
  )

  expect_error(btw_tool_files_patch_impl(patch_str), "not allowed")
})
