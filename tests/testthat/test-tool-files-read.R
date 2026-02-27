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

  # btw_this() returns clean code block (no hashlines)
  # read tool @value has hashlines, display.markdown has clean code block
  expect_equal(
    btw_tool_files_read("mtcars.csv")@extra$display$markdown,
    btw_this("./mtcars.csv")
  )

  expect_match(
    btw_tool_files_read(
      "mtcars.csv",
      line_start = 1,
      line_end = 1
    )@value,
    "^1:[a-f0-9]{3}\\|"
  )

  expect_match(
    btw_tool_files_read(
      "mtcars.csv",
      line_start = 32,
      line_end = 35
    )@value,
    "^32:[a-f0-9]{3}\\|"
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

# is_text_file() and CJK multi-byte UTF-8 boundary truncation
# https://github.com/posit-dev/btw/issues/170
describe("is_text_file()", {
  # Helper: create a file of n copies of a 3-byte CJK character (测 = 0xe6 0xb5 0x8b)
  # UTF-8 encodes CJK characters as 3-byte sequences:
  #   Byte 1 (lead):         0xE0-0xEF
  #   Byte 2 (continuation): 0x80-0xBF
  #   Byte 3 (continuation): 0x80-0xBF
  #
  # is_text_file() reads 8192 bytes. When a file is filled with 3-byte chars,
  # the buffer boundary can land at three positions within a character:
  #   - After byte 3: complete sequence (no truncation)
  #   - After byte 1: lead byte only (2 continuation bytes missing)
  #   - After byte 2: lead + 1 continuation (1 continuation byte missing)
  write_cjk_file <- function(path, n_chars) {
    raw_char <- charToRaw("\u6d4b") # 测
    stopifnot(length(raw_char) == 3L)
    bytes <- rep(raw_char, n_chars)
    writeBin(bytes, path)
  }

  it("accepts an empty file", {
    tmp <- withr::local_tempfile()
    file.create(tmp)
    expect_true(is_text_file(tmp))
  })

  it("rejects a binary file with NULL bytes", {
    tmp <- withr::local_tempfile()
    writeBin(as.raw(c(0x00, 0x01, 0x02, 0xff, 0x00)), tmp)
    expect_false(is_text_file(tmp))
  })

  it("accepts a small CJK file shorter than the 8192-byte buffer", {
    tmp <- withr::local_tempfile()
    # 100 chars * 3 bytes = 300 bytes, well under the 8192 buffer.
    # Still >30% high bytes, so the UTF-8 validation branch is entered.
    write_cjk_file(tmp, 100)
    expect_equal(file.size(tmp), 300)
    expect_true(is_text_file(tmp))
  })

  describe("with CJK text at the 8192-byte buffer boundary", {
    it("accepts when the buffer ends on a complete character", {
      tmp <- withr::local_tempfile()
      # 2730 chars * 3 bytes = 8190 bytes; buffer reads all 8190 bytes.
      # Last byte in buffer is byte 3 of a complete sequence.
      write_cjk_file(tmp, 2730)
      expect_equal(file.size(tmp), 8190)
      expect_true(is_text_file(tmp))
    })

    it("accepts when the buffer truncates after lead + 1 continuation byte", {
      tmp <- withr::local_tempfile()
      # 2731 chars * 3 bytes = 8193 bytes; buffer reads 8192 bytes.
      # Byte 8191 = lead byte of char 2731 (0xe6)
      # Byte 8192 = first continuation byte (0xb5) -- last byte read
      # Byte 8193 = second continuation byte (0x8b) -- NOT read
      write_cjk_file(tmp, 2731)
      expect_equal(file.size(tmp), 8193)
      expect_true(is_text_file(tmp))
    })

    it("accepts when the buffer truncates after a lone lead byte", {
      tmp <- withr::local_tempfile()
      # 2730 complete chars (8190 bytes) + 1 orphan lead byte = 8191 bytes.
      # Buffer reads all 8191 bytes. Last byte is an incomplete lead byte.
      raw_char <- charToRaw("\u6d4b")
      bytes <- c(rep(raw_char, 2730), raw_char[1])
      writeBin(bytes, tmp)
      expect_equal(file.size(tmp), 8191)
      expect_true(is_text_file(tmp))
    })
  })
})

test_that("btw_tool_files_read() works with UTF-8 files containing non-ASCII characters", {
  withr::local_dir(withr::local_tempdir())

  # Create a file with Cyrillic characters (UTF-8)
  writeLines(c("# Тест", "1 + 1"), "test.R", useBytes = FALSE)

  # Verify the file is valid UTF-8
  con <- file("test.R", "rb")
  bytes <- readBin(con, what = "raw", n = file.size("test.R"))
  close(con)
  text <- rawToChar(bytes)
  expect_true(validUTF8(text))

  # Should be able to read the file
  result <- btw_tool_files_read("test.R")
  expect_btw_tool_result(result, has_data = FALSE)
  expect_equal(result@extra$path, "test.R", ignore_attr = TRUE)

  # Check that the file content was read (the main point of this test is that
  # the file can be read at all - previously this errored on Windows)
  expect_match(result@value, "1 \\+ 1")
})

test_that("btw_tool_files_read() returns hashline-annotated output", {
  withr::local_dir(withr::local_tempdir())
  writeLines(c("hello", "world", "foo"), "test.txt")

  result <- btw_tool_files_read("test.txt")

  # Value has hashlines (model-facing)
  lines <- strsplit(result@value, "\n")[[1]]
  expect_length(lines, 3)
  expect_match(lines[1], "^1:[a-f0-9]{3}\\|hello$")
  expect_match(lines[2], "^2:[a-f0-9]{3}\\|world$")
  expect_match(lines[3], "^3:[a-f0-9]{3}\\|foo$")

  # Display markdown is clean code block (user-facing)
  expect_match(result@extra$display$markdown, "^```")
  expect_no_match(result@extra$display$markdown, "^1:[a-f0-9]{3}\\|")
})

test_that("btw_tool_files_read() hashlines use actual line numbers for ranges", {
  withr::local_dir(withr::local_tempdir())
  writeLines(paste0("line", 1:50), "test.txt")

  result <- btw_tool_files_read("test.txt", line_start = 10, line_end = 12)
  lines <- strsplit(result@value, "\n")[[1]]
  expect_match(lines[1], "^10:")
  expect_match(lines[2], "^11:")
  expect_match(lines[3], "^12:")
})
