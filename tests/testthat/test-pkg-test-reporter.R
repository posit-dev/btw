test_that("compact reporter tracks files and prints a final summary", {
  test_dir <- withr::local_tempdir()
  dir.create(file.path(test_dir, "tests", "testthat"), recursive = TRUE)
  scripts <- file.path(test_dir, "tests", "testthat")
  writeLines(c(
    'test_that("passing", {',
    '  expect_true(TRUE)',
    '  skip("not applicable")',
    '})'
  ), file.path(scripts, "test-tool-run.R"))
  writeLines(c(
    'test_that("failure and warning", {',
    '  expect_true(FALSE)',
    '  warning("a warning")',
    '})'
  ), file.path(scripts, "test-config.R"))

  output_file <- withr::local_tempfile()
  withr::local_options(testthat.output_file = output_file)
  reporter <- btw_compact_reporter(test_dir)
  suppressMessages(testthat::test_dir(scripts, reporter = reporter, stop_on_failure = FALSE))
  output <- readLines(output_file, warn = FALSE)
  done <- grep("^[✓✗!] ", output, value = TRUE)
  expect_false(any(grepl("^@ ", output)))
  expect_length(done, 2)
  expect_match(done[[1]], "^✗ config\\s+[0-9.]+s  F:1 W:1$")
  expect_match(done[[2]], "^✓ tool-run\\s+[0-9.]+s  P:1 S:1$")
  expect_true("======== FAILURES ========" %in% output)
  expect_true("======== WARNINGS ========" %in% output)
  expect_equal(tail(output, 1), "[ FAIL 1 | WARN 1 | SKIP 1 | PASS 1 ]")
  expect_false(any(grepl("\033", output, fixed = TRUE)))
  expect_gt(which(output == "======== FAILURES ========"),
            max(which(grepl("^[✓✗!] ", output))))
})

test_that("files finishing out of order print only completion lines", {
  output_file <- withr::local_tempfile()
  withr::local_options(testthat.output_file = output_file)
  reporter <- btw_compact_reporter()
  reporter$start_file("test-a.R")
  reporter$start_file("test-b.R")
  reporter$start_file("test-a.R") # testthat repeats this callback in parallel mode
  reporter$end_file()
  reporter$start_file("test-b.R")
  reporter$end_file()
  reporter$end_reporter()

  output <- readLines(output_file, warn = FALSE)
  expect_false(any(grepl("^@ ", output)))
  expect_match(output[[1]], "^✓ a\\s+[0-9.]+s  P:0$")
  expect_match(output[[2]], "^✓ b\\s+[0-9.]+s  P:0$")
  expect_equal(tail(output, 1), "[ FAIL 0 | WARN 0 | SKIP 0 | PASS 0 ]")
})

test_that("color styling is opt-in", {
  expect_equal(btw_test_color("0.12s", "muted", FALSE), "0.12s")
  withr::local_options(cli.num_colors = 8L)
  expect_true(grepl("\033[", btw_test_color("0.12s", "muted", TRUE), fixed = TRUE))
})

test_that("compact duration uses three display digits", {
  expect_equal(btw_test_duration(0.123), "0.12s")
  expect_equal(btw_test_duration(1.4), "1.40s")
  expect_equal(btw_test_duration(12.34), "12.3s")
  expect_equal(btw_test_duration(123.4), "123s")
  expect_equal(btw_test_duration(9.999), "10.0s")
  expect_equal(btw_test_duration(99.999), "100s")
})

test_that("compact reporter displays empty files and counts errors as failures", {
  test_dir <- withr::local_tempdir()
  scripts <- file.path(test_dir, "tests", "testthat")
  dir.create(scripts, recursive = TRUE)
  writeLines("# no tests", file.path(scripts, "test-empty.R"))
  writeLines('test_that("error", stop("oops"))', file.path(scripts, "test-error.R"))

  output_file <- withr::local_tempfile()
  withr::local_options(testthat.output_file = output_file)
  suppressMessages(testthat::test_dir(
    scripts, reporter = btw_compact_reporter(test_dir), stop_on_failure = FALSE
  ))
  output <- readLines(output_file, warn = FALSE)
  expect_true(any(grepl("^✓ empty\\s+[0-9.]+s  P:0$", output)))
  expect_true(any(grepl("^✗ error\\s+[0-9.]+s  F:1$", output)))
  expect_equal(tail(output, 1), "[ FAIL 1 | WARN 0 | SKIP 0 | PASS 0 ]")
})
