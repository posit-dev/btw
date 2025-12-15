# Test btw_tool_pkg_document --------------------------------------------------

test_that("btw_tool_pkg_document constructs correct code", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Documentation updated")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_document_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::document")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, "roclets = NULL")
  expect_match(result@extra$code, "quiet = FALSE")
})

test_that("btw_tool_pkg_document handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Documentation updated")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_document_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
})

test_that("btw_tool_pkg_document rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_document_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_document_impl("../outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_document_impl("/etc/passwd"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_document validates pkg argument", {
  expect_error(btw_tool_pkg_document_impl(123))
  expect_error(btw_tool_pkg_document_impl(c("a", "b")))
  expect_error(btw_tool_pkg_document_impl(NULL))
})

# Test btw_tool_pkg_check ------------------------------------------------------

test_that("btw_tool_pkg_check constructs correct code", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "R CMD check output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_check_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::check")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, "remote = TRUE")
  expect_match(result@extra$code, "cran = TRUE")
  expect_match(result@extra$code, "manual = FALSE")
  expect_match(result@extra$code, "quiet = FALSE")
  expect_match(result@extra$code, 'error_on = "never"')
})

test_that("btw_tool_pkg_check handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "R CMD check output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_check_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
})

test_that("btw_tool_pkg_check rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_check_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_check_impl("../outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_check_impl("/etc/passwd"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_check validates pkg argument", {
  expect_error(btw_tool_pkg_check_impl(123))
  expect_error(btw_tool_pkg_check_impl(c("a", "b")))
  expect_error(btw_tool_pkg_check_impl(NULL))
})

# Test btw_tool_pkg_test -------------------------------------------------------

test_that("btw_tool_pkg_test constructs correct code without filter", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::test")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, "stop_on_failure = FALSE")
  expect_match(result@extra$code, "export_all = TRUE")
  # Should NOT have filter argument
  expect_false(grepl("filter", result@extra$code))
})

test_that("btw_tool_pkg_test constructs correct code with filter", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl(".", filter = "helper")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::test")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, 'filter = "helper"')
  expect_match(result@extra$code, "stop_on_failure = FALSE")
  expect_match(result@extra$code, "export_all = TRUE")
})

test_that("btw_tool_pkg_test handles different filter patterns", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  # Test with regex pattern
  result1 <- btw_tool_pkg_test_impl(".", filter = "tool-.*")
  expect_match(result1@extra$code, 'filter = "tool-.*"')

  # Test with simple string
  result2 <- btw_tool_pkg_test_impl(".", filter = "pkg")
  expect_match(result2@extra$code, 'filter = "pkg"')
})

test_that("btw_tool_pkg_test handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
})

test_that("btw_tool_pkg_test handles subdirectory paths with filter", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl("subdir/pkg", filter = "helper")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
  expect_match(result@extra$code, 'filter = "helper"')
})

test_that("btw_tool_pkg_test rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_test_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_test_impl("../outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_test_impl("/etc/passwd"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_test validates pkg argument", {
  expect_error(btw_tool_pkg_test_impl(123))
  expect_error(btw_tool_pkg_test_impl(c("a", "b")))
  expect_error(btw_tool_pkg_test_impl(NULL))
})

test_that("btw_tool_pkg_test validates filter argument", {
  expect_error(btw_tool_pkg_test_impl(".", filter = 123))
  expect_error(btw_tool_pkg_test_impl(".", filter = c("a", "b")))
  # NULL should be allowed (no filter)
  expect_no_error({
    local_mocked_bindings(
      btw_tool_run_r_impl = function(code) {
        BtwRunToolResult(
          value = list(ContentOutput(text = "Test output")),
          extra = list(
            code = code,
            status = "success",
            data = NULL,
            contents = list()
          )
        )
      }
    )
    btw_tool_pkg_test_impl(".", filter = NULL)
  })
})

# Test code construction with special characters -------------------------------

test_that("btw_tool_pkg_check handles paths with spaces correctly", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    },
    check_path_within_current_wd = function(path) invisible(path)
  )

  result <- btw_tool_pkg_check_impl("path with spaces")
  expect_s7_class(result, BtwRunToolResult)
  # deparse() should properly quote the path
  expect_match(result@extra$code, 'pkg = "path with spaces"')
})

test_that("btw_tool_pkg_test handles filter with special regex chars", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl(".", filter = "test-.*\\.R$")
  expect_s7_class(result, BtwRunToolResult)
  # deparse() should properly quote the regex
  expect_true(grepl(
    'filter = \"test-.*\\.R$\"',
    result@extra$code,
    fixed = TRUE
  ))
})

# Test return values -----------------------------------------------------------

test_that("all pkg tools return BtwRunToolResult objects", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result1 <- btw_tool_pkg_document_impl(".")
  expect_s7_class(result1, BtwRunToolResult)
  expect_type(result1@value, "list")
  expect_equal(result1@extra$status, "success")

  result2 <- btw_tool_pkg_check_impl(".")
  expect_s7_class(result2, BtwRunToolResult)
  expect_type(result2@value, "list")
  expect_equal(result2@extra$status, "success")

  result3 <- btw_tool_pkg_test_impl(".")
  expect_s7_class(result3, BtwRunToolResult)
  expect_type(result3@value, "list")
  expect_equal(result3@extra$status, "success")
})

test_that("pkg tools pass through btw_tool_run_r_impl status", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentError(text = "Error message")),
        extra = list(
          code = code,
          status = "error",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result1 <- btw_tool_pkg_document_impl(".")
  expect_equal(result1@extra$status, "error")

  result2 <- btw_tool_pkg_check_impl(".")
  expect_equal(result2@extra$status, "error")

  result3 <- btw_tool_pkg_test_impl(".")
  expect_equal(result3@extra$status, "error")
})

# Test btw_tool_pkg_coverage ---------------------------------------------------

test_that("btw_tool_pkg_coverage constructs correct code for package coverage", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Coverage output")),
        extra = list(
          code = code,
          status = "success",
          data = data.frame(
            filename = c("R/example.R", "R/helper.R"),
            coverage = c(85.5, 92.3)
          ),
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_coverage_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "btw:::btw_coverage_package")
  expect_match(result@extra$code, 'path = "."')
  expect_equal(result@extra$display$title, "Package Coverage")
  expect_type(result@extra$display$markdown, "character")
})

test_that("btw_tool_pkg_coverage constructs correct code for file coverage", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Coverage output")),
        extra = list(
          code = code,
          status = "success",
          data = data.frame(
            filename = "R/example.R",
            functions = "add_numbers",
            line_start = 1,
            line_end = 10,
            is_covered = "yes",
            med_hits = 5
          ),
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_coverage_impl(".", filename = "example.R")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "btw:::btw_coverage_file")
  expect_match(result@extra$code, 'path = "."')
  expect_match(result@extra$code, 'filename = "example.R"')
  expect_equal(result@extra$display$title, "Test File Coverage")
  expect_type(result@extra$display$markdown, "character")
})

test_that("btw_tool_pkg_coverage validates pkg argument", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  expect_error(btw_tool_pkg_coverage_impl(123))
  expect_error(btw_tool_pkg_coverage_impl(c("a", "b")))
  expect_error(btw_tool_pkg_coverage_impl(NULL))
})

test_that("btw_tool_pkg_coverage validates filename argument", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  expect_error(btw_tool_pkg_coverage_impl(".", filename = 123))
  expect_error(btw_tool_pkg_coverage_impl(".", filename = c("a", "b")))

  # NULL should be allowed (package-level coverage)
  expect_no_error({
    local_mocked_bindings(
      btw_tool_run_r_impl = function(code) {
        BtwRunToolResult(
          value = list(ContentOutput(text = "Coverage output")),
          extra = list(
            code = code,
            status = "success",
            data = NULL,
            contents = list()
          )
        )
      }
    )
    btw_tool_pkg_coverage_impl(".", filename = NULL)
  })
})

test_that("btw_tool_pkg_coverage rejects paths outside current directory", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  expect_error(
    btw_tool_pkg_coverage_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_coverage_impl("../outside"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_coverage handles subdirectory paths", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Coverage output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_coverage_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'path = "subdir/pkg"')
})

test_that("btw_tool_pkg_coverage sets markdown display when data is present", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Coverage output")),
        extra = list(
          code = code,
          status = "success",
          data = data.frame(
            filename = "R/example.R",
            coverage = 85.5
          ),
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_coverage_impl(".")
  expect_type(result@extra$display$markdown, "character")
  expect_true(nchar(result@extra$display$markdown) > 0)
})

test_that("btw_tool_pkg_coverage handles NULL data gracefully", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Coverage output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_coverage_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  # Should not error, just have no markdown display
})

# Test helper functions --------------------------------------------------------

test_that("btw_can_register_pkg_coverage checks for required packages", {
  # When both packages are installed
  local_mocked_bindings(
    is_installed = function(pkg) TRUE
  )
  expect_true(btw_can_register_pkg_coverage())

  # When covr is missing
  local_mocked_bindings(
    is_installed = function(pkg) pkg != "covr"
  )
  expect_false(btw_can_register_pkg_coverage())

  # When testthat is missing
  local_mocked_bindings(
    is_installed = function(pkg) pkg != "testthat"
  )
  expect_false(btw_can_register_pkg_coverage())

  # When both are missing
  local_mocked_bindings(
    is_installed = function(pkg) FALSE
  )
  expect_false(btw_can_register_pkg_coverage())
})

test_that("btw_find_test_file finds corresponding test file", {
  pkg_dir <- withr::local_tempdir()

  # Create test file structure
  test_dir <- file.path(pkg_dir, "tests", "testthat")
  dir.create(test_dir, recursive = TRUE)
  file.create(file.path(test_dir, "test-example.R"))

  # Should find the test file
  result <- btw_find_test_file("R/example.R", pkg_dir)
  expect_true(!is.null(result))
  expect_true(file.exists(result))
  expect_match(result, "test-example.R$")
})

test_that("btw_find_test_file returns NULL when test file doesn't exist", {
  pkg_dir <- withr::local_tempdir()

  # Create test directory but no test file
  test_dir <- file.path(pkg_dir, "tests", "testthat")
  dir.create(test_dir, recursive = TRUE)

  # Should return NULL
  result <- btw_find_test_file("R/example.R", pkg_dir)
  expect_null(result)
})

test_that("btw_find_test_file handles different source file patterns", {
  pkg_dir <- withr::local_tempdir()
  test_dir <- file.path(pkg_dir, "tests", "testthat")
  dir.create(test_dir, recursive = TRUE)

  # Test with different base names
  file.create(file.path(test_dir, "test-tool-pkg.R"))
  result1 <- btw_find_test_file("R/tool-pkg.R", pkg_dir)
  expect_true(!is.null(result1))
  expect_match(result1, "test-tool-pkg.R$")

  file.create(file.path(test_dir, "test-utils.R"))
  result2 <- btw_find_test_file("R/utils.R", pkg_dir)
  expect_true(!is.null(result2))
  expect_match(result2, "test-utils.R$")
})

# Integration tests with mock package ------------------------------------------

test_that("btw_coverage_package works with minimal package", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")

  pkg_dir <- local_minimal_package()

  # Change to package directory
  withr::local_dir(pkg_dir)

  result <- btw_tool_pkg_coverage(".")

  # Should return a data frame with coverage info
  data <- result@extra$data
  expect_s3_class(data, "data.frame")
  expect_true("filename" %in% names(data))
  expect_true("coverage" %in% names(data))
  expect_true(nrow(data) > 0)

  # Coverage values should be between 0 and 100
  expect_true(all(data$coverage >= 0 & data$coverage <= 100))
})

test_that("btw_coverage_file works with minimal package", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  pkg_dir <- local_minimal_package()
  withr::local_dir(pkg_dir)

  # Test with full path
  result <- btw_tool_pkg_coverage(".", "R/example.R")

  # Should return a data frame with detailed coverage
  data <- result@extra$data
  expect_s3_class(data, "data.frame")
  expect_true("filename" %in% names(data))
  expect_true("functions" %in% names(data))
  expect_true("line" %in% names(data))
  expect_true("value" %in% names(data))
})

test_that("btw_coverage_file resolves relative filenames", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  pkg_dir <- local_minimal_package()
  withr::local_dir(pkg_dir)

  # Should work with just the filename (no R/ prefix)
  result <- btw_tool_pkg_coverage(".", "example.R")
  data <- result@extra$data
  expect_s3_class(data, "data.frame")
  expect_true(nrow(data) > 0)
})

test_that("btw_coverage_file errors on missing source file", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  pkg_dir <- local_minimal_package()
  withr::local_dir(pkg_dir)

  expect_error(
    btw_coverage_file(".", "nonexistent.R"),
    "does not exist"
  )
})

test_that("btw_coverage_file errors when test file is missing", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  pkg_dir <- local_minimal_package(with_tests = FALSE)
  withr::local_dir(pkg_dir)

  expect_error(
    btw_coverage_file(".", "R/example.R"),
    "No test file found"
  )
})

test_that("btw_coverage_package handles package with no tests", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")

  pkg_dir <- local_minimal_package(with_tests = FALSE, with_coverage = FALSE)
  withr::local_dir(pkg_dir)

  # Should complete without error, but may have 0 coverage
  result <- btw_tool_pkg_coverage(".")

  # Result might be NULL or empty data frame
  data <- result@extra$data
  expect_true(is.null(data) || nrow(data) == 0)
})

test_that("btw_coverage_file reports uncovered lines", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  skip_if_not_installed("pkgload")

  pkg_dir <- local_minimal_package()
  withr::local_dir(pkg_dir)

  # Capture output to check for uncovered line reporting
  result <- btw_tool_pkg_coverage(".", "R/example.R")

  # Should mention multiply_numbers as uncovered (it has no tests)
  output_text <- result@value
  expect_match(output_text, "multiply_numbers", ignore.case = TRUE)
})

test_that("has_devtools checks for devtools package", {
  local_mocked_bindings(
    is_installed = function(pkg) pkg == "devtools"
  )
  expect_true(has_devtools())

  local_mocked_bindings(
    is_installed = function(pkg) FALSE
  )
  expect_false(has_devtools())
})

test_that("has_roxygen2 checks for roxygen2 package", {
  local_mocked_bindings(
    is_installed = function(pkg) pkg == "roxygen2"
  )
  expect_true(has_roxygen2())

  local_mocked_bindings(
    is_installed = function(pkg) FALSE
  )
  expect_false(has_roxygen2())
})
