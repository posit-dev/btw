#' @include tool-result.R
NULL

has_devtools <- function() {
  is_installed("devtools")
}

has_roxygen2 <- function() {
  is_installed("roxygen2")
}

btw_can_register_pkg_coverage <- function() {
  is_installed("covr") && is_installed("testthat")
}

# btw_tool_pkg_document --------------------------------------------------------

#' Tool: Generate package documentation
#'
#' Generate package documentation using [devtools::document()]. This runs
#' \pkg{roxygen2} on the package to create/update man pages and `NAMESPACE`.
#'
#' @param pkg Path to package directory. Defaults to `"."`. Must be within
#'   current working directory.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns The output from [devtools::document()].
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @export
btw_tool_pkg_document <- function(pkg = ".", `_intent`) {}

btw_tool_pkg_document_impl <- function(pkg = ".") {
  check_string(pkg)
  check_path_within_current_wd(pkg)

  code <- sprintf(
    'devtools::document(pkg = "%s", roclets = NULL, quiet = FALSE)',
    pkg
  )

  btw_tool_run_r_impl(code)
}

.btw_add_to_tools(
  name = "btw_tool_pkg_document",
  group = "pkg",
  tool = function() {
    ellmer::tool(
      btw_tool_pkg_document_impl,
      name = "btw_tool_pkg_document",
      description = "Generate package documentation.

Runs `devtools::document()` which processes roxygen2 tags to:
- Create/update .Rd help files in man/ directory
- Update NAMESPACE with exports and imports
- Update Collate field in DESCRIPTION if needed

Use this after adding or modifying roxygen2 comments in your R code. The tool modifies files but changes are safe and can be committed to version control. Returns a summary of files created or updated.",
      annotations = ellmer::tool_annotations(
        title = "Package Document",
        read_only_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() has_devtools() && has_roxygen2()
      ),
      arguments = list(
        pkg = ellmer::type_string(
          "Path to package directory. Defaults to '.'. Must be within current working directory.",
          required = FALSE
        )
      )
    )
  }
)

# btw_tool_pkg_check -----------------------------------------------------------

#' Tool: Run R CMD check on a package
#'
#' Run R CMD check on a package using [devtools::check()]. This performs
#' comprehensive checks on the package structure, code, and documentation.
#'
#' The check runs with `remote = TRUE`, `cran = TRUE`, `manual = FALSE`, and
#' `error_on = "never"` to provide comprehensive feedback without failing.
#'
#' @param pkg Path to package directory. Defaults to '.'. Must be within
#'   current working directory.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns The output from [devtools::check()].
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @export
btw_tool_pkg_check <- function(pkg = ".", `_intent`) {}

btw_tool_pkg_check_impl <- function(pkg = ".") {
  check_string(pkg)
  check_path_within_current_wd(pkg)

  withr::local_envvar(TESTHAT_PROBLEMS = "false")
  code <- sprintf(
    'devtools::check(pkg = "%s", remote = TRUE, cran = TRUE, manual = FALSE, quiet = FALSE, error_on = "never")',
    pkg
  )

  btw_tool_run_r_impl(code)
}

.btw_add_to_tools(
  name = "btw_tool_pkg_check",
  group = "pkg",
  tool = function() {
    ellmer::tool(
      btw_tool_pkg_check_impl,
      name = "btw_tool_pkg_check",
      description = "Run comprehensive package checks.

Use this tool to verify the package is ready for release or CRAN submission.

Runs devtools::check() which builds the package and performs ~50 checks including:
- Documentation completeness and validity
- Code syntax and best practices
- Example code execution
- Test suite execution
- Vignette building (if present)
- CRAN policy compliance

This tool runs with CRAN-like settings and takes several minutes to complete. It always completes and reports findings even if errors are found.

For iterative development, use the `btw_tool_pkg_test` if available or `devtools::test()` to run only the test suite for faster feedback.",
      annotations = ellmer::tool_annotations(
        title = "Package Check",
        read_only_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() has_devtools()
      ),
      arguments = list(
        pkg = ellmer::type_string(
          "Path to package directory. Defaults to '.'. Must be within current working directory.",
          required = FALSE
        )
      )
    )
  }
)

# btw_tool_pkg_test ------------------------------------------------------------

#' Tool: Run package tests
#'
#' Run package tests using [devtools::test()]. Optionally filter tests by name
#' pattern.
#'
#' @param pkg Path to package directory. Defaults to '.'. Must be within
#'   current working directory.
#' @param filter Optional regex to filter test files. Example: 'helper' matches
#'   'test-helper.R'.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns The output from [devtools::test()].
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @export
btw_tool_pkg_test <- function(pkg = ".", filter = NULL, `_intent`) {}

btw_tool_pkg_test_impl <- function(pkg = ".", filter = NULL) {
  check_string(pkg)
  check_path_within_current_wd(pkg)

  filter_arg <- if (!is.null(filter)) {
    check_string(filter)
    sprintf(', filter = "%s"', filter)
  } else {
    ""
  }

  code <- sprintf(
    'devtools::test(pkg = "%s"%s, stop_on_failure = FALSE, export_all = TRUE, reporter = "check")',
    pkg,
    filter_arg
  )

  withr::local_envvar(TESTHAT_PROBLEMS = "false")
  btw_tool_run_r_impl(code)
}

.btw_add_to_tools(
  name = "btw_tool_pkg_test",
  group = "pkg",
  tool = function() {
    ellmer::tool(
      btw_tool_pkg_test_impl,
      name = "btw_tool_pkg_test",
      description = "Run testthat tests for an R package.

Runs `devtools::test()` which executes the test suite in tests/testthat/ and reports:
- Number of tests passed, failed, warned, and skipped
- Detailed failure messages with file locations
- Test execution time

The filter parameter accepts a regular expression matched against test file names after stripping the 'test-' prefix and '.R' extension. For example:
- filter = 'helper' runs test-helper.R
- filter = 'tool-.*' runs test-tool-docs.R, test-tool-files.R, etc.
- No filter runs all tests
- It is common to pair `test-{name}.R` with a source `{name}.R` file. To test this file, you can generally use filter = '{name}'.

Use `filter` when working on specific functionality to get faster feedback. The tool always runs all matching tests to completion regardless of failures.",
      annotations = ellmer::tool_annotations(
        title = "Package Test",
        read_only_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() has_devtools()
      ),
      arguments = list(
        pkg = ellmer::type_string(
          "Path to package directory. Defaults to '.'. Must be within current working directory.",
          required = FALSE
        ),
        filter = ellmer::type_string(
          "Optional regex to filter test files. Example: 'helper' matches 'test-helper.R'.",
          required = FALSE
        )
      )
    )
  }
)

# btw_tool_pkg_coverage --------------------------------------------------------

#' Tool: Compute package test coverage
#'
#' Compute test coverage for an R package using [covr::package_coverage()].
#' Returns either a file-level summary for the entire package or line-level
#' details for a specific file.
#'
#' @param pkg Path to package directory. Defaults to `"."`. Must be within
#'   current working directory.
#' @param filename Optional filename to filter coverage results. If `NULL`
#'   (default), returns file-level summary for entire package. If provided,
#'   returns line-level results for the specified file.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns A data frame with different structures depending on `filename`:
#'   * When `filename = NULL`: Returns file-level summary with columns
#'     `filename` and `coverage` (percentage).
#'   * When `filename` is specified: Returns line-level details with columns
#'     `filename`, `functions`, `line_start`, `line_end`, `is_covered`, and
#'     `med_hits`.
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @export
btw_tool_pkg_coverage <- function(pkg = ".", filename = NULL, `_intent`) {}

btw_tool_pkg_coverage_impl <- function(pkg = ".", filename = NULL) {
  check_string(pkg)
  check_path_within_current_wd(pkg)
  check_string(filename, allow_null = TRUE)
  check_installed("covr")
  check_installed("testthat")
  check_installed("pkgload")

  if (!is.null(filename)) {
    title <- "Test File Coverage"
    code <- sprintf(
      'btw:::btw_coverage_file(path = "%s", filename = "%s")',
      pkg,
      filename
    )
  } else {
    title <- "Package Coverage"
    code <- sprintf(
      'btw:::btw_coverage_package(path = "%s")',
      pkg
    )
  }

  res <- btw_tool_run_r_impl(code)

  res@extra$display <- res@extra$display %||% list()
  res@extra$display$title <- title

  if (!is.null(res@extra$data)) {
    res@extra$display$markdown <- md_table(res@extra$data)
  }

  res
}

.btw_add_to_tools(
  name = "btw_tool_pkg_coverage",
  group = "pkg",
  tool = function() {
    ellmer::tool(
      btw_tool_pkg_coverage_impl,
      name = "btw_tool_pkg_coverage",
      description = "Compute test coverage for an R package.

Runs `covr::package_coverage()` to analyze which lines of code in your package are covered by tests. DO NOT separately run the tests; this tool runs the tests and calculates test coverage. If the tests fail, test failures are reported.

The tool returns different information depending on whether you specify a filename:

**Without filename (package-level summary):**
- Returns coverage percentage for each file in the package
- Use this to identify which files need more test coverage

**With filename (line-level details):**
- Returns detailed coverage for the specified file
- Shows contiguous line ranges grouped by coverage status (covered vs uncovered)
- Identifies which specific functions and lines are tested
- Includes median hit counts showing how many times lines were executed

This helps identify untested code and gaps in your test suite.",
      annotations = ellmer::tool_annotations(
        title = "Package Coverage",
        read_only_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = btw_can_register_pkg_coverage
      ),
      arguments = list(
        pkg = ellmer::type_string(
          "Path to package directory. Defaults to '.'. Must be within current working directory.",
          required = FALSE
        ),
        filename = ellmer::type_string(
          "Optional filename to get detailed line-level coverage. If omitted, returns file-level summary for entire package. If provided, returns line-level details for the specified file.",
          required = FALSE
        )
      )
    )
  }
)


btw_find_test_file <- function(source_file, pkg_path) {
  # Extract base name without extension (e.g., "tool-pkg" from "R/tool-pkg.R")
  base_name <- tools::file_path_sans_ext(basename(source_file))

  # Construct test file path
  test_file <- file.path(
    pkg_path,
    "tests",
    "testthat",
    paste0("test-", base_name, ".R")
  )

  if (file.exists(test_file)) {
    return(test_file)
  }

  return(NULL)
}

btw_coverage_package <- function(path = ".") {
  # Run coverage for entire package with all tests
  res <- covr::package_coverage(path)

  # Print overall coverage percentage
  coverage_list <- covr::coverage_to_list(res)
  cat(sprintf(
    "Overall package coverage: %0.2f%%\n\n",
    coverage_list$totalcoverage
  ))

  # Return file-level coverage percentages
  file_coverage <- coverage_list$filecoverage

  if (length(file_coverage) == 0) {
    return(invisible(NULL))
  }

  # Convert list to data frame
  result <- data.frame(
    filename = names(file_coverage),
    coverage = unlist(file_coverage, use.names = FALSE)
  )

  # Sort by coverage in descending order
  result <- result[order(result$coverage, decreasing = TRUE), ]
  rownames(result) <- NULL

  # Print in compact format
  for (i in seq_len(nrow(result))) {
    cat(sprintf("%s - %0.2f%%\n", result$filename[i], result$coverage[i]))
  }

  invisible(result)
}

btw_coverage_file <- function(path = ".", filename) {
  # Resolve source file
  if (!fs::file_exists(filename)) {
    if (fs::file_exists(fs::path(path, "R", filename))) {
      filename <- fs::path(path, "R", filename)
    }
  }

  if (!fs::file_exists(filename)) {
    cli::cli_abort(c(
      "Source file {.file {filename}} does not exist.",
      "i" = "Provide a valid source file within the package."
    ))
  }

  # Find corresponding test file
  test_file <- btw_find_test_file(filename, path)

  if (is.null(test_file)) {
    cli::cli_abort(c(
      "No test file found for {.file {filename}}.",
      "i" = "Expected test file: {.file tests/testthat/test-{tools::file_path_sans_ext(basename(filename))}.R}",
      "i" = "To run coverage for all files, omit the {.arg filename} parameter."
    ))
  }

  if (!is_installed("pkgload")) {
    cli::cli_abort(c(
      "Package {.pkg pkgload} is required for file-level coverage.",
      "i" = "Install it with: {.code install.packages(\"pkgload\")}",
      "i" = "Or run coverage for all files by omitting the {.arg filename} parameter."
    ))
  }

  path <- fs::path_real(path)
  filename <- fs::path_real(filename)
  test_dir <- fs::path_dir(test_file)

  # Load package environment
  pkg_env <- suppressMessages(
    pkgload::load_all(path, quiet = TRUE, export_all = TRUE)$env
  )

  testthat::local_test_directory(test_dir)
  withr::local_options(rlang_interactive = FALSE)

  snap_reporter <- testthat::local_snapshotter()
  reporter <- testthat::MultiReporter$new(
    reporters = list(testthat::StopReporter$new(praise = FALSE), snap_reporter)
  )

  # Run coverage with just the specific test file
  withr::local_envvar(devtools::r_env_vars())
  testthat::with_reporter(reporter, {
    res <- covr::environment_coverage(pkg_env, fs::path_file(test_file))
  })

  # Print overall coverage percentage for this file
  coverage_list <- covr::coverage_to_list(res)
  if (!is.null(coverage_list$filecoverage[[filename]])) {
    cat(sprintf(
      "Coverage for %s: %0.2f%%\n",
      filename,
      coverage_list$filecoverage[[filename]]
    ))
  }

  # Get detailed coverage information for the specified file
  coverage_df <- covr::tally_coverage(res)

  # Filter by filename
  coverage_df <- coverage_df[coverage_df$filename == filename, ]

  if (nrow(coverage_df) == 0) {
    return(data.frame(
      filename = character(),
      functions = character(),
      line_start = integer(),
      line_end = integer(),
      is_covered = logical(),
      med_hits = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # Sort by functions and line
  coverage_df <- coverage_df[
    order(coverage_df$functions, coverage_df$line),
  ]

  # Add is_covered column
  coverage_df$is_covered <- coverage_df$value > 0

  # Process each function to group contiguous lines
  result_list <- list()
  unique_funcs <- unique(coverage_df$functions)

  # Sort functions by their first line number in source
  func_first_line <- sapply(unique_funcs, function(func) {
    min(coverage_df$line[coverage_df$functions == func])
  })
  unique_funcs <- unique_funcs[order(func_first_line)]

  for (func in unique_funcs) {
    subset_df <- coverage_df[coverage_df$functions == func, ]
    n <- nrow(subset_df)

    # Compute group breaks for contiguous line ranges with same coverage status
    group_id <- 1
    groups <- rep(1, n)

    if (n > 1) {
      for (j in 2:n) {
        line_gap <- subset_df$line[j] - subset_df$line[j - 1] != 1
        cov_change <- subset_df$is_covered[j] != subset_df$is_covered[j - 1]

        if (line_gap || cov_change) {
          group_id <- group_id + 1
        }
        groups[j] <- group_id
      }
    }

    # Collect uncovered line ranges
    uncovered_ranges <- character()
    for (g in unique(groups)) {
      grp_idx <- groups == g
      if (!subset_df$is_covered[grp_idx][1]) {
        start <- min(subset_df$line[grp_idx])
        end <- max(subset_df$line[grp_idx])
        uncovered_ranges <- c(
          uncovered_ranges,
          if (start == end) as.character(start) else paste0(start, "-", end)
        )
      }
    }

    if (length(uncovered_ranges) > 0) {
      result_list[[func]] <- uncovered_ranges
    }
  }

  # Format as compact summary string
  MAX_DISPLAY <- 20
  summary_parts <- character()
  for (func in names(result_list)) {
    if (length(result_list[[func]]) > MAX_DISPLAY) {
      n_remaining <- length(result_list[[func]]) - MAX_DISPLAY
      ranges_str <- paste0(
        paste(head(result_list[[func]], MAX_DISPLAY - 5), collapse = ", "),
        ", [... ",
        n_remaining,
        " more ...], ",
        paste(tail(result_list[[func]], 5), collapse = ", ")
      )
    } else {
      ranges_str <- paste(result_list[[func]], collapse = ", ")
    }
    summary_parts <- c(summary_parts, paste0(func, "(): ", ranges_str))
  }

  summary_str <- paste(summary_parts, collapse = "\n")
  cat("Lines not covered by tests:\n")
  cat(summary_str)

  res <- coverage_df
  res$filename <- fs::path_rel(res$filename, start = path)
  res$is_covered <- ifelse(res$is_covered, "yes", "no")
  invisible(res[order(res$line), ])
}
