#' @include tool-result.R
NULL

has_devtools <- function() {
  is_installed("devtools")
}

has_roxygen2 <- function() {
  is_installed("roxygen2")
}

has_pkgload_and_callr <- function() {
  is_installed("pkgload") && is_installed("callr")
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
#' @family pkg tools
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
#' @family pkg tools
#' @export
btw_tool_pkg_check <- function(pkg = ".", `_intent`) {}

btw_tool_pkg_check_impl <- function(pkg = ".") {
  check_string(pkg)
  check_path_within_current_wd(pkg)

  withr::local_envvar(TESTTHAT_PROBLEMS = "false")
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
#' @family pkg tools
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

  rptr <- if (utils::packageVersion("testthat") >= "3.3.2") "llm" else "check"

  code <- sprintf(
    'devtools::test(pkg = "%s"%s, stop_on_failure = FALSE, export_all = TRUE, reporter = "%s")',
    pkg,
    filter_arg,
    rptr
  )

  withr::local_envvar(TESTTHAT_PROBLEMS = "false")
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

# btw_tool_pkg_load_all --------------------------------------------------------

#' Tool: Load package code
#'
#' Load package code using [pkgload::load_all()] in a separate R process via
#' [callr::r()]. This verifies that the package code loads without syntax errors
#' and triggers recompilation of any compiled code (C, C++, etc.).
#'
#' **Important:** This tool runs `load_all()` in an isolated R process and does
#' NOT load the package code into your current R session. If you need to load
#' the package code in your current session for interactive use, use the run R
#' code tool to call `pkgload::load_all()` directly.
#'
#' @param pkg Path to package directory. Defaults to '.'. Must be within
#'   current working directory.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns The output from [pkgload::load_all()].
#'
#' @seealso [btw_tools()]
#' @family pkg tools
#' @export
btw_tool_pkg_load_all <- function(pkg = ".", `_intent`) {}

btw_tool_pkg_load_all_impl <- function(pkg = ".") {
  check_installed("pkgload")
  check_installed("callr")
  check_string(pkg)
  check_path_within_current_wd(pkg)

  code <- glue_("btw:::btw_pkg_load_all(pkg = '{{pkg}}')")
  btw_tool_run_r_impl(code, show_last_value = FALSE)
}

btw_pkg_load_all <- function(pkg = ".") {
  res <- tryCatch(
    callr::r(
      function(pkg) {
        pkgload::load_all(pkg, export_all = FALSE, quiet = FALSE)
      },
      args = list(pkg = pkg)
    ),
    error = function(e) {
      rlang::cnd_signal(e$parent)
    }
  )
  cat("Package loaded successfully!")
  invisible(res)
}

.btw_add_to_tools(
  name = "btw_tool_pkg_load_all",
  group = "pkg",
  tool = function() {
    ellmer::tool(
      btw_tool_pkg_load_all_impl,
      name = "btw_tool_pkg_load_all",
      description = "Load package code to verify it loads correctly.

Runs `pkgload::load_all()` to verify the package can be loaded (without affecting the user's current session).

## When to Use This Tool

Use this tool when:
- You need quick verification that package code loads without running tests or R CMD check
- You want to verify code changes haven't broken the package structure
- You need to trigger recompilation of compiled code
- You want a natural prerequisite step before running tests or examples

This is lighter and faster than running tests or checks when you only need to verify loadability. If you need to run tests, use the `btw_tool_pkg_test` tool instead, which also verifies loading.

**IMPORTANT:** This tool does NOT load the package into your current R session. The code runs in an isolated subprocess that exits after completion. If you need the package loaded in your current session for interactive use, use the run R code tool instead and call `pkgload::load_all()` directly.",
      annotations = ellmer::tool_annotations(
        title = "Package Load All",
        read_only_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() has_pkgload_and_callr()
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
