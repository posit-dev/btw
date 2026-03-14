use_latest_pandoc()

btw_cli_path <- function() {
  path <- test_path("..", "..", "exec", "btw.R")
  if (!file.exists(path)) {
    skip("btw CLI script not found")
  }
  normalizePath(path)
}

run_btw <- function(...) {
  Rapp::run(btw_cli_path(), c(...))
}

run_btw_quietly <- function(...) {
  output <- capture.output(env <- run_btw(...))
  env$.output <- output
  env
}

run_btw_subprocess <- function(...) {
  skip_if_not_installed("processx")
  pkg_dir <- normalizePath(test_path("..", ".."))
  app <- btw_cli_path()
  args <- c(...)
  quoted_args <- paste0('"', args, '"', collapse = ", ")
  script <- paste0(
    'pkgload::load_all("',
    pkg_dir,
    '", quiet = TRUE); ',
    'Rapp::run("',
    app,
    '", c(',
    quoted_args,
    '))'
  )
  processx::run("Rscript", c("-e", script), error_on_status = FALSE)
}

# help output -----------------------------------------------------------

test_that("btw --help shows top-level help", {
  expect_snapshot(run_btw("--help"))
})

test_that("btw docs --help shows docs group help", {
  expect_snapshot(run_btw("docs", "--help"))
})

test_that("btw docs help --help shows help subcommand usage", {
  expect_snapshot(run_btw("docs", "help", "--help"))
})

test_that("btw pkg --help shows pkg group help", {
  expect_snapshot(run_btw("pkg", "--help"))
})

test_that("btw info --help shows info group help", {
  expect_snapshot(run_btw("info", "--help"))
})

test_that("btw cran --help shows cran group help", {
  expect_snapshot(run_btw("cran", "--help"))
})

# --version flag ---------------------------------------------------------

test_that("btw --version prints version and exits 0", {
  result <- run_btw_subprocess("--version")
  expect_equal(result$status, 0)
  expect_match(trimws(result$stdout), "^[0-9]+\\.[0-9]+")
})

# docs help --------------------------------------------------------------

test_that("btw docs help <topic> resolves topic first", {
  local_skip_pandoc_convert_text()
  env <- run_btw_quietly("docs", "help", "rnorm")
  expect_true(is.environment(env))
  expect_equal(env$topic, "rnorm")
  expect_equal(env$package, "")
})

test_that("btw docs help {package} lists help topics", {
  env <- run_btw_quietly("docs", "help", "{stats}")
  expect_true(is.environment(env))
  expect_equal(env$topic, "{stats}")
  expect_equal(env$package, "")
})

test_that("btw docs help <topic> -p <package> reads help page", {
  local_skip_pandoc_convert_text()
  env <- run_btw_quietly("docs", "help", "rnorm", "-p", "stats")
  expect_equal(env$topic, "rnorm")
  expect_equal(env$package, "stats")
})

test_that("btw docs help <name> falls back to package listing", {
  # "stats" as a topic resolves to ?stats, but a name that is only a package
  # (not a topic) should fall back to package listing
  env <- run_btw_quietly("docs", "help", "stats")
  expect_equal(env$topic, "stats")
  expect_equal(env$package, "")
})

test_that("btw docs help pkg::topic reads scoped help page", {
  local_skip_pandoc_convert_text()
  env <- run_btw_quietly("docs", "help", "stats::rnorm")
  expect_equal(env$topic, "stats::rnorm")
  expect_equal(env$package, "")
})

test_that("btw docs help pkg::topic ignores --package with warning", {
  local_skip_pandoc_convert_text()
  expect_warning(
    env <- run_btw_quietly("docs", "help", "stats::rnorm", "-p", "base"),
    "Ignoring --package"
  )
  expect_equal(env$topic, "stats::rnorm")
  expect_equal(env$package, "base")
})

test_that("btw docs help errors for unknown topic", {
  result <- run_btw_subprocess("docs", "help", "completely_nonexistent_xyz")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "completely_nonexistent_xyz", ignore.case = TRUE)
})

# docs vignette ----------------------------------------------------------

test_that("btw docs vignette <package> --list lists vignettes", {
  skip_if_not_installed("dplyr")
  env <- run_btw_quietly("docs", "vignette", "dplyr", "--list")
  expect_true(env$list)
  expect_equal(env$package, "dplyr")
})

test_that("btw docs vignette <package> reads intro vignette", {
  skip_if_not_installed("dplyr")
  local_skip_pandoc_convert()
  env <- run_btw_quietly("docs", "vignette", "dplyr")
  expect_equal(env$package, "dplyr")
  expect_equal(env$name, "")
  expect_false(env$list)
})

test_that("btw docs vignette <package> -n <name> reads specific vignette", {
  skip_if_not_installed("dplyr")
  local_skip_pandoc_convert()
  env <- run_btw_quietly("docs", "vignette", "dplyr", "-n", "programming")
  expect_equal(env$package, "dplyr")
  expect_equal(env$name, "programming")
})

test_that("btw docs vignette falls back to listing when no intro vignette", {
  skip_if_not_installed("testthat")
  # testthat has vignettes but none named "testthat"
  capture.output(
    env <- run_btw_quietly("docs", "vignette", "testthat"),
    type = "message"
  )
  expect_equal(env$package, "testthat")
})

# docs news --------------------------------------------------------------

test_that("btw docs news <package> reads news", {
  local_skip_pandoc_convert_text()
  env <- run_btw_quietly("docs", "news", "dplyr")
  expect_equal(env$package, "dplyr")
  expect_equal(env$search, "")
})

test_that("btw docs news <package> -s <term> searches news", {
  local_skip_pandoc_convert_text()
  env <- run_btw_quietly("docs", "news", "dplyr", "-s", "filter")
  expect_equal(env$package, "dplyr")
  expect_equal(env$search, "filter")
})

test_that("btw docs news errors for non-existent package", {
  result <- run_btw_subprocess("docs", "news", "nonexistent_pkg_xyz")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "not installed|not found", ignore.case = TRUE)
})

# pkg group --------------------------------------------------------------

test_that("btw pkg document calls document impl", {
  local_mocked_bindings(
    btw_tool_pkg_document_impl = function(pkg) "Documentation generated."
  )
  env <- run_btw_quietly("pkg", "document")
  expect_equal(env$path, ".")
})

test_that("btw pkg document with --path", {
  local_mocked_bindings(
    btw_tool_pkg_document_impl = function(pkg) "Documentation generated."
  )
  env <- run_btw_quietly("pkg", "document", "--path", "/tmp/mypkg")
  expect_equal(env$path, "/tmp/mypkg")
})

test_that("btw pkg check calls check impl", {
  local_mocked_bindings(
    btw_tool_pkg_check_impl = function(pkg) "R CMD check passed."
  )
  env <- run_btw_quietly("pkg", "check")
  expect_equal(env$path, ".")
})

test_that("btw pkg test calls test impl with filter", {
  mock_filter <- NULL
  local_mocked_bindings(
    btw_tool_pkg_test_impl = function(pkg, filter = NULL) {
      mock_filter <<- filter
      "Tests passed."
    }
  )
  env <- run_btw_quietly("pkg", "test", "-f", "utils")
  expect_equal(env$filter, "utils")
  expect_equal(mock_filter, "utils")
})

test_that("btw pkg test without filter passes NULL", {
  mock_filter <- "SENTINEL"
  local_mocked_bindings(
    btw_tool_pkg_test_impl = function(pkg, filter = NULL) {
      mock_filter <<- filter
      "Tests passed."
    }
  )
  run_btw_quietly("pkg", "test")
  expect_null(mock_filter)
})

test_that("btw pkg load calls load impl", {
  local_mocked_bindings(
    btw_tool_pkg_load_all_impl = function(pkg) "Package loaded."
  )
  env <- run_btw_quietly("pkg", "load")
  expect_equal(env$path, ".")
})

test_that("btw pkg coverage calls coverage impl", {
  local_mocked_bindings(
    btw_tool_pkg_coverage_impl = function(pkg, filename = NULL) "Coverage: 80%"
  )
  env <- run_btw_quietly("pkg", "coverage")
  expect_equal(env$file, "")
})

test_that("btw pkg coverage --file passes filename", {
  mock_filename <- NULL
  local_mocked_bindings(
    btw_tool_pkg_coverage_impl = function(pkg, filename = NULL) {
      mock_filename <<- filename
      "Coverage: 80%"
    }
  )
  env <- run_btw_quietly("pkg", "coverage", "--file", "utils.R")
  expect_equal(env$file, "utils.R")
  expect_equal(mock_filename, "utils.R")
})

# info group -------------------------------------------------------------

test_that("btw info platform shows platform info", {
  local_sessioninfo_quarto_version()
  env <- run_btw_quietly("info", "platform")
  expect_true(is.environment(env))
})

test_that("btw info packages with no args defaults to attached", {
  mock_pkgs <- NULL
  local_mocked_bindings(
    btw_tool_sessioninfo_package_impl = function(packages, dependencies) {
      mock_pkgs <<- packages
      "package info"
    }
  )
  run_btw_quietly("info", "packages")
  expect_equal(mock_pkgs, "attached")
})

test_that("btw info packages with package names", {
  mock_pkgs <- NULL
  local_mocked_bindings(
    btw_tool_sessioninfo_package_impl = function(packages, dependencies) {
      mock_pkgs <<- packages
      "package info"
    }
  )
  run_btw_quietly("info", "packages", "dplyr", "ggplot2")
  expect_equal(mock_pkgs, c("dplyr", "ggplot2"))
})

test_that("btw info packages --check calls is_package_installed", {
  checked_pkgs <- character()
  local_mocked_bindings(
    btw_tool_sessioninfo_is_package_installed_impl = function(package_name) {
      checked_pkgs <<- c(checked_pkgs, package_name)
      paste(package_name, "is installed")
    }
  )
  run_btw_quietly("info", "packages", "dplyr", "ggplot2", "--check")
  expect_equal(checked_pkgs, c("dplyr", "ggplot2"))
})

test_that("btw info packages --deps passes dependency types", {
  mock_deps <- NULL
  local_mocked_bindings(
    btw_tool_sessioninfo_package_impl = function(packages, dependencies) {
      mock_deps <<- dependencies
      "package info"
    }
  )
  run_btw_quietly("info", "packages", "dplyr", "--deps", "Imports,Suggests")
  expect_equal(mock_deps, "Imports,Suggests")
})

# cran group -------------------------------------------------------------

test_that("btw cran search calls pkgsearch and btw_this", {
  local_mocked_bindings(pkg_search = mock_pkgsearch, .package = "pkgsearch")
  env <- run_btw_quietly("cran", "search", "string interpolation")
  expect_equal(env$query, "string interpolation")
  expect_equal(env$format, "short")
  expect_true(is.na(env$n))
})

test_that("btw cran search with --format and -n", {
  local_mocked_bindings(pkg_search = mock_pkgsearch, .package = "pkgsearch")
  env <- run_btw_quietly(
    "cran",
    "search",
    "string interpolation",
    "--format",
    "long",
    "-n",
    "3"
  )
  expect_equal(env$format, "long")
  expect_equal(env$n, 3L)
})

test_that("btw cran search default n is 20 for short, 5 for long", {
  mock_size <- NULL
  local_mocked_bindings(
    pkg_search = function(query, format, size) {
      mock_size <<- size
      mock_pkgsearch(query, format = format)
    },
    .package = "pkgsearch"
  )
  run_btw_quietly("cran", "search", "test query")
  expect_equal(mock_size, 20L)

  run_btw_quietly("cran", "search", "test query", "--format", "long")
  expect_equal(mock_size, 5L)
})

test_that("btw cran info shows package info", {
  local_mocked_bindings(
    cran_package = mock_cran_package,
    .package = "pkgsearch"
  )
  env <- run_btw_quietly("cran", "info", "anyflights")
  expect_equal(env$package, "anyflights")
})

# error handling ---------------------------------------------------------

test_that("btw pkg error exits with code 1 and message on stderr", {
  result <- run_btw_subprocess("docs", "news", "nonexistent_pkg_xyz")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "nonexistent_pkg_xyz", ignore.case = TRUE)
  expect_equal(trimws(result$stdout), "")
})
