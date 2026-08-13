use_latest_pandoc()

# Resolved once, up front, so tests that change the working directory
# (e.g. to exercise cwd-based dev package detection) don't break path
# resolution for subsequent `test_path()` calls.
btw_cli_path_resolved <- {
  path <- test_path("..", "..", "exec", "btw.R")
  if (file.exists(path)) normalizePath(path) else NA_character_
}
btw_pkg_dir_resolved <- normalizePath(test_path("..", ".."))

btw_cli_path <- function() {
  if (is.na(btw_cli_path_resolved)) {
    skip("btw CLI script not found")
  }
  btw_cli_path_resolved
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
  pkg_dir <- btw_pkg_dir_resolved
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


# docs topics ----------------------------------------------------------

test_that("btw docs topics <package> shows topics and vignettes", {
  env <- run_btw_quietly("docs", "topics", "stats")
  expect_equal(env$package, "stats")
  expect_equal(env$only, "")
})

test_that("btw docs topics --only help shows only help topics", {
  env <- run_btw_quietly("docs", "topics", "stats", "--only", "help")
  expect_equal(env$package, "stats")
  expect_equal(env$only, "help")
})

test_that("btw docs topics --only vignettes shows only vignettes", {
  skip_if_not_installed("dplyr")
  env <- run_btw_quietly("docs", "topics", "dplyr", "--only", "vignettes")
  expect_equal(env$package, "dplyr")
  expect_equal(env$only, "vignettes")
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

test_that("btw docs news <package> <version> reads that version", {
  local_skip_pandoc_convert_text()
  seen_version <- NULL
  local_mocked_bindings(
    btw_tool_docs_package_news_impl = function(
      package_name,
      search_term,
      version
    ) {
      seen_version <<- version
      btw_tool_result("NEWS")
    }
  )
  env <- run_btw_quietly("docs", "news", "dplyr", "v1.1.4")
  expect_equal(env$package, "dplyr")
  expect_equal(env$version, "v1.1.4")
  expect_equal(seen_version, "1.1.4")

  env <- run_btw_quietly("docs", "news", "dplyr", "1.1.4")
  expect_equal(env$version, "1.1.4")
  expect_equal(seen_version, "1.1.4")
})

test_that("btw docs news guides positional search terms to --search", {
  result <- run_btw_subprocess("docs", "news", "dplyr", "a search term")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "must be a package\\s+version")
  expect_match(
    result$stderr,
    "btw docs news dplyr --search 'a search term'",
    fixed = TRUE
  )
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

test_that("btw pkg desc prints the default DESCRIPTION fields", {
  skip_if_not_installed("desc")
  env <- run_btw_quietly("pkg", "desc", "stats", "utils")
  output <- paste(env$.output, collapse = "\n")

  expect_equal(env$`packages...`, c("stats", "utils"))
  expect_equal(env$fields, "")
  expect_false(env$json)
  expect_match(output, "^Package: stats")
  expect_match(output, "\n\n---\n\nPackage: utils")
  expect_equal(length(gregexpr("\n---\n", output, fixed = TRUE)[[1]]), 1)
  expect_match(output, "Description:")
  expect_match(output, "License:")
  expect_false(grepl("Priority:", output, fixed = TRUE))
  expect_false(grepl("Built:", output, fixed = TRUE))
})

test_that("btw pkg desc --json parses DESCRIPTION files with desc", {
  skip_if_not_installed("desc")
  env <- run_btw_quietly("pkg", "desc", "stats", "utils", "--json")
  parsed <- jsonlite::fromJSON(
    paste(env$.output, collapse = "\n"),
    simplifyVector = FALSE
  )

  expect_true(env$json)
  expect_named(parsed, c("stats", "utils"))
  expect_equal(parsed$stats$Package, "stats")
  expect_equal(parsed$utils$Package, "utils")
  expect_true(all(c("Version", "Title", "Description") %in%
    names(parsed$stats)))
  expect_false("Built" %in% names(parsed$stats))
})

test_that("btw pkg desc --fields selects fields plus package identity", {
  skip_if_not_installed("desc")
  env <- run_btw_quietly(
    "pkg",
    "desc",
    "stats",
    "--fields=priority,license",
    "--json"
  )
  parsed <- jsonlite::fromJSON(
    paste(env$.output, collapse = "\n"),
    simplifyVector = FALSE
  )$stats

  expect_equal(env$fields, "priority,license")
  expect_true(all(c("Package", "Version", "Title", "Priority", "License") %in%
    names(parsed)))
  expect_false("Imports" %in% names(parsed))
  expect_false("Built" %in% names(parsed))
})

test_that("btw pkg desc --fields=all returns the raw DESCRIPTION", {
  skip_if_not_installed("desc")
  env <- run_btw_quietly("pkg", "desc", "stats", "--fields=all")
  expected <- readLines(
    system.file("DESCRIPTION", package = "stats"),
    warn = FALSE
  )

  expect_equal(env$fields, "all")
  expect_identical(env$.output, expected)
})

test_that("btw pkg desc --fields=all --json includes every field", {
  skip_if_not_installed("desc")
  env <- run_btw_quietly("pkg", "desc", "stats", "--fields=all", "--json")
  parsed <- jsonlite::fromJSON(
    paste(env$.output, collapse = "\n"),
    simplifyVector = FALSE
  )$stats

  expect_true(all(desc::desc(package = "stats")$fields() %in% names(parsed)))
})

test_that("btw pkg desc reports packages that are not installed", {
  skip_if_not_installed("desc")
  result <- run_btw_subprocess("pkg", "desc", "completely_nonexistent_xyz_pkg")

  expect_equal(result$status, 1)
  expect_equal(result$stdout, "")
  expect_match(result$stderr, "not installed", ignore.case = TRUE)
})

test_that("btw pkg coverage calls coverage impl", {
  local_mocked_bindings(
    btw_tool_pkg_coverage_impl = function(pkg, filename = NULL) "Coverage: 80%"
  )
  env <- run_btw_quietly("pkg", "coverage")
  expect_equal(env$file, "")
})

test_that("btw pkg coverage --json outputs valid JSON", {
  mock_df <- data.frame(
    filename = c("R/foo.R", "R/bar.R"),
    coverage = c(95.5, 80.0),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    btw_tool_pkg_coverage_impl = function(pkg, filename = NULL) {
      btw:::BtwRunToolResult(
        value = "Coverage: 87%",
        extra = list(data = mock_df, code = "coverage()")
      )
    }
  )
  env <- run_btw_quietly("pkg", "coverage", "--json")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_equal(parsed$filename, c("R/foo.R", "R/bar.R"))
  expect_equal(parsed$coverage, c(95.5, 80.0))
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

test_that("btw pkg src methods parses default and source output", {
  env <- run_btw_quietly(
    "pkg",
    "src",
    "methods",
    "stats",
    "predict"
  )

  expect_equal(env$package, "stats")
  expect_equal(env$`generics...`, "predict")
  expect_false(env$source)
  expect_false(env$json)

  output <- paste(env$.output, collapse = "\n")
  expect_match(output, "| generic |", fixed = TRUE)
  expect_false(grepl("```r", output, fixed = TRUE))

  source_env <- run_btw_quietly(
    "pkg",
    "src",
    "methods",
    "stats",
    "predict",
    "--source"
  )

  expect_equal(source_env$package, "stats")
  expect_equal(source_env$`generics...`, "predict")
  expect_true(source_env$source)
  expect_false(source_env$json)

  source_output <- paste(source_env$.output, collapse = "\n")
  expect_match(source_output, "### `predict` method", fixed = TRUE)
  expect_match(source_output, "```r", fixed = TRUE)
})

test_that("btw pkg src methods --source --json returns source data", {
  env <- run_btw_quietly(
    "pkg",
    "src",
    "methods",
    "stats",
    "predict",
    "--source",
    "--json"
  )

  expect_true(env$source)
  expect_true(env$json)

  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_s3_class(parsed, "data.frame")
  expect_true(all(c("generic", "method", "type", "source") %in% names(parsed)))

  row <- parsed[parsed$method == "predict.lm", , drop = FALSE]
  expect_equal(nrow(row), 1)
  expect_equal(row$generic, "predict")
  expect_equal(row$type, "S3method")
  expect_true(nzchar(row$source))
})

# dev package auto-detection ---------------------------------------------

local_dev_package <- function(name, subdir = ".", .local_envir = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = .local_envir)
  pkg_dir <- file.path(root, subdir)
  dir.create(file.path(pkg_dir, "R"), recursive = TRUE)
  writeLines(
    c(
      paste0("Package: ", name),
      "Version: 0.0.0.1",
      "Title: Test",
      "Description: Test.",
      "License: MIT",
      "Encoding: UTF-8"
    ),
    file.path(pkg_dir, "DESCRIPTION")
  )
  writeLines(
    c("#' @export", paste0(name, '_hello <- function() "hello"')),
    file.path(pkg_dir, "R", "hello.R")
  )
  writeLines(
    c(
      "# Generated by roxygen2: do not edit by hand",
      "",
      paste0("export(", name, "_hello)")
    ),
    file.path(pkg_dir, "NAMESPACE")
  )
  withr::local_dir(root, .local_envir = .local_envir)
  withr::defer(
    if (name %in% loadedNamespaces()) {
      try(pkgload::unload(name, quiet = TRUE), silent = TRUE)
    },
    envir = .local_envir
  )
  invisible(root)
}

test_that("btw pkg src list auto-loads a dev package found in cwd", {
  skip_if_not_installed("pkgload")
  app <- btw_cli_path()
  local_dev_package("devpkgone")

  expect_message(
    output <- capture.output(
      env <- Rapp::run(app, c("pkg", "src", "list", "devpkgone"))
    ),
    "Loaded in-development package"
  )
  expect_match(paste(output, collapse = "\n"), "devpkgone_hello")
})

test_that("btw pkg src list --no-dev skips auto-loading", {
  local_dev_package("devpkgtwo")

  result <- run_btw_subprocess("pkg", "src", "list", "devpkgtwo", "--no-dev")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "devpkgtwo", ignore.case = TRUE)
})

test_that("btw pkg src get finds a dev package in an R/ subfolder", {
  skip_if_not_installed("pkgload")
  app <- btw_cli_path()
  local_dev_package("devpkgthree", subdir = "R")

  expect_message(
    output <- capture.output(
      env <- Rapp::run(
        app,
        c("pkg", "src", "get", "devpkgthree", "devpkgthree_hello")
      )
    ),
    "Loaded in-development package"
  )
  expect_match(paste(output, collapse = "\n"), "devpkgthree_hello")
})

test_that("btw pkg src list --no-dev flag defaults to FALSE", {
  env <- run_btw_quietly("pkg", "src", "list", "stats")
  expect_false(env$no_dev)
})

test_that("btw docs topics --no-dev flag defaults to FALSE", {
  env <- run_btw_quietly("docs", "topics", "stats")
  expect_false(env$no_dev)
})

# btw info deprecated ----------------------------------------------------

test_that("btw info exits 1 with deprecation message", {
  result <- run_btw_subprocess("info", "platform")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "deprecated", ignore.case = TRUE)
  expect_match(result$stderr, "system-info")
  expect_match(result$stderr, "check-installed")
  expect_match(result$stderr, "installed-packages")
})

# btw system-info --------------------------------------------------------

test_that("btw system-info shows platform info", {
  local_sessioninfo_quarto_version()
  env <- run_btw_quietly("system-info")
  expect_true(is.environment(env))
})

test_that("btw system-info --json outputs valid JSON", {
  local_sessioninfo_quarto_version()
  env <- run_btw_quietly("system-info", "--json")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_type(parsed, "list")
  expect_true("os:" %in% names(parsed))
})

# btw check-installed ----------------------------------------------------

test_that("btw check-installed calls is_package_installed for each package", {
  checked_pkgs <- character()
  local_mocked_bindings(
    btw_tool_sessioninfo_is_package_installed_impl = function(package_name) {
      checked_pkgs <<- c(checked_pkgs, package_name)
      btw:::BtwToolResult(
        value = paste(package_name, "is installed"),
        extra = list(package = package_name, version = "1.0.0")
      )
    }
  )
  run_btw_quietly("check-installed", "dplyr", "ggplot2")
  expect_equal(checked_pkgs, c("dplyr", "ggplot2"))
})

test_that("btw check-installed exits 0 when package is not installed", {
  result <- run_btw_subprocess(
    "check-installed",
    "completely_nonexistent_xyz_pkg"
  )
  expect_equal(result$status, 0)
})

test_that("btw check-installed --fail exits 1 when package is not installed", {
  result <- run_btw_subprocess(
    "check-installed",
    "completely_nonexistent_xyz_pkg",
    "--fail"
  )
  expect_equal(result$status, 1)
})

test_that("btw check-installed --fail exits 0 when all packages are installed", {
  result <- run_btw_subprocess("check-installed", "base", "--fail")
  expect_equal(result$status, 0)
})

test_that("btw check-installed --json outputs array with installed field", {
  local_mocked_bindings(
    btw_tool_sessioninfo_is_package_installed_impl = function(package_name) {
      btw:::BtwToolResult(
        value = paste(package_name, "is installed"),
        extra = list(package = package_name, version = "1.0.0")
      )
    }
  )
  env <- run_btw_quietly("check-installed", "dplyr", "ggplot2", "--json")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_equal(nrow(parsed), 2)
  expect_equal(parsed$package, c("dplyr", "ggplot2"))
  expect_true(all(parsed$installed))
})


test_that("btw check-installed --json has null version for not-installed package", {
  local_mocked_bindings(
    btw_tool_sessioninfo_is_package_installed_impl = function(package_name) {
      stop(paste("Package", package_name, "is not installed."))
    }
  )
  env <- run_btw_quietly("check-installed", "notapkg", "--json")
  parsed <- jsonlite::fromJSON(
    paste(env$.output, collapse = "\n"),
    simplifyVector = FALSE
  )
  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$package, "notapkg")
  expect_null(parsed[[1]]$version)
  expect_false(parsed[[1]]$installed)
})

# btw installed-packages -------------------------------------------------

test_that("btw installed-packages passes package names", {
  mock_pkgs <- NULL
  local_mocked_bindings(
    btw_tool_sessioninfo_package_impl = function(packages, dependencies) {
      mock_pkgs <<- packages
      "package info"
    }
  )
  run_btw_quietly("installed-packages", "dplyr", "ggplot2")
  expect_equal(mock_pkgs, c("dplyr", "ggplot2"))
})

test_that("btw installed-packages --deps passes dependency types", {
  mock_deps <- NULL
  local_mocked_bindings(
    btw_tool_sessioninfo_package_impl = function(packages, dependencies) {
      mock_deps <<- dependencies
      "package info"
    }
  )
  run_btw_quietly("installed-packages", "dplyr", "--deps", "Imports,Suggests")
  expect_equal(mock_deps, "Imports,Suggests")
})

test_that("btw installed-packages --json outputs valid JSON", {
  mock_df <- data.frame(
    package = c("dplyr", "ggplot2"),
    version = c("1.1.4", "3.5.0"),
    stringsAsFactors = FALSE
  )
  local_mocked_bindings(
    btw_tool_sessioninfo_package_impl = function(packages, dependencies) {
      btw:::BtwPackageInfoToolResult(
        value = "pkg info",
        extra = list(data = mock_df)
      )
    }
  )
  env <- run_btw_quietly("installed-packages", "dplyr", "ggplot2", "--json")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_equal(parsed$package, c("dplyr", "ggplot2"))
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

test_that("btw cran search --json outputs valid JSON", {
  local_mocked_bindings(pkg_search = mock_pkgsearch, .package = "pkgsearch")
  env <- run_btw_quietly("cran", "search", "string interpolation", "--json")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_true(is.data.frame(parsed))
  expect_equal(parsed$package, c("glue", "epoxy", "gsubfn"))
})

test_that("btw cran info --json outputs valid JSON", {
  local_mocked_bindings(
    cran_package = mock_cran_package,
    .package = "pkgsearch"
  )
  env <- run_btw_quietly("cran", "info", "anyflights", "--json")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_equal(parsed$Package, "anyflights")
  expect_equal(parsed$Version, "0.3.5")
})

test_that("btw cran versions supports text and JSON output", {
  seen_dates <- NULL
  local_mocked_bindings(
    cran_versions = function(package_name, after = NULL, before = NULL) {
      seen_dates <<- list(after = after, before = before)
      cran_versions_data(
        version = c("1.1.4", "1.1.3"),
        released = as.Date(c("2023-11-17", "2023-10-15")),
        released_at = c("2023-11-17T00:00:00Z", "2023-10-15T00:00:00Z"),
        current = c(TRUE, FALSE),
        tarball_url = c(
          "https://cran.r-project.org/src/contrib/dplyr_1.1.4.tar.gz",
          "https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.1.3.tar.gz"
        )
      )
    }
  )

  env <- run_btw_quietly("cran", "versions", "dplyr")
  expect_equal(env$package, "dplyr")
  expect_null(seen_dates$after)
  expect_null(seen_dates$before)

  env <- run_btw_quietly(
    "cran",
    "versions",
    "dplyr",
    "--after",
    "2023-01-01",
    "--before",
    "2023-12-31",
    "--json"
  )
  expect_equal(seen_dates$after, "2023-01-01")
  expect_equal(seen_dates$before, "2023-12-31")
  parsed <- jsonlite::fromJSON(paste(env$.output, collapse = "\n"))
  expect_equal(parsed$version, c("1.1.4", "1.1.3"))
  expect_equal(parsed$released, c("2023-11-17", "2023-10-15"))
  expect_equal(
    parsed$released_at,
    c("2023-11-17T00:00:00Z", "2023-10-15T00:00:00Z")
  )
  expect_equal(parsed$current, c(TRUE, FALSE))
})

# error handling ---------------------------------------------------------

test_that("btw pkg error exits with code 1 and message on stderr", {
  result <- run_btw_subprocess("docs", "news", "nonexistent_pkg_xyz")
  expect_equal(result$status, 1)
  expect_match(result$stderr, "nonexistent_pkg_xyz", ignore.case = TRUE)
  expect_equal(trimws(result$stdout), "")
})
