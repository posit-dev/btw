#!/usr/bin/env Rapp
#| name: btw
#| description: >
#|   Describe R objects, documentation, and workspace state in LLM-friendly
#|   text. Wraps btw package tools for docs, pkg, info, and cran operations.

library(btw)
library(utils)

# Global options --------------------------------------------------------------

#| description: Print btw version and exit.
version <- FALSE

if (version) {
  cat(format(utils::packageVersion("btw")), "\n")
  quit(status = 0)
}

# Helpers ---------------------------------------------------------------------

has_value <- function(x) !is.na(x) && nzchar(x)

btw_output <- function(x) {
  if (is.character(x)) {
    # pass through
  } else if (S7::S7_inherits(x)) {
    x <- S7::prop(x, "value")
  } else {
    stop("Unexpected output type: ", class(x)[[1]], call. = FALSE)
  }
  cat(paste(x, collapse = "\n"), "\n")
}

btw_error <- function(e) {
  msg <- conditionMessage(e)
  msg <- cli::ansi_strip(msg)
  cat(msg, "\n", file = stderr())
  quit(status = 1)
}

btw_self_help <- function(...) {
  script_path <- commandArgs(TRUE)[1]
  Rapp:::print_app_help(script_path, yaml = FALSE, command_path = c(...))
  quit(status = 1)
}

# Command implementations -----------------------------------------------------

btw_docs_help <- function(topic, package) {
  if (grepl("::", topic, fixed = TRUE)) {
    parts <- strsplit(topic, "::", fixed = TRUE)[[1]]
    if (has_value(package)) {
      warning(
        "Ignoring --package flag; using package from ",
        topic,
        " syntax",
        call. = FALSE
      )
    }
    btw_output(btw_this(btw:::as_btw_docs_topic(parts[1], parts[2])))
  } else if (grepl("^\\{.+\\}$", topic)) {
    pkg_name <- sub("^\\{(.+)\\}$", "\\1", topic)
    btw_output(btw_this(btw:::as_btw_docs_package(pkg_name)))
  } else if (has_value(package)) {
    btw_output(btw_this(btw:::as_btw_docs_topic(package, topic)))
  } else {
    result <- tryCatch(
      btw_this(btw:::as_btw_docs_topic(NULL, topic)),
      error = function(e) NULL
    )
    if (is.null(result)) {
      btw_output(btw_this(btw:::as_btw_docs_package(topic)))
    } else {
      btw_output(result)
    }
  }
}

btw_docs_vignette <- function(package, name, list) {
  if (list) {
    btw_output(btw_this(utils::vignette(package = package)))
  } else if (has_value(name)) {
    btw_output(btw_this(utils::vignette(name, package = package)))
  } else {
    result <- tryCatch(
      btw_this(utils::vignette(package, package = package)),
      warning = function(w) NULL,
      error = function(e) NULL
    )
    if (is.null(result)) {
      cat(
        "No introductory vignette found for",
        package,
        "-- available vignettes:\n\n",
        file = stderr()
      )
      btw_output(btw_this(utils::vignette(package = package)))
    } else {
      btw_output(result)
    }
  }
}

btw_docs_news <- function(package, search) {
  search_term <- if (has_value(search)) search else ""
  btw_output(btw:::btw_tool_docs_package_news_impl(package, search_term))
}

btw_pkg_document <- function(path) {
  btw_output(btw:::btw_tool_pkg_document_impl(path))
}

btw_pkg_check <- function(path) {
  btw_output(btw:::btw_tool_pkg_check_impl(path))
}

btw_pkg_test <- function(path, filter) {
  btw_output(
    btw:::btw_tool_pkg_test_impl(
      path,
      if (has_value(filter)) filter else NULL
    )
  )
}

btw_pkg_load <- function(path) {
  btw_output(btw:::btw_tool_pkg_load_all_impl(path))
}

btw_pkg_coverage <- function(path, file) {
  btw_output(
    btw:::btw_tool_pkg_coverage_impl(
      path,
      if (has_value(file)) file else NULL
    )
  )
}

btw_info_platform <- function() {
  btw_output(btw:::btw_tool_sessioninfo_platform_impl())
}

btw_info_packages <- function(packages, deps, check) {
  pkgs <- packages
  if (check && length(pkgs) > 0) {
    for (pkg in pkgs) {
      btw_output(btw:::btw_tool_sessioninfo_is_package_installed_impl(pkg))
    }
  } else {
    if (length(pkgs) == 0) {
      pkgs <- "attached"
    }
    deps_val <- if (has_value(deps)) deps else ""
    btw_output(btw:::btw_tool_sessioninfo_package_impl(pkgs, deps_val))
  }
}

btw_cran_search <- function(query, format, n) {
  size <- if (!is.na(n)) {
    n
  } else if (format == "long") {
    5L
  } else {
    20L
  }
  result <- pkgsearch::pkg_search(query, format = format, size = size)
  btw_output(btw_this(result, for_tool_use = TRUE))
}

btw_cran_info <- function(package) {
  btw_output(btw_this(pkgsearch::cran_package(package)))
}

# Subcommand dispatch ---------------------------------------------------------

switch(
  group <- "",

  # docs ----
  docs = {
    switch(
      docs_cmd <- "",

      # docs help ----
      help = {
        #| description: Help topic, package name, or {package} for package listing.
        topic <- NULL
        #| description: Package name to scope the help topic.
        #| short: 'p'
        package <- ""

        tryCatch(btw_docs_help(topic, package), error = btw_error)
      },

      # docs vignette ----
      vignette = {
        #| description: Package name.
        package <- NULL
        #| description: Vignette name.
        #| short: 'n'
        name <- ""
        #| description: List available vignettes.
        #| short: 'l'
        list <- FALSE

        tryCatch(btw_docs_vignette(package, name, list), error = btw_error)
      },

      # docs news ----
      news = {
        #| description: Package name.
        package <- NULL
        #| description: Search term to filter NEWS entries.
        #| short: 's'
        search <- ""

        tryCatch(btw_docs_news(package, search), error = btw_error)
      }
    )
    if (docs_cmd == "") btw_self_help("docs")
  },

  # pkg ----
  pkg = {
    #| description: Path to package directory.
    path <- "."

    switch(
      pkg_cmd <- "",

      # pkg document ----
      document = {
        tryCatch(btw_pkg_document(path), error = btw_error)
      },

      # pkg check ----
      check = {
        tryCatch(btw_pkg_check(path), error = btw_error)
      },

      # pkg test ----
      test = {
        #| description: Regex to filter test files.
        #| short: 'f'
        filter <- ""
        tryCatch(btw_pkg_test(path, filter), error = btw_error)
      },

      # pkg load ----
      load = {
        tryCatch(btw_pkg_load(path), error = btw_error)
      },

      # pkg coverage ----
      coverage = {
        #| description: Filename for line-level coverage details.
        file <- ""
        tryCatch(btw_pkg_coverage(path, file), error = btw_error)
      }
    )
    if (pkg_cmd == "") btw_self_help("pkg")
  },

  # info ----
  info = {
    switch(
      info_cmd <- "",

      # info platform ----
      platform = {
        tryCatch(btw_info_platform(), error = btw_error)
      },

      # info packages ----
      packages = {
        #| description: Package names to query.
        `packages...` <- c()
        #| description: Dependency types to include.
        deps <- ""
        #| description: Check if packages are installed.
        #| short: 'c'
        check <- FALSE

        tryCatch(
          btw_info_packages(`packages...`, deps, check),
          error = btw_error
        )
      }
    )
    if (info_cmd == "") btw_self_help("info")
  },

  # cran ----
  cran = {
    switch(
      cran_cmd <- "",

      # cran search ----
      search = {
        #| description: Search query.
        query <- NULL
        #| description: Result format (short or long).
        format <- "short"
        #| description: Number of results.
        #| short: 'n'
        n <- NA_integer_

        tryCatch(btw_cran_search(query, format, n), error = btw_error)
      },

      # cran info ----
      info = {
        #| description: Package name.
        package <- NULL
        tryCatch(btw_cran_info(package), error = btw_error)
      }
    )
    if (cran_cmd == "") btw_self_help("cran")
  }
)

if (group == "") {
  btw_self_help()
}
