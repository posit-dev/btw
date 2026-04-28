#!/usr/bin/env Rapp
#| name: btw
#| description: >
#|   Describe R objects, documentation, and workspace state in LLM-friendly
#|   text. Wraps btw package tools for docs, pkg, info, and cran operations.
#| launcher:
#|   default-packages: [base, datasets, utils, stats, methods, btw]
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

btw_json_output <- function(x) {
  cat(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE, na = "null"), "\n")
}

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

btw_pkg_coverage <- function(path, file, json = FALSE) {
  result <- btw:::btw_tool_pkg_coverage_impl(
    path,
    if (has_value(file)) file else NULL
  )
  if (json) {
    data <- S7::prop(result, "extra")$data
    btw_json_output(if (!is.null(data)) data else list())
  } else {
    btw_output(result)
  }
}

btw_info_platform <- function(json = FALSE) {
  result <- btw:::btw_tool_sessioninfo_platform_impl()
  if (json) {
    btw_json_output(S7::prop(result, "extra"))
  } else {
    btw_output(result)
  }
}

btw_info_packages <- function(packages, deps, check, json = FALSE) {
  pkgs <- packages
  if (check && length(pkgs) > 0) {
    if (json) {
      results <- lapply(pkgs, function(pkg) {
        result <- btw:::btw_tool_sessioninfo_is_package_installed_impl(pkg)
        S7::prop(result, "extra")
      })
      btw_json_output(results)
    } else {
      for (pkg in pkgs) {
        btw_output(btw:::btw_tool_sessioninfo_is_package_installed_impl(pkg))
      }
    }
  } else {
    if (length(pkgs) == 0) {
      pkgs <- "attached"
    }
    deps_val <- if (has_value(deps)) deps else ""
    result <- btw:::btw_tool_sessioninfo_package_impl(pkgs, deps_val)
    if (json) {
      btw_json_output(S7::prop(result, "extra")$data)
    } else {
      btw_output(result)
    }
  }
}

btw_cran_search <- function(query, format, n, json = FALSE) {
  size <- if (!is.na(n)) {
    n
  } else if (format == "long") {
    5L
  } else {
    20L
  }
  result <- pkgsearch::pkg_search(query, format = format, size = size)
  if (json) {
    df <- as.data.frame(result)
    df[] <- lapply(df, function(col) {
      if (inherits(col, "numeric_version")) as.character(col) else col
    })
    btw_json_output(df)
  } else {
    btw_output(btw_this(result, for_tool_use = TRUE))
  }
}

btw_skills_install <- function(source, skill, scope, overwrite) {
  skill_val <- if (has_value(skill)) skill else NULL
  scope_val <- if (has_value(scope)) scope else "project"
  overwrite_val <- if (is.na(overwrite)) NULL else overwrite

  if (grepl("/", source, fixed = TRUE)) {
    btw_skill_install_github(
      source,
      skill = skill_val,
      scope = scope_val,
      overwrite = overwrite_val
    )
  } else {
    btw_skill_install_package(
      source,
      skill = skill_val,
      scope = scope_val,
      overwrite = overwrite_val
    )
  }
}

btw_cran_info <- function(package, json = FALSE) {
  result <- pkgsearch::cran_package(package)
  if (json) {
    btw_json_output(unclass(result))
  } else {
    btw_output(btw_this(result))
  }
}

# Subcommand dispatch ---------------------------------------------------------

switch(
  group <- "",

  #| title: Access R documentation
  docs = {
    switch(
      docs_cmd <- "",

      #| title: Show help for a topic or package
      help = {
        #| description: Help topic, package name, or {package} for package listing.
        topic <- NULL
        #| description: Package name to scope the help topic.
        #| short: 'p'
        package <- ""

        tryCatch(btw_docs_help(topic, package), error = btw_error)
      },

      #| title: Read a package vignette
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

      #| title: Show package NEWS
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

  #| title: Work with an R package under development
  pkg = {
    #| description: Path to package directory.
    path <- "."

    switch(
      pkg_cmd <- "",

      #| title: Generate package documentation
      document = {
        tryCatch(btw_pkg_document(path), error = btw_error)
      },

      #| title: Run R CMD check
      check = {
        tryCatch(btw_pkg_check(path), error = btw_error)
      },

      #| title: Run package tests
      test = {
        #| description: Regex to filter test files.
        #| short: 'f'
        filter <- ""
        tryCatch(btw_pkg_test(path, filter), error = btw_error)
      },

      #| title: Load package with pkgload
      load = {
        tryCatch(btw_pkg_load(path), error = btw_error)
      },

      #| title: Measure package test coverage
      coverage = {
        #| description: Filename for line-level coverage details.
        file <- ""
        #| description: Output as JSON.
        json <- FALSE
        tryCatch(btw_pkg_coverage(path, file, json), error = btw_error)
      }
    )
    if (pkg_cmd == "") btw_self_help("pkg")
  },

  #| title: Inspect the R session and environment
  info = {
    #| description: Output as JSON.
    json <- FALSE

    switch(
      info_cmd <- "",

      #| title: Show platform and session info
      platform = {
        tryCatch(btw_info_platform(json), error = btw_error)
      },

      #| title: Show installed package information
      packages = {
        #| description: Package names to query.
        `packages...` <- c()
        #| description: Dependency types to include.
        deps <- ""
        #| description: Check if packages are installed.
        #| short: 'c'
        check <- FALSE

        tryCatch(
          btw_info_packages(`packages...`, deps, check, json),
          error = btw_error
        )
      }
    )
    if (info_cmd == "") btw_self_help("info")
  },

  #| title: Query CRAN package metadata
  cran = {
    #| description: Output as JSON.
    json <- FALSE

    switch(
      cran_cmd <- "",

      #| title: Search CRAN for packages
      search = {
        #| description: Search query.
        query <- NULL
        #| description: Result format (short or long).
        format <- "short"
        #| description: Number of results.
        #| short: 'n'
        n <- NA_integer_

        tryCatch(btw_cran_search(query, format, n, json), error = btw_error)
      },

      #| title: Show CRAN metadata for a package
      info = {
        #| description: Package name.
        package <- NULL
        tryCatch(btw_cran_info(package, json), error = btw_error)
      }
    )
    if (cran_cmd == "") btw_self_help("cran")
  },

  #| title: Manage btw skills
  skills = {
    switch(
      skills_cmd <- "",

      #| title: Install a skill from a package or GitHub repository
      install = {
        #| description: Package name (e.g. "btw") or GitHub repo spec (e.g. "posit-dev/btw").
        source <- NULL
        #| description: Skill name to install (if the source has multiple skills).
        #| short: 's'
        skill <- ""
        #| description: Scope for installation ("project" or "user").
        scope <- "project"
        #| description: Overwrite an existing skill if it is already installed.
        overwrite <- FALSE

        tryCatch(
          btw_skills_install(source, skill, scope, overwrite),
          error = btw_error
        )
      }
    )
    if (skills_cmd == "") btw_self_help("skills")
  },

  #| title: Run btw_app() in the current directory
  app = {
    #| description: The client (provider/model string) to use
    client <- ""

    #| description: Comma-separated list of btw tools to enable in the app (by name or group).
    tools <- ""

    #| description: Path to a `btw.md` file.
    path_btw <- ""

    tools <- if (has_value(tools)) {
      strsplit(tools, ",", fixed = TRUE)[[1]]
    }

    btw_app(
      client = if (has_value(client)) client,
      tools = tools,
      path_btw = if (has_value(path_btw)) path_btw
    )
  }
)

if (group == "") {
  btw_self_help()
}
