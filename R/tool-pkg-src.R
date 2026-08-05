#' @include tool-result.R
NULL

# Source inspection tools for exploring package internals -----------------------
#
# The dev package (`.`) is loaded in-process via `pkgload::load_all()` because
# the btw CLI process is ephemeral, so `load_all()` side effects don't matter.
# IF these `_impl` functions are ever registered as ellmer tools running in a
# persistent session, `.` handling must move to an isolated `callr` process
# (as `btw_tool_pkg_load_all` does) to avoid mutating the user's global
# session.

btw_pkg_src_resolve_ns <- function(package) {
  check_string(package)

  if (identical(package, ".")) {
    check_installed("pkgload")
    pkgload::load_all(".", export_all = FALSE, quiet = TRUE)
    name <- pkgload::pkg_name(".")
  } else {
    loadNamespace(package)
    name <- package
  }

  list(ns = asNamespace(name), name = name)
}

btw_pkg_src_namespace_objects <- function(ns, all = FALSE) {
  if (all) {
    ls(ns, all.names = TRUE)
  } else {
    getNamespaceExports(ns)
  }
}

btw_pkg_src_classify <- function(name, x, ns) {
  if (is.function(x)) {
    if (methods::isGeneric(name, where = ns)) {
      return("S4generic")
    }
    return("function")
  }

  if (methods::is(x, "classRepresentation")) {
    return("S4class")
  }

  class_def <- tryCatch(
    methods::getClassDef(name, where = ns),
    error = function(e) NULL
  )
  if (!is.null(class_def)) {
    return("S4class")
  }

  if (inherits(x, "R6ClassGenerator")) {
    return("R6generator")
  }

  if (is.data.frame(x) || is.atomic(x) || is.list(x)) {
    return("data")
  }

  "other"
}

btw_pkg_src_srcref_location <- function(x) {
  srcref <- utils::getSrcref(x)
  if (is.null(srcref)) {
    srcref <- attr(x, "srcref")
  }

  if (is.null(srcref)) {
    return(list(path = NA_character_, line = NA_integer_))
  }

  srcfile <- attr(srcref, "srcfile")
  path <- srcfile$filename

  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(list(path = NA_character_, line = NA_integer_))
  }

  line <- tryCatch(srcref[1], error = function(e) NA_integer_)

  list(path = path, line = as.integer(line))
}

# Describe a single namespace object as a `{name, type, path, line}` row.
# Forcing an object (to classify it) can error on pathological bindings
# (active bindings, promises that error); degrade to a bare `other` row
# instead of aborting the whole listing.
btw_pkg_src_describe <- function(name, ns) {
  tryCatch(
    {
      x <- get(name, envir = ns, inherits = FALSE)
      type <- btw_pkg_src_classify(name, x, ns)
      loc <- btw_pkg_src_srcref_location(x)
      data.frame(
        name = name,
        type = type,
        path = loc$path,
        line = loc$line,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      data.frame(
        name = name,
        type = "other",
        path = NA_character_,
        line = NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  )
}

# Drop columns that are entirely NA. On binary installs `path`/`line` are
# always NA (no srcref on disk), so this keeps both the table and `--json`
# free of dead columns. Left untouched for an empty frame so the full schema
# is preserved when there are no objects.
btw_pkg_src_drop_na_columns <- function(data) {
  if (nrow(data) == 0) {
    return(data)
  }
  keep <- !vapply(data, function(col) all(is.na(col)), logical(1))
  data[, keep, drop = FALSE]
}

btw_tool_pkg_src_list_impl <- function(package, all = FALSE) {
  check_string(package)
  check_bool(all)

  resolved <- btw_pkg_src_resolve_ns(package)
  ns <- resolved$ns

  names <- btw_pkg_src_namespace_objects(ns, all = all)
  names <- sort(names)

  rows <- lapply(names, btw_pkg_src_describe, ns = ns)

  data <- if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(
      name = character(),
      type = character(),
      path = character(),
      line = integer(),
      stringsAsFactors = FALSE
    )
  }
  rownames(data) <- NULL
  data <- btw_pkg_src_drop_na_columns(data)

  value <- if (nrow(data) > 0) md_table(data) else "No objects found."

  btw_tool_result(value = value, data = data)
}

btw_pkg_src_has_source_tree <- function(r_dir) {
  if (!dir.exists(r_dir)) {
    return(FALSE)
  }

  files <- list.files(r_dir, pattern = "\\.[Rr]$")
  length(files) > 0
}

# Resolve a package's install path + whether real R source is on disk.
# Pure lookup: no namespace load (`pkgload::pkg_path()` reads DESCRIPTION
# without loading), so callers that also resolve the namespace don't pay for
# a redundant `load_all()`/`loadNamespace()`.
btw_pkg_src_path_info <- function(package) {
  if (identical(package, ".")) {
    check_installed("pkgload")
    list(path = pkgload::pkg_path("."), source_available = TRUE)
  } else {
    path <- find.package(package)
    list(
      path = path,
      source_available = btw_pkg_src_has_source_tree(file.path(path, "R"))
    )
  }
}

btw_tool_pkg_src_path_impl <- function(packages) {
  check_character(packages)

  if (length(packages) == 0) {
    cli::cli_abort("`packages` must contain at least one package name.")
  }

  rows <- lapply(packages, function(package) {
    info <- btw_pkg_src_path_info(package)
    data.frame(
      package = package,
      path = info$path,
      source_available = info$source_available,
      stringsAsFactors = FALSE
    )
  })

  data <- do.call(rbind, rows)
  rownames(data) <- NULL

  btw_tool_result(value = md_table(data), data = data)
}

btw_pkg_src_render_closure <- function(x) {
  # S4 generics/methods carry a lot of metadata as attributes; printing them
  # directly dumps all of that. Rebuild a plain closure (preserving srcref
  # when present) so we only render the function itself.
  if (methods::is(x, "genericFunction") || methods::is(x, "MethodDefinition")) {
    plain <- function() NULL
    formals(plain) <- formals(x)
    body(plain) <- body(x)
    environment(plain) <- environment(x)
    attr(plain, "srcref") <- attr(x, "srcref")
    x <- plain
  }

  lines <- utils::capture.output(print(x))
  lines <- lines[!grepl("^<(bytecode|environment): ", lines)]
  paste(lines, collapse = "\n")
}

btw_pkg_src_render_str <- function(x) {
  lines <- utils::capture.output(
    utils::str(x, max.level = 1, list.len = 10)
  )
  paste(lines, collapse = "\n")
}

btw_pkg_src_render_s4generic <- function(name, x, ns) {
  default_method <- tryCatch(
    methods::getMethod(name, "ANY", where = ns),
    error = function(e) NULL
  )
  if (is.null(default_method)) {
    default_method <- tryCatch(
      methods::selectMethod(name, "ANY"),
      error = function(e) NULL
    )
  }

  if (!is.null(default_method)) {
    return(btw_pkg_src_render_closure(default_method))
  }

  # No default/standard method readily available; fall back to the
  # generic's own standardGeneric() closure.
  btw_pkg_src_render_closure(x)
}

btw_pkg_src_render_s4class <- function(name, ns) {
  class_def <- tryCatch(
    methods::getClassDef(name, where = ns),
    error = function(e) NULL
  )
  if (is.null(class_def)) {
    return(NA_character_)
  }
  paste(
    utils::capture.output(utils::str(class_def, max.level = 2)),
    collapse = "\n"
  )
}

btw_pkg_src_render_source <- function(name, x, type, ns) {
  switch(
    type,
    "function" = btw_pkg_src_render_closure(x),
    "S4generic" = btw_pkg_src_render_s4generic(name, x, ns),
    "S4class" = btw_pkg_src_render_s4class(name, ns),
    btw_pkg_src_render_str(x)
  )
}

btw_tool_pkg_src_get_impl <- function(package, objects) {
  check_string(package)
  check_character(objects)

  if (length(objects) == 0) {
    cli::cli_abort("`objects` must contain at least one object name.")
  }

  resolved <- btw_pkg_src_resolve_ns(package)
  ns <- resolved$ns

  rows <- lapply(objects, function(name) {
    if (!exists(name, envir = ns, inherits = FALSE)) {
      return(data.frame(
        name = name,
        type = NA_character_,
        path = NA_character_,
        line = NA_integer_,
        source = "Object not found in namespace.",
        stringsAsFactors = FALSE
      ))
    }

    x <- get(name, envir = ns, inherits = FALSE)
    type <- btw_pkg_src_classify(name, x, ns)
    loc <- btw_pkg_src_srcref_location(x)
    source <- btw_pkg_src_render_source(name, x, type, ns)

    data.frame(
      name = name,
      type = type,
      path = loc$path,
      line = loc$line,
      source = source,
      stringsAsFactors = FALSE
    )
  })

  data <- do.call(rbind, rows)
  rownames(data) <- NULL

  blocks <- lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, ]

    if (is.na(row$type)) {
      return(c(paste0("### `", row$name, "`"), "", row$source))
    }

    header <- paste0("### `", row$name, "` (", row$type, ")")
    loc <- if (!is.na(row$path)) paste0("`", row$path, ":", row$line, "`")

    code_type <- if (row$type %in% c("function", "S4generic")) "r" else ""

    c(header, loc, "", md_code_block(code_type, row$source))
  })

  value <- paste(
    vapply(blocks, paste, character(1), collapse = "\n"),
    collapse = "\n\n"
  )

  btw_tool_result(value = value, data = data)
}

btw_pkg_src_method_row <- function(
  generic,
  method,
  class,
  type,
  fn,
  source = FALSE
) {
  loc <- btw_pkg_src_srcref_location(fn)
  row <- data.frame(
    generic = generic,
    method = method,
    class = class,
    type = type,
    path = loc$path,
    line = loc$line,
    stringsAsFactors = FALSE
  )
  if (source) {
    row$source <- btw_pkg_src_render_closure(fn)
  }
  row
}

btw_pkg_src_is_namespace_function <- function(fn, ns) {
  is.function(fn) && identical(environment(fn), ns)
}

btw_pkg_src_s3_registered_methods <- function(generic, ns, source = FALSE) {
  registrations <- tryCatch(
    getNamespaceInfo(ns, "S3methods"),
    error = function(e) NULL
  )
  if (is.null(registrations) || length(registrations) == 0) {
    return(NULL)
  }

  registrations <- as.data.frame(registrations, stringsAsFactors = FALSE)
  if (ncol(registrations) < 3) {
    return(NULL)
  }

  registrations <- registrations[
    !is.na(registrations[[1]]) & registrations[[1]] == generic,
    ,
    drop = FALSE
  ]
  if (nrow(registrations) == 0) {
    return(NULL)
  }

  rows <- lapply(seq_len(nrow(registrations)), function(i) {
    class <- registrations[[2]][[i]]
    method <- registrations[[3]][[i]]

    if (is.character(method) && length(method) == 1 && !is.na(method)) {
      if (!nzchar(method)) {
        return(NULL)
      }

      fn <- tryCatch(
        get(method, envir = ns, inherits = FALSE),
        error = function(e) NULL
      )
      method_name <- method
    } else if (btw_pkg_src_is_namespace_function(method, ns)) {
      fn <- method
      candidate <- paste(generic, class, sep = ".")
      candidate_fn <- tryCatch(
        get(candidate, envir = ns, inherits = FALSE),
        error = function(e) NULL
      )
      method_name <- if (identical(fn, candidate_fn)) {
        candidate
      } else {
        NA_character_
      }
    } else {
      return(NULL)
    }

    if (!is.function(fn)) {
      return(NULL)
    }

    btw_pkg_src_method_row(
      generic,
      method_name,
      class,
      "S3method",
      fn,
      source = source
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    return(NULL)
  }
  data <- do.call(rbind, rows)
  data[!duplicated(data$class), , drop = FALSE]
}

btw_pkg_src_s3_runtime_methods <- function(generic, ns, source = FALSE) {
  s3_table <- tryCatch(
    get(".__S3MethodsTable__.", envir = ns, inherits = FALSE),
    error = function(e) NULL
  )
  if (is.null(s3_table)) {
    return(NULL)
  }

  prefix <- paste0(generic, ".")
  keys <- ls(s3_table, all.names = TRUE)
  keys <- keys[startsWith(keys, prefix)]
  if (length(keys) == 0) {
    return(NULL)
  }

  rows <- lapply(keys, function(key) {
    fn <- tryCatch(get(key, envir = s3_table), error = function(e) NULL)
    if (!btw_pkg_src_is_namespace_function(fn, ns)) {
      return(NULL)
    }

    btw_pkg_src_method_row(
      generic,
      key,
      substring(key, nchar(prefix) + 1),
      "S3method",
      fn,
      source = source
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    return(NULL)
  }
  do.call(rbind, rows)
}

# Enumerate the S3 and S4 methods of one generic within a namespace.
# S3 methods declared by the package live in `S3methods` namespace metadata,
# including registrations for generics owned by another package. The runtime
# registry supplements declarations made dynamically during package loading.
# S4 methods come from `methods::findMethods()`; they aren't reachable by a
# simple name, so `method` is NA and the signature lives in `class`.
btw_pkg_src_methods_for <- function(generic, ns, source = FALSE) {
  rows <- list()

  s3_registered <- btw_pkg_src_s3_registered_methods(
    generic,
    ns,
    source = source
  )
  if (!is.null(s3_registered)) {
    rows[[length(rows) + 1]] <- s3_registered
  }

  s3_runtime <- btw_pkg_src_s3_runtime_methods(
    generic,
    ns,
    source = source
  )
  if (!is.null(s3_runtime)) {
    if (!is.null(s3_registered)) {
      registered_classes <- s3_registered$class
      s3_runtime <- s3_runtime[!s3_runtime$class %in% registered_classes, ]
    }
    if (nrow(s3_runtime) > 0) {
      rows[[length(rows) + 1]] <- s3_runtime
    }
  }

  is_s4 <- isTRUE(tryCatch(
    methods::isGeneric(generic, where = ns),
    error = function(e) FALSE
  ))
  if (is_s4) {
    s4 <- tryCatch(
      methods::findMethods(generic, where = ns),
      error = function(e) NULL
    )
    for (i in seq_along(s4)) {
      fn <- s4[[i]]
      if (is.function(fn)) {
        rows[[length(rows) + 1]] <- btw_pkg_src_method_row(
          generic,
          NA_character_,
          names(s4)[i],
          "S4method",
          fn,
          source = source
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(NULL)
  }
  data <- do.call(rbind, rows)
  rownames(data) <- NULL
  data
}

btw_pkg_src_methods_source_value <- function(data) {
  blocks <- lapply(seq_len(nrow(data)), function(i) {
    row <- data[i, ]
    name <- if (identical(row$type, "S3method")) row$method else row$class
    header <- paste0(
      "### `",
      row$generic,
      "` method for `",
      name,
      "` (",
      row$type,
      ")"
    )
    loc <- if ("path" %in% names(row) && !is.na(row$path)) {
      paste0("`", row$path, ":", row$line, "`")
    } else {
      character()
    }

    c(header, loc, "", md_code_block("r", row$source))
  })

  paste(
    vapply(blocks, paste, character(1), collapse = "\n"),
    collapse = "\n\n"
  )
}

btw_tool_pkg_src_methods_impl <- function(package, generics, source = FALSE) {
  check_string(package)
  check_character(generics)
  check_bool(source)

  if (length(generics) == 0) {
    cli::cli_abort("`generics` must contain at least one generic name.")
  }

  resolved <- btw_pkg_src_resolve_ns(package)
  ns <- resolved$ns

  rows <- lapply(generics, btw_pkg_src_methods_for, ns = ns, source = source)
  rows <- rows[!vapply(rows, is.null, logical(1))]

  data <- if (length(rows) > 0) {
    do.call(rbind, rows)
  } else {
    data.frame(
      generic = character(),
      method = character(),
      class = character(),
      type = character(),
      path = character(),
      line = integer(),
      source = character(),
      stringsAsFactors = FALSE
    )
  }
  if (!source) {
    data$source <- NULL
  }
  rownames(data) <- NULL
  data <- btw_pkg_src_drop_na_columns(data)

  value <- if (nrow(data) == 0) {
    "No methods found."
  } else if (source) {
    btw_pkg_src_methods_source_value(data)
  } else {
    md_table(data)
  }

  btw_tool_result(value = value, data = data)
}

btw_pkg_src_materialize_dir <- function(ns) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  names <- sort(btw_pkg_src_namespace_objects(ns, all = TRUE))
  mapping <- list()

  for (i in seq_along(names)) {
    name <- names[[i]]
    # Deparsed source loses original comments/formatting and has no
    # original line numbers; each object is written to its own file so
    # search results can be mapped back to the exact object name.
    # Skip objects that error when forced (active bindings, erroring
    # promises) rather than aborting the whole materialization.
    tryCatch(
      {
        x <- get(name, envir = ns, inherits = FALSE)
        type <- btw_pkg_src_classify(name, x, ns)
        source <- btw_pkg_src_render_source(name, x, type, ns)

        # Objects with no renderable source (e.g. an S4 class whose def
        # can't be resolved) would otherwise write the literal "NA".
        if (!(length(source) == 1 && is.na(source))) {
          filename <- sprintf("%06d.R", i)
          file <- file.path(dir, filename)
          writeLines(source, file)
          mapping[[length(mapping) + 1]] <- data.frame(
            filename = filename,
            name = name,
            stringsAsFactors = FALSE
          )
        }
      },
      error = function(e) NULL
    )
  }

  mapping <- if (length(mapping) > 0) {
    do.call(rbind, mapping)
  } else {
    data.frame(filename = character(), name = character())
  }

  list(dir = dir, mapping = mapping)
}

btw_tool_pkg_src_search_impl <- function(
  package,
  terms,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE
) {
  check_string(package)
  check_character(terms)

  if (length(terms) == 0) {
    cli::cli_abort("`terms` must contain at least one search term.")
  }

  check_installed("duckdb")
  check_installed("DBI")

  path_info <- btw_pkg_src_path_info(package)

  # Real R source is searched in place; binary installs are materialized as
  # deparsed source in a temp dir. `materialized` flags the latter so we can
  # replace the transient temp paths with plain object names below.
  materialized <- !path_info$source_available
  if (materialized) {
    resolved <- btw_pkg_src_resolve_ns(package)
    materialized_sources <- btw_pkg_src_materialize_dir(resolved$ns)
    search_dir <- materialized_sources$dir
  } else {
    search_dir <- file.path(path_info$path, "R")
  }

  search_fn <- btw_tool_files_search_factory(
    path = search_dir,
    restrict_to_wd = FALSE
  )

  results <- lapply(terms, function(term) {
    res <- search_fn(
      term,
      limit = limit,
      case_sensitive = case_sensitive,
      use_regex = use_regex,
      show_lines = TRUE
    )
    data <- S7::prop(res, "value")
    if (nrow(data) > 0) {
      data$term <- term
    } else {
      data$term <- character()
    }
    data
  })

  data <- do.call(rbind, results)
  rownames(data) <- NULL

  if (materialized) {
    # Deparsed source lives in a temp dir that is removed when this call
    # returns, so the paths aren't readable. Surface the object name instead
    # so the agent uses `pkg src get` rather than trying to read a stale temp
    # path. Never reverse a filesystem-safe filename: an explicit mapping
    # preserves names such as `[<-.vctrs_vctr` and prevents collisions.
    data$filename <- unname(
      materialized_sources$mapping$name[
        match(basename(data$filename), materialized_sources$mapping$filename)
      ]
    )
  }

  max_display <- 20L
  value <- if (nrow(data) == 0) {
    "No matches found."
  } else {
    paste0(
      md_table(utils::head(data, max_display)),
      if (nrow(data) > max_display) {
        paste0("\n\n... and ", nrow(data) - max_display, " more matches.")
      }
    )
  }

  btw_tool_result(value = value, data = data)
}
