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

btw_tool_pkg_src_path_impl <- function(packages) {
  check_character(packages)

  if (length(packages) == 0) {
    cli::cli_abort("`packages` must contain at least one package name.")
  }

  rows <- lapply(packages, function(package) {
    if (identical(package, ".")) {
      check_installed("pkgload")
      pkgload::load_all(".", export_all = FALSE, quiet = TRUE)
      path <- pkgload::pkg_path(".")
      source_available <- TRUE
    } else {
      path <- find.package(package)
      source_available <- btw_pkg_src_has_source_tree(file.path(path, "R"))
    }

    data.frame(
      package = package,
      path = path,
      source_available = source_available,
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

btw_pkg_src_materialize_dir <- function(package) {
  resolved <- btw_pkg_src_resolve_ns(package)
  ns <- resolved$ns

  dir <- withr::local_tempdir(.local_envir = parent.frame())
  names <- sort(btw_pkg_src_namespace_objects(ns, all = TRUE))

  for (name in names) {
    # Deparsed source loses original comments/formatting and has no
    # original line numbers; each object is written to its own file so
    # search results still carry meaningful filenames/line numbers.
    # Skip objects that error when forced (active bindings, erroring
    # promises) rather than aborting the whole materialization.
    tryCatch(
      {
        x <- get(name, envir = ns, inherits = FALSE)
        type <- btw_pkg_src_classify(name, x, ns)
        source <- btw_pkg_src_render_source(name, x, type, ns)

        file <- file.path(dir, paste0(fs::path_sanitize(name), ".R"))
        writeLines(source, file)
      },
      error = function(e) NULL
    )
  }

  dir
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

  path_info <- btw_tool_pkg_src_path_impl(package)
  path_data <- S7::prop(path_info, "extra")$data

  search_dir <- if (path_data$source_available) {
    file.path(path_data$path, "R")
  } else {
    btw_pkg_src_materialize_dir(package)
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

  value <- if (nrow(data) > 0) md_table(data) else "No matches found."

  btw_tool_result(value = value, data = data)
}
