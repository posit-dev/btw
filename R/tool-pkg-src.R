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

btw_tool_pkg_src_list_impl <- function(package, all = FALSE) {
  check_string(package)
  check_bool(all)

  resolved <- btw_pkg_src_resolve_ns(package)
  ns <- resolved$ns

  names <- btw_pkg_src_namespace_objects(ns, all = all)
  names <- sort(names)

  rows <- lapply(names, function(name) {
    x <- get(name, envir = ns)
    type <- btw_pkg_src_classify(name, x, ns)
    loc <- btw_pkg_src_srcref_location(x)

    data.frame(
      name = name,
      type = type,
      path = loc$path,
      line = loc$line,
      stringsAsFactors = FALSE
    )
  })

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
