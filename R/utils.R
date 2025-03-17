# ad-hoc check functions ------------------------------------------------------
check_inherits <- function(
  x,
  class,
  x_arg = caller_arg(x),
  call = caller_env()
) {
  if (!inherits(x, class)) {
    cli::cli_abort(
      "{.arg {x_arg}} must be a {.cls {class}}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  invisible(NULL)
}

# rlang's version prompts when interactive
check_installed <- function(x, call = caller_env()) {
  if (!is_installed(x)) {
    cli::cli_abort("Package {.pkg {x}} is not installed.", call = call)
  }

  invisible(NULL)
}

interactive <- NULL

as_json_rowwise <- function(x, ...) {
  json <- jsonlite::toJSON(
    x,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    rownames = FALSE
  )

  json <- sub("^\\[\\{", "[\n  {", json)
  json <- sub("\\}\\]$", "}\n]", json)
  gsub("\\},\\{", "},\n  {", json)
}

path_find_in_project <- function(filename, dir = getwd()) {
  if (file.exists(file.path(dir, filename))) {
    return(normalizePath(file.path(dir, filename)))
  }

  root_files <- c("DESCRIPTION", ".git", ".Rbuildignore", ".vscode", ".here")

  if (any(file.exists(file.path(dir, root_files)))) return(NULL)
  if (dirname(dir) == dir) return(NULL)

  path_find_in_project(filename, dirname(dir))
}
