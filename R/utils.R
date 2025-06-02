pandoc_convert <- function(path, ..., from = "html", to = "markdown") {
  tmp_file <- withr::local_tempfile()

  rmarkdown::pandoc_convert(
    path,
    from = from,
    to = to,
    output = tmp_file,
    ...
  )

  readLines(tmp_file)
}

pandoc_convert_text <- function(text, ..., from = "html", to = "markdown") {
  map_chr(text, function(x) {
    tmp_input <- withr::local_tempfile()
    writeLines(x, tmp_input)
    paste(pandoc_convert(tmp_input, from = from, to = to, ...), collapse = "\n")
  })
}

cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

glue_ <- function(x, ..., .envir = parent.frame()) {
  as.character(ellmer::interpolate(x, ..., .envir = .envir))
}

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

  root_files <- c("DESCRIPTION", ".git", ".vscode", ".here")

  at_project_root <-
    any(file.exists(file.path(dir, root_files))) ||
    length(dir(pattern = ".[.]Rproj$")) > 0 ||
    dirname(dir) == dir

  if (at_project_root) return(NULL)

  path_find_in_project(filename, dirname(dir))
}

path_find_project_root <- function(dir = getwd()) {
  root_files <- c(
    "DESCRIPTION",
    ".git",
    ".vscode",
    ".here",
    "btw.md",
    "btw-memory.yaml",
    "btw-memory.yml"
  )

  if (any(file.exists(file.path(dir, root_files)))) {
    return(normalizePath(dir))
  }

  if (length(dir(pattern = ".[.]Rproj$")) > 0) {
    return(normalizePath(dir))
  }

  if (dirname(dir) == dir) {
    return(NULL)
  }

  path_find_project_root(dirname(dir))
}

local_reproducible_output <- function(
  width = 80L,
  max.print = 100,
  .env = parent.frame()
) {
  # Replicating testthat::local_reproducible_output()
  withr::local_options(width = width, cli.width = width, .local_envir = .env)
  withr::local_envvar(RSTUDIO_CONSOLE_WIDTH = width, .local_envir = .env)
  withr::local_envvar(list(NO_COLOR = "true"), .local_envir = .env)
  withr::local_options(
    crayon.enabled = FALSE,
    cli.hyperlink = FALSE,
    cli.hyperlink_run = FALSE,
    cli.hyperlink_help = FALSE,
    cli.hyperlink_vignette = FALSE,
    cli.dynamic = FALSE,
    cli.unicode = FALSE,
    cli.condition_width = Inf,
    cli.num_colors = 1L,
    useFancyQuotes = FALSE,
    lifecycle_verbosity = "warning",
    OutDec = ".",
    rlang_interactive = FALSE,
    max.print = max.print,
    .local_envir = .env
  )
}
