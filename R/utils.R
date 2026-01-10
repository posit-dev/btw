pandoc_convert <- function(path, ..., from = "html", to = "markdown") {
  tmp_file <- withr::local_tempfile()

  rmarkdown::pandoc_convert(
    path,
    from = from,
    to = to,
    output = tmp_file,
    ...
  )

  read_lines(tmp_file)
}

pandoc_convert_text <- function(text, ..., from = "html", to = "markdown") {
  map_chr(text, function(x) {
    tmp_input <- withr::local_tempfile()
    write_file(x, tmp_input)
    paste(pandoc_convert(tmp_input, from = from, to = to, ...), collapse = "\n")
  })
}

pandoc_html_simplify <- function(
  html,
  ...,
  options = c(
    "--markdown-headings=atx",
    "--embed-resources=false"
  ),
  to = "markdown_strict-raw_html+pipe_tables+backtick_code_blocks"
) {
  # Remove base64 embedded images to avoid bloating the output
  html <- remove_base64_images(html)

  tmp_input <- withr::local_tempfile()
  write_lines(html, tmp_input)
  pandoc_convert(tmp_input, from = "html", to = to, ...)
}

xml_from_html <- function(html) {
  html_text <- paste(html, collapse = "\n")

  doc <- tryCatch(
    xml2::read_html(html_text),
    error = function(e) NULL
  )

  doc
}

#' Remove base64 embedded images from HTML
#'
#' Replaces <img> tags with base64 data URIs with a text placeholder
#' containing the alt text if available.
#'
#' @param html Character vector of HTML content
#' @return Character vector with images removed/replaced
#' @noRd
remove_base64_images <- function(html) {
  doc <- xml_from_html(html)

  if (is.null(doc)) {
    return(html)
  }

  # Find all <img> tags with data: URIs (includes base64 and other data URIs)
  img_nodes <- xml2::xml_find_all(doc, "//img[contains(@src, 'data:')]")

  if (length(img_nodes) == 0) {
    return(html)
  }

  # Replace data: URI images with text placeholders
  for (img in img_nodes) {
    alt_text <- xml2::xml_attr(img, "alt")
    replacement <- if (!is.na(alt_text) && nzchar(alt_text)) {
      sprintf("[Image: %s]", alt_text)
    } else {
      "[Image]"
    }

    xml2::xml_replace(img, "span", replacement)
  }

  as.character(doc)
}

cli_escape <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  gsub("}", "}}", x, fixed = TRUE)
}

glue_ <- function(x, ..., .envir = parent.frame()) {
  as.character(ellmer::interpolate(x, ..., .envir = .envir))
}

HTML <- function(text, ...) {
  # In-lined htmltools::HTML() to avoid dependency
  x <- paste(c(text, ...), collapse = " ")
  attr(x, "html") <- TRUE
  class(x) <- c("html", "character")
  x
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

  if (at_project_root) {
    return(NULL)
  }

  path_find_in_project(filename, dirname(dir))
}

path_find_user <- function(filename) {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    # In testthat, we don't want to use the home directory
    return(NULL)
  }

  possibilities <- c(
    fs::path_home(filename),
    fs::path_home_r(filename),
    fs::path_home(".config", "btw", filename)
  )

  for (path in possibilities) {
    if (fs::file_exists(path)) {
      return(fs::path_norm(path))
    }
  }

  NULL
}

detect_project_is_r_package <- function(dir = getwd()) {

  !is.null(path_find_in_project("DESCRIPTION", dir))
}

# Agent file discovery ---------------------------------------------------------

# Find agent-*.md files in project .btw/ directory
find_project_agent_files <- function(dir = getwd()) {
  btw_dir <- path_find_in_project(".btw", dir)

  if (is.null(btw_dir) || !fs::dir_exists(btw_dir)) {
    return(character())
  }

  files <- fs::dir_ls(btw_dir, regexp = "agent-.*\\.md$", type = "file")
  as.character(files)
}

# Find agent-*.md files in user config directories (~/.btw/, ~/.config/btw/)
find_user_agent_files <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(character())
  }

  user_dirs <- c(
    fs::path_home(".btw"),
    fs::path_home(".config", "btw")
  )

  files <- character()
  for (dir in user_dirs) {
    if (fs::dir_exists(dir)) {
      found <- fs::dir_ls(dir, regexp = "agent-.*\\.md$", type = "file")
      files <- c(files, as.character(found))
    }
  }

  files
}

# Claude Code agent file discovery ---------------------------------------------

# Find agent *.md files in project .claude/agents/ directory
find_project_claude_code_agent_files <- function(dir = getwd()) {
  agents_dir <- path_find_in_project(".claude/agents", dir)

  if (is.null(agents_dir) || !fs::dir_exists(agents_dir)) {
    # Also try .claude then agents subdirectory
    claude_dir <- path_find_in_project(".claude", dir)
    if (!is.null(claude_dir)) {
      agents_dir <- fs::path(claude_dir, "agents")
    }
  }

  if (is.null(agents_dir) || !fs::dir_exists(agents_dir)) {
    return(character())
  }

  files <- fs::dir_ls(agents_dir, regexp = "\\.md$", type = "file")
  as.character(files)
}

# Find agent *.md files in ~/.claude/agents/ directory
find_user_claude_code_agent_files <- function() {
  if (identical(Sys.getenv("TESTTHAT"), "true")) {
    return(character())
  }

  agents_dir <- fs::path_home(".claude", "agents")

  if (!fs::dir_exists(agents_dir)) {
    return(character())
  }

  files <- fs::dir_ls(agents_dir, regexp = "\\.md$", type = "file")
  as.character(files)
}

path_btw_cache <- function(...) {
  cache_base <- normalizePath(
    tools::R_user_dir("btw", which = "cache"),
    mustWork = FALSE,
    winslash = "/"
  )
  fs::path(cache_base, ...)
}

local_reproducible_output <- function(
  width = 80L,
  max.print = 100,
  disable_ansi_features = TRUE,
  .env = parent.frame()
) {
  # Replicating testthat::local_reproducible_output()
  withr::local_options(width = width, cli.width = width, .local_envir = .env)
  withr::local_envvar(
    RSTUDIO_CONSOLE_WIDTH = width,
    R_CLI_DYNAMIC = "false",
    .local_envir = .env
  )

  if (disable_ansi_features) {
    withr::local_envvar(NO_COLOR = "true", .local_envir = .env)
    withr::local_options(
      crayon.enabled = FALSE,
      cli.unicode = FALSE,
      cli.condition_width = Inf,
      cli.num_colors = 1L,
      .local_envir = .env
    )
  } else {
    withr::local_envvar(list(NO_COLOR = NA), .local_envir = .env)
    withr::local_options(
      crayon.enabled = TRUE,
      cli.ansi = TRUE,
      cli.unicode = TRUE,
      cli.condition_width = width,
      cli.num_colors = 16L,
      .local_envir = .env
    )
  }

  withr::local_options(
    cli.dynamic = FALSE,
    cli.spinner = FALSE,
    cli.hyperlink = FALSE,
    cli.hyperlink_run = FALSE,
    cli.hyperlink_help = FALSE,
    cli.hyperlink_vignette = FALSE,
    useFancyQuotes = FALSE,
    lifecycle_verbosity = "warning",
    OutDec = ".",
    rlang_interactive = FALSE,
    max.print = max.print,
    .local_envir = .env
  )
}

strip_ansi <- function(text) {
  # Matches codes like "\x1B[31;43m", "\x1B[1;3;4m"
  ansi_pattern <- "(\x1B|\x033)\\[[0-9;?=<>]*[@-~]"
  gsub(ansi_pattern, "", text)
}

to_title_case <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
