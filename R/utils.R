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
  writeLines(html, tmp_input)
  pandoc_convert(tmp_input, from = "html", to = to, ...)
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
  # Combine into single string for xml2 processing
  html_text <- paste(html, collapse = "\n")

  # Parse HTML
  doc <- tryCatch(
    xml2::read_html(html_text),
    error = function(e) {
      # If parsing fails, just return original HTML
      return(NULL)
    }
  )

  if (is.null(doc)) {
    return(html)
  }

  # Find all img tags
  img_nodes <- xml2::xml_find_all(doc, "//img")

  if (length(img_nodes) == 0) {
    return(html)
  }

  # Process each image
  for (img in img_nodes) {
    src <- xml2::xml_attr(img, "src")

    # Only process base64 data URIs
    if (!is.na(src) && grepl("^data:image", src)) {
      # Get alt text if available
      alt_text <- xml2::xml_attr(img, "alt")

      # Create replacement text
      if (!is.na(alt_text) && nzchar(alt_text)) {
        replacement <- paste0("[Image: ", alt_text, "]")
      } else {
        replacement <- "[Image]"
      }

      # Replace the img node with a text node
      xml2::xml_replace(img, xml2::read_xml(paste0("<span>", replacement, "</span>")))
    }
  }

  # Convert back to character vector
  result <- as.character(doc)
  strsplit(result, "\n")[[1]]
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

to_title_case <- function(x) {
  paste0(toupper(substring(x, 1, 1)), substring(x, 2))
}
