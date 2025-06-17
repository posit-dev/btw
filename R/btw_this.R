#' Describe something for use by an LLM
#'
#' A generic function used to describe an object for use by LLM.
#'
#' @examples
#' btw_this(mtcars) # describe the mtcars dataset
#' btw_this(dplyr::mutate) # include function source
#' \dontrun{
#' btw_this("{dplyr}") # include dplyr's intro vignette
#' btw_this("./") # list files in the current working directory
#' }
#'
#' @param x The thing to describe.
#' @param ... Additional arguments passed down to underlying methods. Unused
#'   arguments are silently ignored.
#'
#' @return A character vector of lines describing the object.
#' @family `btw_this()` methods
#' @export
btw_this <- function(x, ...) {
  UseMethod("btw_this")
}

#' @export
btw_this.default <- function(x, ...) {
  capture_print(x)
}

capture_print <- function(x) {
  # TODO: Replace with {evaluate}
  local_reproducible_output(max.print = 100)

  out <- capture.output(print(x))
  if (length(out) == 0 || !any(nzchar(out))) {
    out <- capture.output(print(x), type = "message")
  }

  as_btw_capture(out)
}

as_btw_capture <- function(x) {
  x <- cli::ansi_strip(x)
  structure(x, class = c("btw_captured", "character"))
}

#' Describe objects
#'
#' @description
#' Character strings in `btw_this()` are used as shortcuts to many underlying
#' methods. `btw_this()` detects specific formats in the input string to
#' determine which method to call, or by default it will try to evaluate the
#' character string as R code and return the appropriate object description.
#'
#' `btw_this()` knows about the following special character string formats:
#'
#' * `"./path"` \cr
#'   Any string starting with `./` is treated as a relative path.
#'   If the path is a file, we call [btw_tool_files_read_text_file()] and if the path
#'   is a directory we call [btw_tool_files_list_files()] on the path.
#'
#'   * `btw_this("./data")` lists the files in `data/`.
#'   * `btw_this("./R/load_data.R")` reads the source of the `R/load_data.R`
#'     file.
#'
#' * `"{pkgName}"` \cr
#'   A package name wrapped in braces. Returns the list of help topics
#'   ([btw_tool_docs_package_help_topics()]) and, if it exists, the
#'   introductory vignette for the package ([btw_tool_docs_vignette()]).
#'
#'   * `btw_this("{dplyr}")` includes dplyr's introductory vignette.
#'   * `btw_this("{btw}")` returns only the package help index (because `btw`
#'     doesn't have an intro vignette, yet).
#'
#' * `"?help_topic"` \cr
#'   When the string starts with `?`, btw searches R's help
#'   topics using [btw_tool_docs_help_page()].
#'
#'   * `btw_this("?dplyr::across")` includes the reference page for
#'     `dplyr::across`.
#'
#' * `"@news {{package_name}} {{search_term}}"` \cr
#'   Include the release notes (NEWS) from the latest package release, e.g.
#'   `"@news dplyr"`, or that match a search term, e.g. `"@news dplyr join_by"`.
#'
#' * `"@url {{url}}"` \cr
#'   Include the contents of a web page at the specified URL as markdown, e.g.
#'   `"@url https://cran.r-project.org/doc/FAQ/R-FAQ.html"`. Requires the
#'   \pkg{chromote} package to be installed.
#'
#' * `"@current_file"` or `"@current_selection"` \cr
#'   When used in RStudio or Positron, or anywhere else that the
#'   \pkg{rstudioapi} is supported, `btw("@current_file")` includes the contents
#'   of the file currently open in the editor using
#'   [rstudioapi::getSourceEditorContext()].
#'
#' * `"@clipboard"` \cr
#'   Includes the contents currently stored in your clipboard.
#'
#' * `"@platform_info"` \cr
#'   Includes information about the current platform, such as the R version,
#'   operating system, IDE or UI being used, as well as language, locale,
#'   timezone and current date.
#'
#' * `"@attached_packages"`, `"@loaded_packages"`, `"@installed_packages"` \cr
#'   Includes information about the attached, loaded, or installed packages in
#'   your R session, using [sessioninfo::package_info()].
#'
#' * `"@last_error"` \cr
#'   Includes the message from the last error that occurred in your session.
#'   To reliably capture the last error, you need to enable
#'   [rlang::global_entrace()] in your session.
#'
#' * `"@last_value"` \cr
#'   Includes the `.Last.value`, i.e. the result of the last expression
#'   evaluated in your R console.
#'
#' @examples
#' btw_this("?btw::btw_this")
#'
#' @param x A character string
#' @param ... Ignored.
#' @param caller_env The caller environment.
#'
#' @inherit btw_this return
#'
#' @family `btw_this()` methods
#' @export
btw_this.character <- function(x, ..., caller_env = parent.frame()) {
  check_string(x)
  x <- trimws(x)

  if (identical(x, "@current_file")) {
    return(I(
      btw_tool_ide_read_current_editor(selection = FALSE, consent = TRUE)@value
    ))
  }
  if (identical(x, "@current_selection")) {
    return(I(
      btw_tool_ide_read_current_editor(selection = TRUE, consent = TRUE)@value
    ))
  }
  if (identical(x, "@clipboard")) {
    return(I(clipr::read_clip()))
  }
  if (identical(x, "@platform_info")) {
    return(btw_tool_session_platform_info()@value)
  }
  if (identical(x, "@attached_packages")) {
    return(I(btw_tool_session_package_info("attached")@value))
  }
  if (identical(x, "@loaded_packages")) {
    return(I(btw_tool_session_package_info("loaded")@value))
  }
  if (identical(x, "@installed_packages")) {
    return(I(btw_tool_session_package_info("installed")@value))
  }
  if (identical(x, "@last_error")) {
    err <- get_last_error()
    return(
      if (is.null(err)) btw_ignore() else capture_print(err)
    )
  }
  if (identical(x, "@last_value")) {
    return(btw_this(get_last_value()))
  }
  if (identical(substring(x, 1, 5), "@news")) {
    # Special syntax for @news: '@news dplyr' or '@news dplyr join_by'
    args <- substring(x, 7)
    if (!nzchar(args)) {
      cli::cli_abort(c(
        "{.code @news} must be followed by a package name and an optional search term.",
        "i" = 'e.g. {.code "@news dplyr"} or {.code "@news dplyr join_by"}'
      ))
    }
    parts <- strsplit(args, " ", fixed = TRUE)[[1]]
    package_name <- parts[1]
    search_term <- if (length(parts) > 1) {
      paste(parts[-1], collapse = " ")
    } else {
      ""
    }
    return(I(btw_tool_docs_package_news(package_name, search_term)@value))
  }
  if (identical(substring(x, 1, 4), "@url")) {
    if (!has_chromote()) {
      return(btw_ignore())
    }

    # Special syntax for @url: '@url https://example.com'
    url <- trimws(substring(x, 5))
    if (!nzchar(url)) {
      cli::cli_abort(c(
        "{.code @url} must be followed by a valid URL.",
        "i" = 'e.g. {.code "@url https://example.com"}'
      ))
    }
    return(I(btw_tool_web_read_url(url)@value))
  }

  if (grepl("^\\./", x)) {
    path <- substring(x, 3, nchar(x))
    if (!nzchar(path)) {
      path <- "."
    }
    if (fs::is_file(path)) {
      return(btw_tool_files_read_text_file(path)@value)
    } else {
      return(btw_tool_files_list_files(path)@value)
    }
  }

  if (grepl("^\\{[a-zA-Z][a-zA-Z0-9.]+\\}$", x)) {
    # Catch R packages in the form: {dplyr} or {btw}
    # R packages must:
    # * start with a letter
    # * use only letters, numbers or .
    # * be two or more characters long
    pkg <- substring(x, 2, nchar(x) - 1)
    res <- c(
      btw_tool_docs_package_help_topics(pkg)@value,
      tryCatch(
        c("", btw_tool_docs_vignette(pkg)@value),
        error = function(e) NULL
      )
    )
    return(res)
  }

  if (substring(x, 1, 1) == "?") {
    x <- substring(x, 2, nchar(x))
    x <- strsplit(x, "::", fixed = TRUE)[[1]]
    if (length(x) == 2) {
      return(btw_this(as_btw_docs_topic(x[1], x[2])))
    } else {
      return(btw_this(as_btw_docs_topic(NULL, x[1])))
    }
  }

  btw_user_prompt(x)
}

#' @export
btw_this.Chat <- function(x, ...) {
  btw_ignore()
}

#' @export
btw_this.function <- function(x, ...) {
  fn_def <- capture.output(print(x))

  fn_def <- fn_def[!grepl("^<(bytecode|environment): ", fn_def)]

  md_code_block("r", fn_def)
}

#' @export
btw_this.btw_docs_topic <- function(x, ...) {
  btw_tool_docs_help_page(package_name = x$package, topic = x$topic)@value
}

#' @export
btw_this.help_files_with_topic <- function(x, ...) {
  args <- call_args(attr(x, "call")) # help(topic = {topic}, package = {package})
  btw_tool_docs_help_page(package_name = args$package, topic = args$topic)@value
}

as_btw_docs_topic <- function(package, topic) {
  structure(
    list(package = package, topic = topic),
    class = "btw_docs_topic"
  )
}

#' @export
btw_this.btw_docs_package <- function(x, ...) {
  c(
    sprintf("Documented functions and help topics in package %s:", x$package),
    btw_tool_docs_package_help_topics(x$package)@value
  )
}

as_btw_docs_package <- function(package) {
  structure(list(package = package), class = "btw_docs_package")
}

#' @export
btw_this.packageIQR <- function(x, ...) {
  # vignette(package = "btw")
  package_name <- unique(x$results[, "Package"])
  btw_tool_docs_available_vignettes(package_name)@value
}

#' @export
btw_this.btw_docs_vignettes <- function(x, ...) {
  btw_tool_docs_available_vignettes(package_name = x$package)@value
}

#' @export
btw_this.btw_docs_vignette <- function(x, ...) {
  btw_tool_docs_vignette(
    package_name = x$package,
    vignette = x$vignette %||% x$package
  )@value
}

#' @export
btw_this.vignette <- function(x, ...) {
  btw_tool_docs_vignette(
    package_name = x$Package,
    vignette = x$Topic
  )@value
}

btw_ignore <- function() {
  structure(list(), class = "btw_ignore")
}

btw_user_prompt <- function(...) {
  structure(c(...), class = c("btw_user_prompt"))
}

# https://github.com/RConsortium/S7/issues/501#issuecomment-2494609728
#' @rawNamespace S3method(base::print, btw_ignore)
print.btw_ignore <- function(x, ...) {
  invisible(x)
}

btw_returns_character <- function(...) {
  structure(c(...), class = c("btw_returns_character", "character"))
}

#' @export
btw_this.btw_returns_character <- function(x, ...) {
  capture_print(unclass(x))
}

# Helpers ---------------------------------------------------------------------

get_last_error <- function() {
  # last_error() throws its own error if there aren't any errors yet
  err <- tryCatch(last_error(), error = function(e) NULL)
  if (is.null(err)) {
    cli::cli_warn(c(
      "No last error was found.",
      "i" = "Use {.run ?rlang::global_entrace} to enable {.code @last_error}."
    ))
  }
  err
}

get_last_value <- function() {
  base::.Last.value
}
