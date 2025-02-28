#' Describe something for use by an LLM
#'
#' A generic function used to describe an object for use by LLM.
#'
#' @examples
#' btw_this(mtcars)
#' btw_this(dplyr::mutate)
#' btw_this("{dplyr}")
#'
#' # Files ----
#' btw_this("./") # list files in the current working directory
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
  out <- capture.output(print(x))
  if (length(out) && nzchar(out)) return(out)

  capture.output(print(x), type = "message")
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
#'   If the path is a file, we call [btw_tool_read_text_file()] and if the path
#'   is a directory we call [btw_tool_list_files()] on the path.
#'
#'   * `btw_this("./data")` lists the files in `data/`.
#'   * `btw_this("./R/load_data.R")` reads the source of the `R/load_data.R`
#'     file.
#'
#' * `"{pkgName}"` \cr
#'   A package name wrapped in braces. Returns either the
#'   introductory vignette for the package
#'   ([btw_tool_get_vignette_from_package()]) or a list of help topics if no
#'   such vignette exists ([btw_tool_get_package_help_topics()]).
#'
#'   * `btw_this("{dplyr}")` includes dplyr's introductory vignette.
#'   * `btw_this("{btw}")` returns the package help index (because `btw`
#'     doesn't have an intro vignette, yet).
#'
#' * `"?help_topic"` \cr
#'   When the string starts with `?`, btw searches R's help
#'   topics using [btw_tool_get_help_page()].
#'
#'   * `btw_this("?dplyr::across")` includes the reference page for
#'     `dplyr::across`.
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

  if (grepl("^\\./", x)) {
    path <- substring(x, 3, nchar(x))
    if (!nzchar(path)) {
      path <- "."
    }
    if (fs::is_file(path)) {
      return(btw_tool_read_text_file(path))
    } else {
      return(btw_tool_list_files(path))
    }
  }

  if (grepl("^\\{[a-zA-Z][a-zA-Z0-9.]+\\}$", x)) {
    # Catch R packages in the form: {dplyr} or {btw}
    # R packages must:
    # * start with a letter
    # * use only letters, numbers or .
    # * be two or more characters long
    pkg <- substring(x, 2, nchar(x) - 1)
    res <- tryCatch(
      # Get the package vignette
      btw_tool_get_vignette_from_package(pkg),
      error = function(err) {
        # or list help topics
        btw_tool_get_package_help_topics(pkg)
      }
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

  x_expr <- parse_expr(x)
  tryCatch(
    inject(btw_this(!!x_expr), env = caller_env),
    error = function(err) {
      inject(capture_print(!!x))
    }
  )
}

#' @export
btw_this.Chat <- function(x, ...) {
  btw_ignore()
}

#' @export
btw_this.function <- function(x, ...) {
  x_text <- deparse(substitute(x))
  if (is_namespace(fn_env(x))) {
    # packaged function
    package <- sub(
      "namespace:",
      "",
      env_name(fn_env(x)),
      fixed = TRUE
    )
    fn_name <- sub("^([^:]*::)?", "", x_text)
    return(btw_this(as_btw_docs_topic(package, fn_name)))
  }

  strsplit(expr_text(fn_body(x), width = 80), "\n")[[1]]
}

#' @export
btw_this.btw_docs_topic <- function(x, ...) {
  btw_tool_get_help_page(x$package, x$topic)
}

#' @export
btw_this.help_files_with_topic <- function(x, ...) {
  args <- call_args(attr(x, "call")) # help(topic = {topic}, package = {package})
  btw_tool_get_help_page(args$package, args$topic)
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
    btw_tool_get_package_help_topics(x$package)
  )
}

as_btw_docs_package <- function(package) {
  structure(list(package = package), class = "btw_docs_package")
}

#' @export
btw_this.btw_docs_vignettes <- function(x, ...) {
  btw_tool_get_available_vignettes_in_package(package_name = x$package)
}

#' @export
btw_this.btw_docs_vignette <- function(x, ...) {
  btw_tool_get_vignette_from_package(
    package_name = x$package,
    vignette = x$vignette %||% x$package
  )
}

btw_ignore <- function() {
  structure(list(), class = "btw_ignore")
}

#' @export
print.btw_ignore <- function(x, ...) {
  invisible(x)
}
