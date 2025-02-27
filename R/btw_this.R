#' Describe something for use by an LLM
#'
#' A generic function used to describe an object for use by LLM.
#'
#' @examples
#' btw_this(mtcars)
#' btw_this(dplyr::mutate)
#' btw_this("{dplyr}")
#'
#' @param x The thing to describe.
#' @param ... Additional arguments passed down to underlying methods. Unused
#'   arguments are silently ignored.
#'
#' @return A character vector of lines describing the object.
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

#' @export
btw_this.character <- function(x, ..., caller_env = parent.frame()) {
  check_string(x)
  x <- trimws(x)

  if (grepl("^\\{[a-zA-Z][a-zA-Z0-9.]+\\}$", x)) {
    # Catch R packages in the form: {dplyr} or {btw}
    # R packages must:
    # * start with a letter
    # * use only letters, numbers or .
    # * be two or more characters long
    x <- substring(x, 2, nchar(x) - 1)
    return(btw_this(as_btw_docs_package(x)))
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
