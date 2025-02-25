#' Describe something for use by an LLM
#'
#' A generic function used to describe an object for use by LLM.
#'
#' @examples
#' btw_this(mtcars)
#' btw_this(dplyr::mutate)
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
  get_help_page(x$package, x$topic)
}

as_btw_docs_topic <- function(package, topic) {
  structure(
    list(package = package, topic = topic),
    class = "btw_docs_topic"
  )
}

#' @export
btw_this.btw_docs_package <- function(x, ...) {
  get_package_help(x$package)
}

#' @export
btw_this.btw_docs_vignettes <- function(x, ...) {
  get_package_vignettes(package_name = x$package)
}

#' @export
btw_this.btw_docs_vignette <- function(x, ...) {
  get_package_vignette(
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
