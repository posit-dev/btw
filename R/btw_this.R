btw_this <- function(x, ...) {
  UseMethod("btw_this")
}

#' @export
btw_this.default <- function(x, ...) {
  # TODO: Replace with {evaluate}
  out <- capture.output(print(x))
  if (length(out) && nzchar(out)) return(out)

  capture.output(print(x), type = "message")
}

#' @export
btw_this.Chat <- function(x, ...) {
  btw_ignore()
}

#' @export
btw_this.function <- function(x, ...) {
  # Implementation for the function method
}

#' @export
btw_this.btw_docs_topic <- function(x, ...) {
  get_help_page(x$package, x$topic)
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
