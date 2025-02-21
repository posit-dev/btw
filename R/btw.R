#' Plain-text descriptions of R objects
#'
#' @description
#' This function allows you to quickly describe your computational environment
#' to a model by concatenating plain-text descriptions of "R stuff", from
#' data frames to packages to function documentation.
#'
#' @param ... Objects to describe from your R environment. You can pass objects
#' themselves, like data frames or functions, or the function also accepts
#' output from `get_*()` functions like [get_package_help()], [get_help_page()],
#' etc. If omitted, this function will just describe the elements in your global
#' R environment.
#' @param clipboard Whether to write the results to the clipboard.
#' A single logical value; will default to `TRUE` when run interactively.
#'
#' @returns
#' The combined elements as a string, invisibly. If `clipboard` is `TRUE`, the
#' result is also written to the system clipboard.
#'
#' @export
btw <- function(..., clipboard = interactive()) {
  check_bool(clipboard)

  elts <- dots_list(..., .named = TRUE)

  if (length(elts) == 0) {
    res <- get_environment()
  } else {
    res <- get_environment(as.environment(elts))
  }

  if (clipboard) {
    write_to_clipboard(res)
  }

  invisible(res)
}
