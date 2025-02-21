#' Plain-text descriptions of R objects
#'
#' @description
#' A short description...
#'
#' @param ... Objects to describe from your R environment. Character strings
#' will be searched for on the search path, though the function also accepts
#' output from `this_*()` functions like [this_env()], [this_fn()],
#' [this_pkg()], or [this_df()].
#'
#' If omitted, this function will just describe the elements in your global
#' R environment.
#' @param ... Character vectors or, more likely, outputs of `this_*()`
#' functions like [this_env()], [this_fn()], [this_pkg()], or [this_df()].
#' If nothing is provided in this argument, `btw()` will just describe the
#' objects in your global R environment.
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

  elts <- list2(...)

  if (length(elts) == 0) {
    # TODO: this_env() isn't actually implemented yet
    # res <- this_env(global_env())
    res <- "hey"
  } else {
    # TODO:: check that each element inherits from character or `this_*()` output class
    res <- paste0(elts, collapse = "\n\n")
  }

  if (clipboard) {
    write_to_clipboard(res)
  }

  invisible(res)
}
