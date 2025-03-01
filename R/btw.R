#' Plain-text descriptions of R objects
#'
#' @description
#' This function allows you to quickly describe your computational environment
#' to a model by concatenating plain-text descriptions of "R stuff", from
#' data frames to packages to function documentation.
#'
#' @param ... Objects to describe from your R environment. You can pass objects
#'   themselves, like data frames or functions, or the function also accepts
#'   output from `btw_tool_*()` functions like
#'   [btw_tool_get_package_help_topics()], [btw_tool_get_help_page()], etc. If
#'   omitted, this function will just describe the elements in your global R
#'   environment.
#' @param clipboard Whether to write the results to the clipboard.
#'   A single logical value; will default to `TRUE` when run interactively.
#'
#' @examples
#' btw()
#'
#' btw(mtcars)
#'
#' btw(btw::btw)
#'
#' @returns
#' The combined elements as a string, invisibly. If `clipboard` is `TRUE`, the
#' result is also written to the system clipboard.
#'
#' @export
btw <- function(..., clipboard = is_interactive()) {
  check_bool(clipboard)

  elts <- dots_list(..., .named = TRUE)

  if (length(elts) == 0) {
    res <- btw_this(globalenv())
  } else {
    res <- btw_this(
      new_environment(elts, parent = parent.frame()),
      items = names(elts)
    )
  }

  res <- paste(res, collapse = "\n")

  if (clipboard) {
    write_to_clipboard(res)
  }

  invisible(res)
}
