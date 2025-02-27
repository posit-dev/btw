.btw_tools <- list()

.btw_add_to_tools <- function(fn) {
  stopifnot(is.function(fn))
  .btw_tools[[length(.btw_tools) + 1]] <<- fn
  invisible(fn)
}
