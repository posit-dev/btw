this_env <- function(x, which, ..., clipboard = interactive()) {
  # TODO: gander:::fetch_env_context sort of thing

  if (clipboard && !in_btw()) {
    write_to_clipboard(res)
  }

  invisible(res)
}
