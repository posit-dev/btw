write_to_clipboard <- function(x, what = "{.pkg btw}") {
  if (!is_interactive() || !clipr::clipr_available()) {
    if (is_interactive()) {
      cli::cli_alert_warning(
        "Clipboard is not available, copy prompt below..."
      )
      cli::cat_line(x)
    }
    return(invisible(x))
  }

  # nocov start
  tryCatch(
    {
      clipr::write_clip(x)
      cli::cli_alert_success(sprintf("%s copied to the clipboard!", what))
    },
    error = function(e) e
  )
  # nocov end

  invisible(x)
}
