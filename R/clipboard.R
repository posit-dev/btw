write_to_clipboard <- function(x) {
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
      cli::cli_alert_success("{.pkg btw} copied to the clipboard!")
    },
    error = function(e) e
  )
  # nocov end

  invisible(x)
}
