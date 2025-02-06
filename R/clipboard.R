# largely copied from reprex ---------------------------------------------------
write_to_clipboard <- function(x) {
  tryCatch(
    {
      clipr::write_clip(x)
      cli::cli_alert_success("{.pkg btw} copied to the clipboard!")
    },
    error = function(e) e
  )
  return(invisible())
}
