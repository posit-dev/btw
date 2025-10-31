btw_app_addin <- function() {
  rlang::check_installed("httpuv")

  rstudioapi::setPersistentValue("btw_app_addin_url", "")

  code <- c(
    'options(btw.app.in_addin = TRUE)',
    'btw::btw_app()'
  )
  tmp <- tempfile("btw_app_job_", fileext = ".R")
  writeLines(code, tmp)
  rstudioapi::jobRunScript(tmp, name = "btw_app()", workingDir = getwd())
  rstudioapi::executeCommand("activateConsole")

  now <- function() as.integer(Sys.time())
  max_wait <- now() + 10
  spinner <- cli::make_spinner(
    "balloon",
    template = paste(
      "{spin}",
      cli::format_inline("Starting up {.field btw_app()} ...")
    )
  )

  while (now() < max_wait) {
    spinner$spin()
    url <- rstudioapi::getPersistentValue("btw_app_addin_url")
    if (nzchar(url)) {
      break
    }
    Sys.sleep(0.25)
  }

  spinner$finish()

  url <- rstudioapi::getPersistentValue("btw_app_addin_url")

  if (!nzchar(url)) {
    cli::cli_abort(
      "{.fn btw::btw_app} failed to start up in 10s. See background job pane for more details."
    )
  }

  url <- rstudioapi::translateLocalUrl(url, absolute = TRUE)

  get(".rs.invokeShinyPaneViewer", "tools:rstudio")(url)

  invisible(url)
}
