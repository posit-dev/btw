skip_if_not_macos <- function() {
  # Skip helper: snapshots only on macos for now
  skip_on_os("windows")
  skip_on_os("linux")
}

scrub_system_info <- function(x) {
  x <- sub(R.version.string, "R VERSION", x, fixed = TRUE)
  x <- sub(sessioninfo::os_name(), "OPERATING SYSTEM", x, fixed = TRUE)
  x <- sub(version$system, "SYSTEM VERSION", x, fixed = TRUE)
  x <- sub(Sys.timezone(), "TIMEZONE", x, fixed = TRUE)
  x <- sub(platform_date(), "CURRENT DATE", x, fixed = TRUE)
  x <- sub(
    sprintf("'%s'", Sys.getlocale("LC_CTYPE")),
    "'LC_CTYPE'",
    x,
    fixed = TRUE
  )
  x
}

mock_platform_date <- function() {
  local_mocked_bindings(
    platform_date = function() "DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)",
    .env = caller_env()
  )
}

with_mocked_platform <- function(
  code,
  lc_collate = "C",
  timezone = "Europe/Madrid",
  language = "es",
  ide = "positron"
) {
  mock_platform_date()

  withr::local_language(language)
  withr::local_locale(c(LC_COLLATE = lc_collate))
  withr::local_timezone(timezone)

  switch(
    ide,
    positron = withr::local_envvar(list(POSITRON = 1, RSTUDIO = "")),
    rstudio = withr::local_envvar(list(RSTUDIO = 1, POSITRON = "")),
    vscode = withr::local_envvar(list(
      TERM_PROGRAM = "vscode",
      POSITRON = "",
      RSTUDIO = ""
    )),
    stop("unsupported ide: ", ide)
  )

  force(code)
}
