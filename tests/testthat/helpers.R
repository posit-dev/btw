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
  x
}

mock_platform_date <- function() {
  local_mocked_bindings(
    platform_date = function() "DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)",
    .env = caller_env()
  )
}
