use_latest_pandoc <- function(.envir = parent.frame()) {
  if (nzchar(Sys.getenv("CI"))) {
    if (!nzchar(Sys.getenv("BTW_TESTS_PANDOC_VERSION"))) {
      # ci installs latest pandoc for us
      return()
    }
  }
  v <- Sys.getenv("BTW_TESTS_PANDOC_VERSION", "latest")
  if (identical(v, "latest")) {
    v <- suppressMessages(pandoc::pandoc_available_releases()[1])
  }
  if (!pandoc::pandoc_is_installed(v)) {
    pandoc::pandoc_install(v)
  }
  pandoc::pandoc_activate(v, quiet = TRUE)
}

skip_if_not_snapshot_env <- function() {
  # Skip helper: snapshots only on macos for now
  skip_on_os("windows")
  skip_on_os("linux")
  skip_if_not_installed("ellmer", "0.4.0")
}

expect_btw_tool_result <- function(
  x,
  has_data = TRUE,
  expect_value_type = "character"
) {
  expect_s3_class(x, "ellmer::ContentToolResult")
  expect_type(x@value, expect_value_type)
  if (has_data) {
    expect_s3_class(x@extra$data, "data.frame")
  }
}

scrub_system_info <- function(x) {
  x <- sub(R.version.string, "R VERSION", x, fixed = TRUE)
  x <- sub(sessioninfo::os_name(), "OPERATING SYSTEM", x, fixed = TRUE)
  x <- sub(version$system, "SYSTEM VERSION", x, fixed = TRUE)
  x <- sub(Sys.timezone(), "TIMEZONE", x, fixed = TRUE)
  x <- sub(platform_date(), "CURRENT DATE", x, fixed = TRUE)
  x <- sub(
    sprintf(": %s", Sys.getlocale("LC_CTYPE")),
    ": LC_CTYPE",
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

# Helper to enable tools that are conditionally registered
local_enable_tools <- function(
  has_chromote = TRUE,
  has_devtools = TRUE,
  has_roxygen2 = TRUE,
  rstudioapi_has_source_editor_context = TRUE,
  btw_can_register_git_tool = TRUE,
  btw_can_register_gh_tool = TRUE,
  btw_can_register_run_r_tool = TRUE,
  btw_can_register_subagent_tool = TRUE,
  .env = caller_env()
) {
  local_mocked_bindings(
    has_chromote = function() has_chromote,
    has_devtools = function() has_devtools,
    has_roxygen2 = function() has_roxygen2,
    rstudioapi_has_source_editor_context = function() {
      rstudioapi_has_source_editor_context
    },
    btw_can_register_git_tool = function() btw_can_register_git_tool,
    btw_can_register_gh_tool = function() btw_can_register_gh_tool,
    btw_can_register_run_r_tool = function() btw_can_register_run_r_tool,
    btw_can_register_subagent_tool = function() btw_can_register_subagent_tool,
    .env = .env
  )
}

local_sessioninfo_quarto_version <- function(.env = caller_env()) {
  local_mocked_bindings(
    get_quarto_version = function() "99.9.9 @ /Applications/quarto/bin/quarto",
    .package = "sessioninfo",
    .env = .env
  )
}

local_skip_pandoc_convert_text <- function(.env = caller_env()) {
  local_mocked_bindings(
    pandoc_convert_text = function(text, ...) {
      # Skip actual pandoc conversion for speed
      text
    },
    .env = .env
  )
}

local_skip_pandoc_convert <- function(.env = caller_env()) {
  local_mocked_bindings(
    pandoc_convert = function(path, ...) {
      # Skip actual pandoc conversion for speed
      read_file(path)
    },
    .env = .env
  )
}
