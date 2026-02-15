#' Deprecated functions
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' These functions have been renamed. The old names continue to work but emit
#' deprecation warnings. Use the new names for new code.
#'
#' @keywords internal
#' @name deprecated
NULL

# Note: Deprecating tools is *complicated*, especially here in btw. The
# functions in this file are stubs that throw deprecation errors. They are
# replaced with proper tool definitions that wrap the new tools, with
# deprecation warnings, when the package is loaded.

#' @describeIn deprecated `btw_tool_session_platform_info()` was renamed to
#'   [btw_tool_sessioninfo_platform()] in btw 1.2.0.
#' @export
btw_tool_session_platform_info <- function(`_intent` = NULL) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_session_platform_info()",
    "btw_tool_sessioninfo_platform()"
  )
}

#' @describeIn deprecated `btw_tool_session_package_info()` was renamed to
#'   [btw_tool_sessioninfo_package()] in btw 1.2.0.
#' @export
btw_tool_session_package_info <- function(
  packages = "attached",
  dependencies = "",
  `_intent` = NULL
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_session_package_info()",
    "btw_tool_sessioninfo_package()"
  )
}

#' @describeIn deprecated `btw_tool_session_check_package_installed()` was
#'   renamed to [btw_tool_sessioninfo_is_package_installed()] in btw 1.2.0.
#' @export
btw_tool_session_check_package_installed <- function(
  package_name,
  `_intent` = NULL
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_session_check_package_installed()",
    "btw_tool_sessioninfo_is_package_installed()"
  )
}

#' @describeIn deprecated `btw_tool_search_packages()` was renamed to
#'   [btw_tool_cran_search()] in btw 1.2.0.
#' @export
btw_tool_search_packages <- function(
  query,
  format = c("short", "long"),
  n_results = NULL,
  `_intent` = NULL
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_search_packages()",
    "btw_tool_cran_search()"
  )
}

#' @describeIn deprecated `btw_tool_search_package_info()` was renamed to
#'   [btw_tool_cran_package()] in btw 1.2.0.
#' @export
btw_tool_search_package_info <- function(package_name, `_intent` = NULL) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_search_package_info()",
    "btw_tool_cran_package()"
  )
}

#' @describeIn deprecated `btw_tool_files_list_files()` was renamed to
#'   [btw_tool_files_list()] in btw 1.2.0.
#' @export
btw_tool_files_list_files <- function(
  path = NULL,
  type = c("any", "file", "directory"),
  regexp = "",
  `_intent` = NULL
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_files_list_files()",
    "btw_tool_files_list()"
  )
}

#' @describeIn deprecated `btw_tool_files_read_text_file()` was renamed to
#'   [btw_tool_files_read()] in btw 1.2.0.
#' @export
btw_tool_files_read_text_file <- function(
  path,
  line_start = 1,
  line_end = 1000,
  `_intent` = NULL
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_files_read_text_file()",
    "btw_tool_files_read()"
  )
}

#' @describeIn deprecated `btw_tool_files_write_text_file()` was renamed to
#'   [btw_tool_files_write()] in btw 1.2.0.
#' @export
btw_tool_files_write_text_file <- function(
  path,
  content,
  `_intent` = NULL
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_files_write_text_file()",
    "btw_tool_files_write()"
  )
}

#' @describeIn deprecated `btw_tool_files_code_search()` was renamed to
#'   [btw_tool_files_search()] in btw 1.2.0.
#' @export
btw_tool_files_code_search <- function(
  term,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE,
  show_lines = FALSE,
  `_intent` = ""
) {
  lifecycle::deprecate_stop(
    "1.2.0",
    "btw_tool_files_code_search()",
    "btw_tool_files_search()"
  )
}

# Deprecated tool registrations
#
# These create ellmer tool definitions for deprecated tool names, allowing
# them to be used with ellmer chat clients. Each deprecated tool wraps the
# new tool's implementation and emits a deprecation warning when called.
#
# All deprecated tools are placed in the "deprecated" group, which only
# appears in btw_app() if one of these tools is actively selected. The
# deprecated tool version is recorded in the btw_deprecated annotation.

.btw_add_deprecated_tool <- function(
  deprecated_name,
  new_name,
  version
) {
  check_string(deprecated_name)
  check_string(new_name)
  check_string(version)

  deprecated_group <- "deprecated"

  btw_env <- fn_env(btw_tools)
  new_tool <- env_get(btw_env, new_name, NULL)

  if (is.null(new_tool)) {
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
      cli::cli_abort(
        "Cannot create deprecated tool {.val {deprecated_name}}: new tool {.val {new_name}} not found."
      )
    }
    return(invisible())
  }

  new_tool_call <- call2(
    new_name,
    !!!lapply(
      set_names(fn_fmls_names(new_tool)),
      function(arg) sym(arg)
    )
  )

  deprecated_tool_body <- expr({
    lifecycle::deprecate_warn(
      !!version,
      !!paste0(deprecated_name, "()"),
      !!paste0(new_name, "()")
    )
    !!new_tool_call
  })

  deprecated_tool_fn <- new_function(
    fn_fmls(new_tool),
    deprecated_tool_body
  )
  fn_env(deprecated_tool_fn) <- btw_env

  annotations <- new_tool@annotations
  annotations$btw_group <- "deprecated"
  annotations$btw_deprecated <- "1.2.0"

  deprecated_tool <- ellmer::tool(
    deprecated_tool_fn,
    description = new_tool@description,
    arguments = new_tool@arguments@properties,
    name = deprecated_name,
    convert = new_tool@convert,
    annotations = annotations
  )
  assign(deprecated_name, deprecated_tool, envir = btw_env)
}

rlang::on_load({
  # Session tools renamed to sessioninfo in 1.2.0
  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_session_platform_info",
    new_name = "btw_tool_sessioninfo_platform",
    version = "1.2.0"
  )

  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_session_package_info",
    new_name = "btw_tool_sessioninfo_package",
    version = "1.2.0"
  )

  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_session_check_package_installed",
    new_name = "btw_tool_sessioninfo_is_package_installed",
    version = "1.2.0"
  )

  # Search tools renamed to cran in 1.2.0
  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_search_packages",
    new_name = "btw_tool_cran_search",
    version = "1.2.0"
  )

  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_search_package_info",
    new_name = "btw_tool_cran_package",
    version = "1.2.0"
  )

  # Files tools renamed in 1.2.0
  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_files_list_files",
    new_name = "btw_tool_files_list",
    version = "1.2.0"
  )

  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_files_read_text_file",
    new_name = "btw_tool_files_read",
    version = "1.2.0"
  )

  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_files_write_text_file",
    new_name = "btw_tool_files_write",
    version = "1.2.0"
  )

  .btw_add_deprecated_tool(
    deprecated_name = "btw_tool_files_code_search",
    new_name = "btw_tool_files_search",
    version = "1.2.0"
  )
})
