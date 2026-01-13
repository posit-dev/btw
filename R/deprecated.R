# Deprecated btw tool functions
#
# These functions have been renamed. The old names continue to work but emit
# deprecation warnings. Use the new names for new code.

# Session tools renamed to sessioninfo group --------------------------------

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_session_platform_info()` was renamed to
#' [btw_tool_sessioninfo_platform()] in btw 1.2.0.
#' @rdname btw_tool_sessioninfo_platform
#' @keywords internal
#' @export
btw_tool_session_platform_info <- function(`_intent` = NULL) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_session_platform_info()",
    "btw_tool_sessioninfo_platform()"
  )
  btw_tool_sessioninfo_platform_impl()
}

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_session_package_info()` was renamed to
#' [btw_tool_sessioninfo_package()] in btw 1.2.0.
#' @rdname btw_tool_sessioninfo_package
#' @keywords internal
#' @export
btw_tool_session_package_info <- function(
    packages = "attached",
    dependencies = "",
    `_intent` = NULL
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_session_package_info()",
    "btw_tool_sessioninfo_package()"
  )
  btw_tool_sessioninfo_package_impl(
    packages = packages,
    dependencies = dependencies
  )
}

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_session_check_package_installed()` was renamed to
#' [btw_tool_sessioninfo_is_package_installed()] in btw 1.2.0.
#' @rdname btw_tool_sessioninfo_is_package_installed
#' @keywords internal
#' @export
btw_tool_session_check_package_installed <- function(
    package_name,
    `_intent` = NULL
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_session_check_package_installed()",
    "btw_tool_sessioninfo_is_package_installed()"
  )
  btw_tool_sessioninfo_is_package_installed_impl(
    package_name = package_name
  )
}

# Search tools renamed to cran group ----------------------------------------

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_search_packages()` was renamed to
#' [btw_tool_cran_search()] in btw 1.2.0.
#' @rdname btw_tool_cran_search
#' @keywords internal
#' @export
btw_tool_search_packages <- function(
    query,
    format = c("short", "long"),
    n_results = NULL,
    `_intent` = NULL
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_search_packages()",
    "btw_tool_cran_search()"
  )
  btw_tool_cran_search_impl(
    query = query,
    format = format,
    n_results = n_results
  )
}

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_search_package_info()` was renamed to
#' [btw_tool_cran_package()] in btw 1.2.0.
#' @rdname btw_tool_cran_package
#' @keywords internal
#' @export
btw_tool_search_package_info <- function(package_name, `_intent` = NULL) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_search_package_info()",
    "btw_tool_cran_package()"
  )
  btw_tool_cran_package_impl(package_name = package_name)
}

# Files tools renamed with simpler names ------------------------------------

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_files_list_files()` was renamed to
#' [btw_tool_files_list()] in btw 1.2.0.
#' @rdname btw_tool_files_list
#' @keywords internal
#' @export
btw_tool_files_list_files <- function(
    path = NULL,
    type = c("any", "file", "directory"),
    regexp = "",
    `_intent` = NULL
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_files_list_files()",
    "btw_tool_files_list()"
  )
  btw_tool_files_list_impl(
    path = path,
    type = type,
    regexp = regexp
  )
}

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_files_read_text_file()` was renamed to
#' [btw_tool_files_read()] in btw 1.2.0.
#' @rdname btw_tool_files_read
#' @keywords internal
#' @export
btw_tool_files_read_text_file <- function(
    path,
    line_start = 1,
    line_end = 1000,
    `_intent` = NULL
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_files_read_text_file()",
    "btw_tool_files_read()"
  )
  btw_tool_files_read_impl(
    path = path,
    line_start = line_start,
    line_end = line_end
  )
}

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_files_write_text_file()` was renamed to
#' [btw_tool_files_write()] in btw 1.2.0.
#' @rdname btw_tool_files_write
#' @keywords internal
#' @export
btw_tool_files_write_text_file <- function(
    path,
    content,
    `_intent` = NULL
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_files_write_text_file()",
    "btw_tool_files_write()"
  )
  btw_tool_files_write_impl(path = path, content = content)
}

#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `btw_tool_files_code_search()` was renamed to
#' [btw_tool_files_search()] in btw 1.2.0.
#' @rdname btw_tool_files_search
#' @keywords internal
#' @export
btw_tool_files_code_search <- function(
    term,
    limit = 100,
    case_sensitive = TRUE,
    use_regex = FALSE,
    show_lines = FALSE,
    `_intent` = ""
) {
  lifecycle::deprecate_warn(
    "1.2.0",
    "btw_tool_files_code_search()",
    "btw_tool_files_search()"
  )
  project_code_search <- btw_tool_files_search_factory()
  project_code_search(
    term = term,
    limit = limit,
    case_sensitive = case_sensitive,
    use_regex = use_regex,
    show_lines = show_lines
  )
}
