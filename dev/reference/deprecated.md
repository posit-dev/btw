# Deprecated functions

**\[deprecated\]**

These functions have been renamed. The old names continue to work but
emit deprecation warnings. Use the new names for new code.

## Usage

``` r
btw_tool_session_platform_info(`_intent` = "")

btw_tool_session_package_info(
  packages = "attached",
  dependencies = "",
  `_intent` = ""
)

btw_tool_session_check_package_installed(package_name, `_intent` = "")

btw_tool_search_packages(
  query,
  format = c("short", "long"),
  n_results = NULL,
  `_intent` = ""
)

btw_tool_search_package_info(package_name, `_intent` = "")

btw_tool_files_list_files(
  path = NULL,
  type = c("any", "file", "directory"),
  regexp = "",
  `_intent` = ""
)

btw_tool_files_read_text_file(
  path,
  line_start = 1,
  line_end = 1000,
  `_intent` = ""
)

btw_tool_files_write_text_file(path, content, `_intent` = "")

btw_tool_files_code_search(
  term,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE,
  show_lines = FALSE,
  `_intent` = ""
)
```

## Functions

- `btw_tool_session_platform_info()`: `btw_tool_session_platform_info()`
  was renamed to
  [`btw_tool_sessioninfo_platform()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_platform.md)
  in btw 1.2.0.

- `btw_tool_session_package_info()`: `btw_tool_session_package_info()`
  was renamed to
  [`btw_tool_sessioninfo_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_package.md)
  in btw 1.2.0.

- `btw_tool_session_check_package_installed()`:
  `btw_tool_session_check_package_installed()` was renamed to
  [`btw_tool_sessioninfo_is_package_installed()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_is_package_installed.md)
  in btw 1.2.0.

- `btw_tool_search_packages()`: `btw_tool_search_packages()` was renamed
  to
  [`btw_tool_cran_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_search.md)
  in btw 1.2.0.

- `btw_tool_search_package_info()`: `btw_tool_search_package_info()` was
  renamed to
  [`btw_tool_cran_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_package.md)
  in btw 1.2.0.

- `btw_tool_files_list_files()`: `btw_tool_files_list_files()` was
  renamed to
  [`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md)
  in btw 1.2.0.

- `btw_tool_files_read_text_file()`: `btw_tool_files_read_text_file()`
  was renamed to
  [`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
  in btw 1.2.0.

- `btw_tool_files_write_text_file()`: `btw_tool_files_write_text_file()`
  was renamed to
  [`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)
  in btw 1.2.0.

- `btw_tool_files_code_search()`: `btw_tool_files_code_search()` was
  renamed to
  [`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md)
  in btw 1.2.0.
