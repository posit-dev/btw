# Tool: List files

Tool: List files

## Usage

``` r
btw_tool_files_list_files(
  path = NULL,
  type = c("any", "file", "directory"),
  regexp = "",
  `_intent` = ""
)
```

## Arguments

- path:

  Path to a directory or file for which to get information. The `path`
  must be in the current working directory. If `path` is a directory, we
  use [`fs::dir_info()`](https://fs.r-lib.org/reference/dir_ls.html) to
  list information about files and directories in `path` (use `type` to
  pick only one or the other). If `path` is a file, we show information
  about that file.

- type:

  File type(s) to return, one of `"any"` or `"file"` or `"directory"`.

- regexp:

  A regular expression (e.g. `[.]csv$`) passed on to
  [`grep()`](https://rdrr.io/r/base/grep.html) to filter paths.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a character table of file information.

## See also

Other Tools:
[`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md),
[`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md),
[`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md),
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read_text_file.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md),
[`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md),
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
[`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md),
[`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md),
[`btw_tool_search_packages()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_search_packages.md),
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md),
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md),
[`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
withr::with_tempdir({
  write.csv(mtcars, "mtcars.csv")

  btw_tool_files_list_files(type = "file")
})
#> <btw::BtwToolResult>
#>  @ value  : chr "| path | type | size | modification_time |\n|------|------|------|-------------------|\n| mtcars.csv | file | 1"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ data   : tibble [1 × 4] (S3: tbl_df/tbl/data.frame)
#>  ..  ..$ path             : 'fs_path' chr "mtcars.csv"
#>  ..  ..$ type             : Factor w/ 8 levels "any","block_device",..: 7
#>  ..  ..$ size             : 'fs_bytes' num 1.74K
#>  ..  ..$ modification_time: POSIXct[1:1], format: "2025-12-15 18:33:15"
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "| path | type | size | modification_time |\n|------|------|------|-------------------|\n| mtcars.csv | file | 1"| __truncated__
#>  @ request: NULL
```
