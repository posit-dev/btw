# Tool: Read a file

Tool: Read a file

## Usage

``` r
btw_tool_files_read_text_file(
  path,
  line_start = 1,
  line_end = 1000,
  `_intent` = ""
)
```

## Arguments

- path:

  Path to a file for which to get information. The `path` must be in the
  current working directory.

- line_start:

  Starting line to read, defaults to 1 (starting from the first line).

- line_end:

  Ending line to read, defaults to 1000. Change only this value if you
  want to read more or fewer lines. Use in combination with `line_start`
  to read a specific line range of the file.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a character vector of lines from the file.

## See also

Other Tools:
[`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md),
[`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md),
[`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md),
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md),
[`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list_files.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md),
[`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md),
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md),
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

  btw_tool_files_read_text_file("mtcars.csv", line_end = 5)
})
#> <btw::BtwTextFileToolResult>
#>  @ value  : chr "```csv\n\"\",\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"\n\"Mazda "| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ path   : 'fs_path' chr "mtcars.csv"
#>  .. $ display:List of 2
#>  ..  ..$ markdown: chr "```csv\n\"\",\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"\n\"Mazda "| __truncated__
#>  ..  ..$ title   : 'html' chr "Read <code>mtcars.csv</code>"
#>  ..  .. ..- attr(*, "html")= logi TRUE
#>  @ request: NULL
```
