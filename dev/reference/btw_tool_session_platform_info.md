# Tool: Describe user's platform

Describes the R version, operating system, and language and locale
settings for the user's system. When using
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
or
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md),
this information is automatically included in the system prompt.

## Usage

``` r
btw_tool_session_platform_info(`_intent` = "")
```

## Arguments

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a string describing the user's platform.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other Tools:
[`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md),
[`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md),
[`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md),
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md),
[`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list_files.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read_text_file.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md),
[`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md),
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
[`btw_tool_search_packages()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_search_packages.md),
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md),
[`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
btw_tool_session_platform_info()
#> <btw::BtwSessionInfoToolResult>
#>  @ value  : chr "<system_info>\nR_VERSION: R version 4.5.2 (2025-10-31)\nOS: Ubuntu 24.04.3 LTS\nSYSTEM: x86_64, linux-gnu\nUI: "| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 9
#>  .. $ r_version:: chr "R version 4.5.2 (2025-10-31)"
#>  .. $ os:       : chr "Ubuntu 24.04.3 LTS"
#>  .. $ system:   : chr "x86_64, linux-gnu"
#>  .. $ ui:       : chr "X11"
#>  .. $ language: : chr "en"
#>  .. $ locale:   : chr "C"
#>  .. $ encoding: : chr "C.UTF-8"
#>  .. $ timezone: : chr "UTC"
#>  .. $ date:     : chr "Monday, December  8, 2025 (2025-12-08)"
#>  .. - attr(*, "class")= chr [1:2] "platform_info" "list"
#>  @ request: NULL
```
