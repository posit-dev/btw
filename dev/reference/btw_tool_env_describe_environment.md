# Tool: Describe an environment

This tool can be used by the LLM to describe the contents of an R
session, i.e. the data frames and other objects loaded into the global
environment. This tool will only see variables that you've named and
created in the global environment, it cannot reach into package
namespaces, see which packages you have loaded, or access files on your
computer.

## Usage

``` r
btw_tool_env_describe_environment(items = NULL, `_intent` = "")
```

## Arguments

- items:

  Optional. A character vector of objects in the environment to
  describe.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A string describing the environment contents with `#>` prefixing each
object's printed representation.

## See also

[`btw_this.environment()`](https://posit-dev.github.io/btw/dev/reference/btw_this.environment.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other Tools:
[`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md),
[`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md),
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md),
[`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list_files.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read_text_file.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md),
[`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md),
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
[`btw_tool_search_packages()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_search_packages.md),
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md),
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md),
[`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
my_cars <- mtcars[mtcars$mpg > 25, ]
btw_tool_env_describe_environment("my_cars")
#> <btw::BtwToolResult>
#>  @ value  : NULL
#>  @ error  : NULL
#>  @ extra  : list()
#>  @ request: NULL
```
