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

Other session tools:
[`btw_tool_session_check_package_installed()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_check_package_installed.md),
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md)

## Examples

``` r
btw_tool_session_platform_info()
#> <btw::BtwSessionInfoToolResult>
#>  @ value  : chr "<system_info>\nR_VERSION: R version 4.5.2 (2025-10-31)\nOS: Ubuntu 24.04.3 LTS\nSYSTEM: x86_64, linux-gnu\nLANG"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 8
#>  .. $ r_version:: chr "R version 4.5.2 (2025-10-31)"
#>  .. $ os:       : chr "Ubuntu 24.04.3 LTS"
#>  .. $ system:   : chr "x86_64, linux-gnu"
#>  .. $ language: : chr "en"
#>  .. $ locale:   : chr "C"
#>  .. $ encoding: : chr "C.UTF-8"
#>  .. $ timezone: : chr "UTC"
#>  .. $ date:     : chr "Saturday, December 20, 2025 (2025-12-20)"
#>  .. - attr(*, "class")= chr [1:2] "platform_info" "list"
#>  @ request: NULL
```
