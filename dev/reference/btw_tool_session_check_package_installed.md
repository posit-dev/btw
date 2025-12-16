# Tool: Check if a package is installed

Checks if a package is installed in the current session. If the package
is installed, it returns the version number. If not, it suggests
packages with similar names to help the LLM resolve typos.

## Usage

``` r
btw_tool_session_check_package_installed(package_name, `_intent` = "")
```

## Arguments

- package_name:

  The name of the package.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A message indicating whether the package is installed and its version,
or an error indicating that the package is not installed.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other session tools:
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md),
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md)

## Examples

``` r
btw_tool_session_check_package_installed("dplyr")@value
#> [1] "Package `dplyr` version 1.1.4 is installed."

tryCatch(
  btw_tool_session_check_package_installed("dplry"),
  error = function(err) {
    cat(conditionMessage(err))
  }
)
#> Package dplry is not installed.
#> ℹ Did you mean "dplR", "DALY", "dblr", "dlr", or "dpcR"?
```
