# Tool: Gather information about a package or currently loaded packages

Uses
[`sessioninfo::package_info()`](https://sessioninfo.r-lib.org/reference/package_info.html)
to provide information about the loaded, attached, or installed
packages. The primary use case is to verify that a package is installed;
check the version number of a specific packages; or determine which
packages are already in use in a session.

## Usage

``` r
btw_tool_session_package_info(
  packages = "attached",
  dependencies = "",
  `_intent` = ""
)
```

## Arguments

- packages:

  Which packages to show, or `"loaded"` to show all loaded packages,
  `"attached"` to show all attached packages, or `"installed"` to show
  all installed packages.

- dependencies:

  Whether to include the dependencies when listing package information.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a string describing the selected packages.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md),
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md)

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
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md),
[`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
btw_tool_session_package_info("btw")
#> <btw::BtwPackageInfoToolResult>
#>  @ value  : chr "```\n package * version    date (UTC) lib source\n btw     * 1.0.0.9000 2025-11-17 [1] local\n\n [1] /home/runn"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ data   :Classes ‘packages_info’ and 'data.frame':  1 obs. of  11 variables:
#>  ..  ..$ package      : chr "btw"
#>  ..  ..$ ondiskversion: chr "1.0.0.9000"
#>  ..  ..$ loadedversion: chr "1.0.0.9000"
#>  ..  ..$ path         : chr "/home/runner/work/_temp/Library/btw"
#>  ..  ..$ loadedpath   : chr "/home/runner/work/_temp/Library/btw"
#>  ..  ..$ attached     : logi TRUE
#>  ..  ..$ is_base      : logi FALSE
#>  ..  ..$ date         : chr "2025-11-17"
#>  ..  ..$ source       : chr "local"
#>  ..  ..$ md5ok        : logi NA
#>  ..  ..$ library      : Factor w/ 3 levels "/home/runner/work/_temp/Library",..: 1
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "| package | ondiskversion | loadedversion | path | loadedpath | attached | is_base | date | source | md5ok | li"| __truncated__
#>  @ request: NULL
```
