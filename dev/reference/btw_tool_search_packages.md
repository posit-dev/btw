# Tool: Search for an R package on CRAN

Uses
[`pkgsearch::pkg_search()`](https://r-hub.github.io/pkgsearch/reference/pkg_search.html)
to search for R packages on CRAN.

## Usage

``` r
btw_tool_search_packages(
  query,
  format = c("short", "long"),
  n_results = NULL,
  `_intent` = ""
)
```

## Arguments

- query:

  Search query string. If this argument is missing or `NULL`, then the
  results of the last query are printed, in *short* and *long* formats,
  in turns for successive `pkg_search()` calls. If this argument is
  missing, then all other arguments are ignored.

- format:

  Default formatting of the results. *short* only outputs the name and
  title of the packages, *long* also prints the author, last version,
  full description and URLs. Note that this only affects the default
  printing, and you can still inspect the full results, even if you
  specify *short* here.

- n_results:

  Number of search results to include. Defaults to 10 for 'short' format
  and 5 for 'long' format.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A listing of packages matching the search term.

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
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
[`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md),
[`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md),
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md),
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md),
[`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
# Copy pkgsearch results to the clipboard for use in any LLM app
btw(
  pkgsearch::pkg_search("network visualization", size = 1),
  clipboard = FALSE
)
#> Warning: Your package search query is too broad and returned too many results!
#> • It's likely the exact phrase in `query` wasn't found, so the search fell back
#>   to searching for the individual words in `query`.
#> ℹ Try removing common words like `data`, `API`, `tools`, `statistics`, etc. or
#>   find a more specific phrase.
#> ## Context
#> 
#> pkgsearch::pkg_search("network visualization", size = 1)
#> Found 3223 packages matching `network visualization`, showing 1 result.
#> 
#> | package | title | version | date | url | downloads_last_month |
#> |---------|-------|---------|------|-----|----------------------|
#> | DiagrammeR | Graph/Network Visualization | 1.0.11 | 2024-02-02 | https://rich-iannone.github.io/DiagrammeR/, https://github.com/rich-iannone/DiagrammeR | 74,154 |
btw(
  pkgsearch::pkg_search("network visualization", format = "long", size = 1),
  clipboard = FALSE
)
#> Warning: Your package search query is too broad and returned too many results!
#> • It's likely the exact phrase in `query` wasn't found, so the search fell back
#>   to searching for the individual words in `query`.
#> ℹ Try removing common words like `data`, `API`, `tools`, `statistics`, etc. or
#>   find a more specific phrase.
#> ## Context
#> 
#> pkgsearch::pkg_search(...)
#> Found 3223 packages matching `network visualization`, showing 1 result.
#> 
#> ### DiagrammeR (v1.0.11) -- Graph/Network Visualization
#> 
#> * Maintainer: Richard Iannone
#> * Homepage: https://rich-iannone.github.io/DiagrammeR/, https://github.com/rich-iannone/DiagrammeR
#> * Date: 2024-02-02
#> * Downloads Last Month: 74,154
#> 
#> Build graph/network structures using functions for stepwise
#> addition and deletion of nodes and edges. Work with data available in
#> tables for bulk addition of nodes, edges, and associated metadata. Use
#> graph selections and traversals to apply changes to specific nodes or
#> edges. A wide selection of graph algorithms allow for the analysis of
#> graphs. Visualize the graphs and take advantage of any aesthetic
#> properties assigned to nodes and edges.
```
