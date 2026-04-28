# Tool: Search for an R package on CRAN

Uses
[`pkgsearch::pkg_search()`](https://r-hub.github.io/pkgsearch/reference/pkg_search.html)
to search for R packages on CRAN.

## Usage

``` r
btw_tool_cran_search(
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

Other cran tools:
[`btw_tool_cran_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_package.md)

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
#> Found 3485 packages matching `network visualization`, showing 1 result.
#> 
#> | package | title | version | date | url | downloads_last_month |
#> |---------|-------|---------|------|-----|----------------------|
#> | visNetwork | Network Visualization using 'vis.js' Library | 2.1.4 | 2025-09-04 | https://datastorm-open.github.io/visNetwork/ | 76,729 |
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
#> Found 3485 packages matching `network visualization`, showing 1 result.
#> 
#> ### visNetwork (v2.1.4) -- Network Visualization using 'vis.js' Library
#> 
#> * Maintainer: Benoit Thieurmel
#> * Homepage: https://datastorm-open.github.io/visNetwork/
#> * Date: 2025-09-04
#> * Downloads Last Month: 76,729
#> 
#> Provides an R interface to the 'vis.js' JavaScript charting
#> library. It allows an interactive visualization of networks.
```
