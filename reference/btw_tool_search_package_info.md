# Tool: Describe a CRAN package

Describes a CRAN package using
[`pkgsearch::cran_package()`](https://r-hub.github.io/pkgsearch/reference/cran_package.html).

## Usage

``` r
btw_tool_search_package_info(package_name, `_intent` = "")
```

## Arguments

- package_name:

  The name of a package on CRAN.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

An info sheet about the package.

## Examples

``` r
cli::cat_line(
  btw_this(pkgsearch::cran_package("anyflights"))
)
#> ### anyflights (v0.3.5) -- Query 'nycflights13'-Like Air Travel Data for Given Years and
#> Airports
#> 
#> #### Description
#> 
#> Supplies a set of functions to query air travel data for user-
#> specified years and airports. Datasets include on-time flights, airlines,
#> airports, planes, and weather.
#> 
#> #### Details
#> 
#> * License: CC0
#> * Home: https://github.com/simonpcouch/anyflights,https://simonpcouch.github.io/anyflights/
#> * Issue Tracker: https://github.com/simonpcouch/anyflights/issues
#> * Last Updated: 2025-01-10
#> 
#> #### Dependencies
#> 
#> * Depends
#>   * R >= 3.5.0
#> * Imports
#>   * httr
#>   * dplyr
#>   * readr
#>   * utils
#>   * lubridate
#>   * vroom
#>   * glue
#>   * purrr
#>   * stringr
#>   * curl
#>   * usethis
#>   * roxygen2
#>   * progress
#>   * tidyr
#> * Suggests
#>   * testthat
#>   * nycflights13
#>   * covr
#> 
#> #### Author Information
#> 
#> Simon P. Couch [aut, cre],
#> Hadley Wickham [ctb],
#> Jay Lee [ctb],
#> Dennis Irorere [ctb]
#> 
#> **Maintainer**: Simon P. Couch <simonpatrickcouch@gmail.com>
```
