# Tool: Describe data frame

Tool: Describe data frame

## Usage

``` r
btw_tool_env_describe_data_frame(
  data_frame,
  format = c("skim", "glimpse", "print", "json"),
  max_rows = 5,
  max_cols = 100,
  package = NULL,
  `_intent` = ""
)
```

## Arguments

- data_frame:

  The data frame to describe

- format:

  One of `"skim"`, `"glimpse"`, `"print"`, or `"json"`.

  - `"skim"` is the most information-dense format for describing the
    data. It uses and returns the same information as
    [`skimr::skim()`](https://docs.ropensci.org/skimr/reference/skim.html)
    but formatting as a JSON object that describes the dataset.

  - To glimpse the data column-by-column, use `"glimpse"`. This is
    particularly helpful for getting a sense of data frame column names,
    types, and distributions, when pairings of entries in individual
    rows aren't particularly important.

  - To just print out the data frame, use
    [`print()`](https://rdrr.io/r/base/print.html).

  - To get a json representation of the data, use `"json"`. This is
    particularly helpful when the pairings among entries in specific
    rows are important to demonstrate.

- max_rows:

  The maximum number of rows to show in the data frame. Only applies
  when `format = "json"`.

- max_cols:

  The maximum number of columns to show in the data frame. Only applies
  when `format = "json"`.

- package:

  The name of the package that provides the data set. If not provided,
  `data_frame` must be loaded in the current environment, or may also be
  inferred from the name of the data frame, e.g. `"dplyr::storms"`.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A character vector containing a representation of the data frame. Will
error if the named data frame is not found in the environment.

## See also

[`btw_this.data.frame()`](https://posit-dev.github.io/btw/dev/reference/btw_this.data.frame.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other env tools:
[`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md)

## Examples

``` r
btw_tool_env_describe_data_frame(mtcars)
#> <btw::BtwToolResult>
#>  @ value  : chr [1:3] "```json" ...
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ data   :'data.frame':  32 obs. of  11 variables:
#>  ..  ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
#>  ..  ..$ cyl : num [1:32] 6 6 4 6 8 6 8 4 4 6 ...
#>  ..  ..$ disp: num [1:32] 160 160 108 258 360 ...
#>  ..  ..$ hp  : num [1:32] 110 110 93 110 175 105 245 62 95 123 ...
#>  ..  ..$ drat: num [1:32] 3.9 3.9 3.85 3.08 3.15 2.76 3.21 3.69 3.92 3.92 ...
#>  ..  ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
#>  ..  ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
#>  ..  ..$ vs  : num [1:32] 0 0 1 1 0 1 0 1 1 1 ...
#>  ..  ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
#>  ..  ..$ gear: num [1:32] 4 4 4 3 3 3 3 4 4 4 ...
#>  ..  ..$ carb: num [1:32] 4 4 1 1 2 1 4 2 2 4 ...
#>  .. $ display:List of 1
#>  ..  ..$ title: chr "View Data Frame"
#>  @ request: NULL
```
