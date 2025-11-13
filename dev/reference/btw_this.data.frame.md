# Describe a data frame in plain text

Describe a data frame in plain text

## Usage

``` r
# S3 method for class 'data.frame'
btw_this(
  x,
  ...,
  format = c("skim", "glimpse", "print", "json"),
  max_rows = 5,
  max_cols = 100,
  package = NULL
)

# S3 method for class 'tbl'
btw_this(
  x,
  ...,
  format = c("skim", "glimpse", "print", "json"),
  max_rows = 5,
  max_cols = 100,
  package = NULL
)
```

## Arguments

- x:

  A data frame or tibble.

- ...:

  Additional arguments are silently ignored.

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

## Value

A character vector containing a representation of the data frame. Will
error if the named data frame is not found in the environment.

## Functions

- `btw_this(data.frame)`: Summarize a data frame.

- `btw_this(tbl)`: Summarize a `tbl`.

## See also

[`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md)

Other btw formatting methods:
[`btw_this()`](https://posit-dev.github.io/btw/dev/reference/btw_this.md),
[`btw_this.character()`](https://posit-dev.github.io/btw/dev/reference/btw_this.character.md),
[`btw_this.environment()`](https://posit-dev.github.io/btw/dev/reference/btw_this.environment.md)

Other btw formatting methods:
[`btw_this()`](https://posit-dev.github.io/btw/dev/reference/btw_this.md),
[`btw_this.character()`](https://posit-dev.github.io/btw/dev/reference/btw_this.character.md),
[`btw_this.environment()`](https://posit-dev.github.io/btw/dev/reference/btw_this.environment.md)

## Examples

``` r
btw_this(mtcars)
#> [1] "```json"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
#> [2] "{\"n_cols\":11,\"n_rows\":32,\"groups\":[],\"class\":\"data.frame\",\"columns\":{\"mpg\":{\"variable\":\"mpg\",\"type\":\"numeric\",\"mean\":20.0906,\"sd\":6.0269,\"p0\":10.4,\"p25\":15.425,\"p50\":19.2,\"p75\":22.8,\"p100\":33.9},\"cyl\":{\"variable\":\"cyl\",\"type\":\"numeric\",\"mean\":6.1875,\"sd\":1.7859,\"p0\":4,\"p25\":4,\"p50\":6,\"p75\":8,\"p100\":8},\"disp\":{\"variable\":\"disp\",\"type\":\"numeric\",\"mean\":230.7219,\"sd\":123.9387,\"p0\":71.1,\"p25\":120.825,\"p50\":196.3,\"p75\":326,\"p100\":472},\"hp\":{\"variable\":\"hp\",\"type\":\"numeric\",\"mean\":146.6875,\"sd\":68.5629,\"p0\":52,\"p25\":96.5,\"p50\":123,\"p75\":180,\"p100\":335},\"drat\":{\"variable\":\"drat\",\"type\":\"numeric\",\"mean\":3.5966,\"sd\":0.5347,\"p0\":2.76,\"p25\":3.08,\"p50\":3.695,\"p75\":3.92,\"p100\":4.93},\"wt\":{\"variable\":\"wt\",\"type\":\"numeric\",\"mean\":3.2172,\"sd\":0.9785,\"p0\":1.513,\"p25\":2.5812,\"p50\":3.325,\"p75\":3.61,\"p100\":5.424},\"qsec\":{\"variable\":\"qsec\",\"type\":\"numeric\",\"mean\":17.8487,\"sd\":1.7869,\"p0\":14.5,\"p25\":16.8925,\"p50\":17.71,\"p75\":18.9,\"p100\":22.9},\"vs\":{\"variable\":\"vs\",\"type\":\"numeric\",\"mean\":0.4375,\"sd\":0.504,\"p0\":0,\"p25\":0,\"p50\":0,\"p75\":1,\"p100\":1},\"am\":{\"variable\":\"am\",\"type\":\"numeric\",\"mean\":0.4062,\"sd\":0.499,\"p0\":0,\"p25\":0,\"p50\":0,\"p75\":1,\"p100\":1},\"gear\":{\"variable\":\"gear\",\"type\":\"numeric\",\"mean\":3.6875,\"sd\":0.7378,\"p0\":3,\"p25\":3,\"p50\":4,\"p75\":4,\"p100\":5},\"carb\":{\"variable\":\"carb\",\"type\":\"numeric\",\"mean\":2.8125,\"sd\":1.6152,\"p0\":1,\"p25\":2,\"p50\":2,\"p75\":4,\"p100\":8}}}"
#> [3] "```"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       

btw_this(mtcars, format = "print")
#> [1] "    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb"
#> [2] "  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>"
#> [3] "1  21       6   160   110  3.9   2.62  16.5     0     1     4     4"
#> [4] "2  21       6   160   110  3.9   2.88  17.0     0     1     4     4"
#> [5] "3  22.8     4   108    93  3.85  2.32  18.6     1     1     4     1"
#> [6] "4  21.4     6   258   110  3.08  3.22  19.4     1     0     3     1"
#> [7] "5  18.7     8   360   175  3.15  3.44  17.0     0     0     3     2"
#> attr(,"class")
#> [1] "btw_captured" "character"   

btw_this(mtcars, format = "json")
#> [1] "```json"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
#> [2] "[\n  {\"mpg\":21,\"cyl\":6,\"disp\":160,\"hp\":110,\"drat\":3.9,\"wt\":2.62,\"qsec\":16.46,\"vs\":0,\"am\":1,\"gear\":4,\"carb\":4},\n  {\"mpg\":21,\"cyl\":6,\"disp\":160,\"hp\":110,\"drat\":3.9,\"wt\":2.875,\"qsec\":17.02,\"vs\":0,\"am\":1,\"gear\":4,\"carb\":4},\n  {\"mpg\":22.8,\"cyl\":4,\"disp\":108,\"hp\":93,\"drat\":3.85,\"wt\":2.32,\"qsec\":18.61,\"vs\":1,\"am\":1,\"gear\":4,\"carb\":1},\n  {\"mpg\":21.4,\"cyl\":6,\"disp\":258,\"hp\":110,\"drat\":3.08,\"wt\":3.215,\"qsec\":19.44,\"vs\":1,\"am\":0,\"gear\":3,\"carb\":1},\n  {\"mpg\":18.7,\"cyl\":8,\"disp\":360,\"hp\":175,\"drat\":3.15,\"wt\":3.44,\"qsec\":17.02,\"vs\":0,\"am\":0,\"gear\":3,\"carb\":2}\n]"
#> [3] "```"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         
```
