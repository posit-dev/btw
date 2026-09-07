# Tool: List CRAN package versions

Lists the current CRAN version and archived package versions with their
release dates. Archive dates are taken from CRAN's package archive
index.

## Usage

``` r
btw_tool_cran_versions(
  package_name,
  after = NULL,
  before = NULL,
  `_intent` = ""
)
```

## Arguments

- package_name:

  The name of a package on CRAN.

- after:

  Only return releases on or after this ISO date (`YYYY-MM-DD`).

- before:

  Only return releases on or before this ISO date (`YYYY-MM-DD`).

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A data frame with the version, release date and timestamp, current
release status, and source tarball URL for each package release.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other cran tools:
[`btw_tool_cran_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_package.md),
[`btw_tool_cran_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_search.md)
