# Tool: Compute package test coverage

Compute test coverage for an R package using
[`covr::package_coverage()`](http://covr.r-lib.org/reference/package_coverage.md).
Returns either a file-level summary for the entire package or line-level
details for a specific file.

## Usage

``` r
btw_tool_pkg_coverage(pkg = ".", filename = NULL, `_intent` = "")
```

## Arguments

- pkg:

  Path to package directory. Defaults to `"."`. Must be within current
  working directory.

- filename:

  Optional filename to filter coverage results. If `NULL` (default),
  returns file-level summary for entire package. If provided, returns
  line-level results for the specified file.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A data frame with different structures depending on `filename`:

- When `filename = NULL`: Returns file-level summary with columns
  `filename` and `coverage` (percentage).

- When `filename` is specified: Returns line-level details with columns
  `filename`, `functions`, `line_start`, `line_end`, `is_covered`, and
  `med_hits`.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/reference/btw_tools.md)

Other pkg tools:
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_document.md),
[`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_test.md)
