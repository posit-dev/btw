# Tool: Run R CMD check on a package

Run R CMD check on a package using
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html).
This performs comprehensive checks on the package structure, code, and
documentation.

## Usage

``` r
btw_tool_pkg_check(pkg = ".", `_intent` = "")
```

## Arguments

- pkg:

  Path to package directory. Defaults to '.'. Must be within current
  working directory.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

The output from
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html).

## Details

The check runs with `remote = TRUE`, `cran = TRUE`, `manual = FALSE`,
and `error_on = "never"` to provide comprehensive feedback without
failing.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other pkg tools:
[`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md),
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
[`btw_tool_pkg_load_all()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_load_all.md),
[`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md)
