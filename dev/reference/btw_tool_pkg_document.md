# Tool: Generate package documentation

Generate package documentation using
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html).
This runs roxygen2 on the package to create/update man pages and
`NAMESPACE`.

## Usage

``` r
btw_tool_pkg_document(pkg = ".", `_intent` = "")
```

## Arguments

- pkg:

  Path to package directory. Defaults to `"."`. Must be within current
  working directory.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

The output from
[`devtools::document()`](https://devtools.r-lib.org/reference/document.html).

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other pkg tools:
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md),
[`btw_tool_pkg_load_all()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_load_all.md),
[`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md)
