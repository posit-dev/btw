# Tool: Run package tests

Run package tests using
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html).
Optionally filter tests by name pattern.

## Usage

``` r
btw_tool_pkg_test(pkg = ".", filter = NULL, `_intent` = "")
```

## Arguments

- pkg:

  Path to package directory. Defaults to '.'. Must be within current
  working directory.

- filter:

  Optional regex to filter test files. Example: 'helper' matches
  'test-helper.R'.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

The output from
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html).

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other pkg tools:
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md),
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md)
