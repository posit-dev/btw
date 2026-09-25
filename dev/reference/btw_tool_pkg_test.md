# Tool: Run package tests

Run package tests using
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html).
Optionally filter tests by name pattern. The default `"minimal"`
reporter returns failures and a final summary without per-file progress,
which suits non-streaming tool clients. Use `"compact"` to include file
starts, per-file results, and timings, or pass a testthat reporter name.

## Usage

``` r
btw_tool_pkg_test(
  pkg = ".",
  filter = NULL,
  reporter = "minimal",
  `_intent` = ""
)
```

## Arguments

- pkg:

  Path to package directory. Defaults to '.'. Must be within current
  working directory.

- filter:

  Optional regex to filter test files. Example: 'helper' matches
  'test-helper.R'.

- reporter:

  Either `"minimal"` (the default), `"compact"` (per-file progress and
  timing), or a testthat reporter name passed to
  [`devtools::test()`](https://devtools.r-lib.org/reference/test.html).

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
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
[`btw_tool_pkg_load_all()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_load_all.md)
