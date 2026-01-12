# Tool: Load package code

Load package code using
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
in a separate R process via
[`callr::r()`](https://callr.r-lib.org/reference/r.html). This verifies
that the package code loads without syntax errors and triggers
recompilation of any compiled code (C, C++, etc.).

## Usage

``` r
btw_tool_pkg_load_all(pkg = ".", `_intent` = "")
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
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html).

## Details

**Important:** This tool runs `load_all()` in an isolated R process and
does NOT load the package code into your current R session. If you need
to load the package code in your current session for interactive use,
use the run R code tool to call
[`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
directly.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other pkg tools:
[`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
[`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md),
[`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
[`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md)
