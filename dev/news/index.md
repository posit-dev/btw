# Changelog

## btw (development version)

- New “pkg” tool group with package development tools:
  [`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
  [`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
  [`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md)
  and
  [`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md)
  provide LLMs with the ability to document, check, test, and check test
  coverage of R packages during development
  ([\#133](https://github.com/posit-dev/btw/issues/133),
  [\#136](https://github.com/posit-dev/btw/issues/136)).

- New
  [`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md)
  tool allows LLMs to run R code and to see the output, including of
  plots. Because this tool lets LLMs run R arbitrary R code in the
  global environment (which can be great but can also have security
  implications), it is opt-in and disabled by default. See
  [`?btw_tool_run_r`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md)
  for more details
  ([\#126](https://github.com/posit-dev/btw/issues/126)).

- [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)
  now uses markdown headings and sections for argument descriptions,
  rather than a table. This is considerably more token efficient when
  the argument descriptions have more than one paragraph and can’t be
  converted into a markdown table
  ([@jeanchristophe13v](https://github.com/jeanchristophe13v),
  [\#123](https://github.com/posit-dev/btw/issues/123)).

- btw now removes large inline base64-encoded images, replacing them
  with a placeholder containing the image’s alt text
  ([@jeanchristophe13v](https://github.com/jeanchristophe13v),
  [\#119](https://github.com/posit-dev/btw/issues/119)).

- [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now works correctly with [ellmer](https://ellmer.tidyverse.org) v0.4.0
  ([\#121](https://github.com/posit-dev/btw/issues/121)).

- [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now shows more precise cost estimates when the cost is less than
  \$0.10 ([\#121](https://github.com/posit-dev/btw/issues/121)).

## btw 1.0.0

CRAN release: 2025-11-04

- Initial CRAN submission.
