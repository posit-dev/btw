# Changelog

## btw (development version)

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
