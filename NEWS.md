# btw (development version)

* `btw_tool_docs_help_page()` now uses markdown headings and sections for argument descriptions, rather than a table. This is considerably more token efficient when the argument descriptions have more than one paragraph and can't be converted into a markdown table (@jeanchristophe13v, #123).

* btw now removes large inline base64-encoded images, replacing them with a placeholder containing the image's alt text (@jeanchristophe13v, #119).

* `btw_app()` now works correctly with `{ellmer}` v0.4.0 (#121).

* `btw_app()` now shows more precise cost estimates when the cost is less than $0.10 (#121).

# btw 1.0.0

* Initial CRAN submission.
