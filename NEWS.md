# btw (development version)

* New `btw_tool_run_r()` tool allows LLMs to run R code and to see the output, including of plots. Because this tool lets LLMs run R arbitrary R code in the global environment (which can be great but can also have security implications), it is opt-in and disabled by default. See `?btw_tool_run_r` for more details (#126).

* `btw_tool_docs_help_page()` now uses markdown headings and sections for argument descriptions, rather than a table. This is considerably more token efficient when the argument descriptions have more than one paragraph and can't be converted into a markdown table (@jeanchristophe13v, #123).

* btw now removes large inline base64-encoded images, replacing them with a placeholder containing the image's alt text (@jeanchristophe13v, #119).

* `btw_app()` now works correctly with `{ellmer}` v0.4.0 (#121).

* `btw_app()` now shows more precise cost estimates when the cost is less than $0.10 (#121).

# btw 1.0.0

* Initial CRAN submission.
