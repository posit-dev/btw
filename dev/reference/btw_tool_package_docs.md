# Tool: Describe R package documentation

These functions describe package documentation in plain text.

### Additional Examples

Show a list of available vignettes in the `dplyr` package:

    btw_tool_docs_available_vignettes("dplyr")

Get the introductory vignette for the `dplyr` package:

    btw_tool_docs_vignette("dplyr")

Get a specific vignette, such as the programming vignette for the
`dplyr` package:

    btw_tool_docs_vignette("dplyr", "programming")

## Usage

``` r
btw_tool_docs_package_help_topics(package_name, `_intent` = "")

btw_tool_docs_help_page(topic, package_name = "", `_intent` = "")

btw_tool_docs_available_vignettes(package_name, `_intent` = "")

btw_tool_docs_vignette(package_name, vignette = package_name, `_intent` = "")
```

## Arguments

- package_name:

  The name of the package as a string, e.g. `"shiny"`.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

- topic:

  The `topic_id` or `alias` of the help page, e.g. `"withProgress"` or
  `"incProgress"`. Find `topic_id`s or `alias`es using
  `get_package_help()`.

- vignette:

  The name (or index) of the vignette to retrieve. Defaults to the
  "intro" vignette to the package (by the same rules as pkgdown.)

## Value

- `btw_tool_docs_package_help_topics()` returns the `topic_id`, `title`,
  and `aliases` fields for every topic in a package's documentation as a
  json-formatted string.

- `btw_tool_docs_help_page()` returns the help-page for a package topic
  as a string.

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

Other docs tools:
[`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md)

## Examples

``` r
btw_tool_docs_package_help_topics("btw")
#> <btw::BtwToolResult>
#>  @ value  : chr [1:3] "```json" ...
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ data   : tibble [40 × 3] (S3: tbl_df/tbl/data.frame)
#>  ..  ..$ topic_id: chr [1:40] "btw" "btw-package" "btw_client" "btw_task_create_btw_md" ...
#>  ..  ..$ title   : chr [1:40] "Plain-text descriptions of R objects" "btw: A Toolkit for Connecting R and Large Language Models" "Create a btw-enhanced ellmer chat client" "Task: Initialize Project Context File" ...
#>  ..  ..$ aliases :List of 40
#>  ..  .. ..$ : 'AsIs' chr "btw"
#>  ..  .. ..$ : 'AsIs' chr "btw-package"
#>  ..  .. ..$ : 'AsIs' chr [1:2] "btw_client" "btw_app"
#>  ..  .. ..$ : 'AsIs' chr "btw_task_create_btw_md"
#>  ..  .. ..$ : 'AsIs' chr "btw_task_create_readme"
#>  ..  .. ..$ : 'AsIs' chr "btw_this"
#>  ..  .. ..$ : 'AsIs' chr "btw_this.character"
#>  ..  .. ..$ : 'AsIs' chr [1:2] "btw_this.data.frame" "btw_this.tbl"
#>  ..  .. ..$ : 'AsIs' chr "btw_this.environment"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_docs_package_news"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_env_describe_data_frame"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_env_describe_environment"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_files_code_search"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_files_list_files"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_files_read_text_file"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_files_write_text_file"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_branch_checkout"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_branch_create"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_branch_list"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_commit"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_diff"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_log"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_git_status"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_github"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_ide_read_current_editor"
#>  ..  .. ..$ : 'AsIs' chr [1:5] "btw_tool_package_docs" "btw_tool_docs_package_help_topics" "btw_tool_docs_help_page" "btw_tool_docs_available_vignettes" ...
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_pkg_check"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_pkg_coverage"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_pkg_document"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_pkg_test"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_run_r"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_search_package_info"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_search_packages"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_session_check_package_installed"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_session_package_info"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_session_platform_info"
#>  ..  .. ..$ : 'AsIs' chr "btw_tool_web_read_url"
#>  ..  .. ..$ : 'AsIs' chr "btw_tools"
#>  ..  .. ..$ : 'AsIs' chr [1:3] "mcp" "btw_mcp_server" "btw_mcp_session"
#>  ..  .. ..$ : 'AsIs' chr [1:2] "use_btw_md" "edit_btw_md"
#>  .. $ display:List of 2
#>  ..  ..$ title   : chr "{btw} Help Topics"
#>  ..  ..$ markdown: chr "| topic_id | title | aliases |\n|----------|-------|---------|\n| btw | Plain-text descriptions of R objects | "| __truncated__
#>  @ request: NULL

btw_tool_docs_help_page("btw", "btw")
#> <btw::BtwHelpPageToolResult>
#>  @ value  : chr [1:94] "## `help(package = \"btw\", \"btw\")`" "" ...
#>  @ error  : NULL
#>  @ extra  :List of 4
#>  .. $ help_text: chr [1:93] "" "### Plain-text descriptions of R objects" "" "#### Description" ...
#>  .. $ topic    : chr "btw"
#>  .. $ package  : chr "btw"
#>  .. $ display  :List of 2
#>  ..  ..$ title   : 'html' chr "<code>?btw::btw</code>"
#>  ..  .. ..- attr(*, "html")= logi TRUE
#>  ..  ..$ markdown: chr "\n### Plain-text descriptions of R objects\n\n#### Description\n\nThis function allows you to quickly describe "| __truncated__
#>  @ request: NULL
```
