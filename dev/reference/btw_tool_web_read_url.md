# Tool: Read a Web Page as Markdown

Tool: Read a Web Page as Markdown

## Usage

``` r
btw_tool_web_read_url(url, `_intent` = "")
```

## Arguments

- url:

  The URL of the web page to read.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a `BtwWebPageResult` object that inherits from
[ellmer::ContentToolResult](https://ellmer.tidyverse.org/reference/Content.html)
containing the markdown content of the web page.

## Details

You can control the maximum time to wait for the page to load by setting
the `btw.max_wait_for_page_load_s` option globally in your R session.

## See also

Other Tools:
[`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md),
[`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md),
[`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md),
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md),
[`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list_files.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read_text_file.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md),
[`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md),
[`btw_tool_package_docs`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
[`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md),
[`btw_tool_search_packages()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_search_packages.md),
[`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md),
[`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md),
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

## Examples

``` r
if (FALSE) { # rlang::is_installed("chromote") && rlang::is_interactive()
btw_tool_web_read_url("https://www.r-project.org/")
btw_tool_web_read_url(
  "https://posit.co/blog/easy-tool-calls-with-ellmer-and-chatlas/"
)
}
```
