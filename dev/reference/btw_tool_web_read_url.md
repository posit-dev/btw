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

## Examples

``` r
if (FALSE) { # rlang::is_installed("chromote") && rlang::is_interactive()
btw_tool_web_read_url("https://www.r-project.org/")
btw_tool_web_read_url(
  "https://posit.co/blog/easy-tool-calls-with-ellmer-and-chatlas/"
)
}
```
