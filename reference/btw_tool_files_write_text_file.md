# Tool: Write a text file

Tool: Write a text file

## Usage

``` r
btw_tool_files_write_text_file(path, content, `_intent` = "")
```

## Arguments

- path:

  Path to the file to write. The `path` must be in the current working
  directory.

- content:

  The text content to write to the file. This should be the complete
  content as the file will be overwritten.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a message confirming the file was written.

## See also

Other files tools:
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/reference/btw_tool_files_code_search.md),
[`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/reference/btw_tool_files_list_files.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read_text_file.md)

## Examples

``` r
withr::with_tempdir({
  btw_tool_files_write_text_file("example.txt", "Hello\nWorld!")
  readLines("example.txt")
})
#> Warning: incomplete final line found on 'example.txt'
#> [1] "Hello"  "World!"
```
