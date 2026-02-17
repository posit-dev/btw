# Tool: Write a text file

Tool: Write a text file

## Usage

``` r
btw_tool_files_write(path, content, `_intent` = "")
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
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md),
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md),
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md)

## Examples

``` r
withr::with_tempdir({
  btw_tool_files_write("example.txt", "Hello\nWorld!")
  readLines("example.txt")
})
#> Warning: incomplete final line found on 'example.txt'
#> [1] "Hello"  "World!"
```
