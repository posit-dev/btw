# Tool: Replace exact strings in a text file

Tool: Replace exact strings in a text file

## Usage

``` r
btw_tool_files_replace(
  path,
  old_string,
  new_string,
  replace_all = FALSE,
  `_intent` = ""
)
```

## Arguments

- path:

  Path to the file to edit. The `path` must be in the current working
  directory.

- old_string:

  The exact string to find in the file. Must be unique unless
  `replace_all` is `TRUE`.

- new_string:

  The replacement string. Must differ from `old_string`.

- replace_all:

  If `TRUE`, replace all occurrences of `old_string`. Defaults to
  `FALSE`, which requires exactly one occurrence.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a message confirming the replacement was applied.

## See also

Other files tools:
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md),
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)
