# Tool: Edit a text file

Tool: Edit a text file

## Usage

``` r
btw_tool_files_edit(path, edits, `_intent` = "")
```

## Arguments

- path:

  Path to the file to edit. The `path` must be in the current working
  directory.

- edits:

  A list of edit operations. Each edit is a named list with `action`,
  `line`, and `content` fields.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a message confirming the edits were applied.

## See also

Other files tools:
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md),
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)
