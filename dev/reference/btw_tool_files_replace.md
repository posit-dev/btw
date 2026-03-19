# Tool: Replace exact strings in a text file

### Description

Finds and replaces exact string occurrences in a file. Because this tool
operates on exact string matches, it's suited for simple renames, value
updates, or repetitive text changes where the target string is
unambiguous.

### Replace vs Edit

btw's two file-editing tools serve different use cases:

- `btw_tool_files_replace()` is best for exact text substitutions:
  renaming a variable, updating a URL, or changing a value. It does not
  require a prior
  [`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
  call. By default, it requires the match to be unique to prevent
  unintended changes.

- [`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md)
  is best for structural, line-based edits: inserting new lines,
  deleting lines, replacing a range of lines, or making several edits at
  once. It requires hashline references from a prior
  [`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
  call and validates them against the current file state.

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

  The exact string to find in the file. Must match
  character-for-character, including whitespace and indentation. Must be
  unique unless `replace_all` is `TRUE`.

- new_string:

  The replacement string. Must differ from `old_string`. Use an empty
  string (`""`) to delete the matched text.

- replace_all:

  If `TRUE`, replace all occurrences of `old_string`. Defaults to
  `FALSE`, which requires exactly one occurrence.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a message confirming the replacement was applied, including the
number of occurrences replaced.

## See also

[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md)
for line-based structural edits using hashline references,
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
for reading files.

Other files tools:
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md),
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)
