# Tool: Edit a text file

### Description

This tool makes targeted, validated edits to a file using hashline
references from
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md).
This approach is based on the technique described in ["The Harness
Problem" by Can
Duruk](https://blog.can.ac/2026/02/12/the-harness-problem/), with
additional customizations to improve the performance of the technique,
in agentic coding session.

The core idea is that each line is annotated with a short content hash
("hashline") serving as a stable reference. When submitting an edit, the
model includes hashline references for the target lines. The tool
validates these hashes against the current file before applying changes,
ensuring edits are applied to the intended lines. If a hash mismatch
occurs, the edit is rejected and the model must re-read the file for
fresh references.

### How hashlines work

[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
annotates each line of the file with a short content hash in the format
`line_number:hash|content`, e.g. `2:d6a|library(btw)`. The 3-character
hash is derived from
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) over the
line content (trimmed of leading/trailing whitespace and truncated to 80
characters).

When the model submits an edit, it references specific lines by their
hashline, e.g. `"2:d6a"`, which ensures that the edit is applied
precisely to the intended line. If the file has changed since the last
read, the hashline references will not match the current file state, and
the edit will be rejected with a hash mismatch error, prompting the
model to re-read the file.

### Edit actions

`btw_tool_files_edit()` supports three types of edit actions, each using
hashline references for validation:

- `"replace"`: Replace a single line. The model provides a `line` with a
  single hashline reference, e.g. `"2:d6a"`, and `content` to replace
  the line. The model can also remove the line by providing empty
  `content`.

- `"insert_after"`: Insert new lines after a reference line. The model
  provides a `line` with a hashline reference and `content` with the
  lines to insert after it. The model can also insert at the start of
  the file using `line = "0:000"`.

- `"replace_range"`: Replace a range of consecutive lines. The model
  provides a `line` with two hashline references indicating the start
  and end of the range, e.g. `"10:a3f,15:b2c"`, and `content` with the
  replacement lines.

Multiple edits in a single call are allowed and are applied together:
all succeed or all fail. Edits must not have overlapping line ranges.

### Response format

After a successful edit, the response includes updated hashline
references for the edited regions (plus 1 line of surrounding context).
When edits change the total line count, the response includes shift
hints that tell the model how to adjust any cached line numbers without
re-reading the entire file (e.g. "update line numbers by +2").

### Benefits and limitations

The hashline approach provides strong validation for targeted edits, and
for a single round of edits, it's generally more token efficient than
the find-and-replace approach of
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md)
(which is also the approach used by many file-editing tools, e.g. `Edit`
in Claude Code). With the hashline approach, the model can make very
specific edits without needing to include large amounts of unchanged
context around the edit, which is often required for find-and-replace to
avoid unintended matches.

However, when repeatedly editing the same file, the hashline approach
can cause the model to repeatedly re-read the file to get fresh hashline
references. I've attempted to mitigate this by including updated
references and shift hints (e.g. "update line numbers by +2") in the
edit response, which allows the model to adjust the hashline references
of previously read lines without re-reading the entire file. Models can
also choose to re-read only specific sections of the file using the
`line_start` and `line_end` parameters of
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
to minimize token usage while refreshing references for the relevant
lines.

However, not all models will make use of these features, and in
practice, over longer editing sessions, the hashline approach may lead
to more token usage due to the model repeatedly re-reading the file to
get fresh references.

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
  `line`, and `content` fields. See **Edit actions** for details.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a message confirming the edits were applied, including updated
hashline references for the edited regions and shift hints when line
numbers have changed.

## See also

[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md)
for reading files with hashline annotations,
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md)
for find-and-replace edits that don't require hashline references.

Other files tools:
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md),
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)
