# Tool: Apply a patch to files

Applies a structured diff-like patch envelope to one or more files.
Unlike
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md)
(which requires hashline references from a prior read) or
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md)
(which requires exact strings), the patch tool uses context-matching
hunks so models can produce coordinated edits across multiple files in a
single tool call. A single patch envelope can add, update, delete, and
move files atomically: either all operations succeed or none are
applied.

### Patch syntax

A patch is a text envelope that begins with `*** Begin Patch` and ends
with `*** End Patch`. Inside the envelope, each operation starts with a
header:

    ** Begin Patch
    ** Add File: docs/example.md
    Hello
    World
    ** Update File: src/main.py
    @@
    context line
    old line
    new line
    ** Delete File: old.txt
    ** Update File: src/old.ts
    ** Move to: src/new.ts
    @@
    context line
    export const oldName = 1
    export const newName = 1
    ** End Patch

#### Headers

- `*** Add File: <path>` – create a new file (must not exist).

- `*** Update File: <path>` – modify an existing file.

- `*** Delete File: <path>` – remove an existing file.

- `*** Move to: <path>` – sub-header inside an `Update File` block;
  renames the file to `<path>` after applying any hunks. Destination
  must not exist.

#### Hunk lines (inside `Update File`)

- `@` – hunk boundary; any trailing text on this line is informational
  and ignored.

- ` <text>` (space prefix) – context line that must match the file
  exactly.

- `-<text>` – line to delete; must match the file exactly at this
  position.

- `+<text>` – line to insert.

Every hunk must include at least one context or delete line to anchor
the edit; pure-insert hunks are rejected. Use `*** Add File` for new
files.

#### Add File body

All body lines under `*** Add File` must start with `+`; the file
content is the text after each `+`.

## Usage

``` r
btw_tool_files_patch(patch, `_intent` = "")
```

## Arguments

- patch:

  The full patch text in the wire format described below. Must begin
  with `*** Begin Patch` and end with `*** End Patch`.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a summary of the operations applied.

## See also

[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md)
for hashline-based targeted edits,
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md)
for exact find-and-replace edits.

Other files tools:
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md),
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md),
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)
