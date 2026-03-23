# Tool: Read a file

### Description

Reads the contents of a text file, optionally restricted to a line
range. Each line is annotated with a hashline prefix
(`line_number:hash|content`) that enables validated editing via
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/reference/btw_tool_files_edit.md).

### Hashline annotations

The hashline format prefixes each line with `line_number:hash|`, e.g.
`2:f1a| return("world")`. The 3-character hash is a truncated
[`rlang::hash()`](https://rlang.r-lib.org/reference/hash.html) of the
line content after trimming whitespace and truncating to 80 characters.
These hashes are used by
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/reference/btw_tool_files_edit.md)
to validate that the file hasn't changed between reading and editing.
See
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/reference/btw_tool_files_edit.md)
for details on the hashline approach and its benefits and limitations.

Hashline annotations are only included in the model-facing tool output.
The display shown to users in
[`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
or
[`shinychat::chat_ui()`](https://posit-dev.github.io/shinychat/r/reference/chat_ui.html)
is always a clean code block.

## Usage

``` r
btw_tool_files_read(path, line_start = 1, line_end = 1000, `_intent` = "")
```

## Arguments

- path:

  Path to a file for which to get information. The `path` must be in the
  current working directory.

- line_start:

  Starting line to read, defaults to 1 (starting from the first line).

- line_end:

  Ending line to read, defaults to 1000. Change only this value if you
  want to read more or fewer lines. Use in combination with `line_start`
  to read a specific line range of the file.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns lines with hashline annotations (see **Hashline annotations**).

## See also

[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/reference/btw_tool_files_edit.md)
for making validated edits using hashline references,
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/reference/btw_tool_files_replace.md)
for exact string find-and-replace,
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/reference/btw_tool_files_write.md)
for writing entire files.

Other files tools:
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/reference/btw_tool_files_edit.md),
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/reference/btw_tool_files_list.md),
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/reference/btw_tool_files_replace.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/reference/btw_tool_files_write.md)

## Examples

``` r
withr::with_tempdir({
  write.csv(mtcars, "mtcars.csv")

  btw_tool_files_read("mtcars.csv", line_end = 5)
})
#> <btw::BtwTextFileToolResult>
#>  @ value  : chr "1:8e0|\"\",\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"\n2:847|\"Ma"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ path   : 'fs_path' chr "mtcars.csv"
#>  .. $ display:List of 2
#>  ..  ..$ markdown: chr "```csv\n\"\",\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"\n\"Mazda "| __truncated__
#>  ..  ..$ title   : 'html' chr "Read <code>mtcars.csv</code>"
#>  ..  .. ..- attr(*, "html")= logi TRUE
#>  @ request: NULL
```
