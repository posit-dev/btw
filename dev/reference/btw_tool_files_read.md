# Tool: Read a file

Tool: Read a file

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

Returns a character vector of lines from the file.

## See also

Other files tools:
[`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md),
[`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md),
[`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md),
[`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md),
[`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md)

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
