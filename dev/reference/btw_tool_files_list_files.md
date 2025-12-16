# Tool: List files

Tool: List files

## Usage

``` r
btw_tool_files_list_files(
  path = NULL,
  type = c("any", "file", "directory"),
  regexp = "",
  `_intent` = ""
)
```

## Arguments

- path:

  Path to a directory or file for which to get information. The `path`
  must be in the current working directory. If `path` is a directory, we
  use [`fs::dir_info()`](https://fs.r-lib.org/reference/dir_ls.html) to
  list information about files and directories in `path` (use `type` to
  pick only one or the other). If `path` is a file, we show information
  about that file.

- type:

  File type(s) to return, one of `"any"` or `"file"` or `"directory"`.

- regexp:

  A regular expression (e.g. `[.]csv$`) passed on to
  [`grep()`](https://rdrr.io/r/base/grep.html) to filter paths.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a character table of file information.

## See also

Other files tools:
[`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read_text_file.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md)

## Examples

``` r
withr::with_tempdir({
  write.csv(mtcars, "mtcars.csv")

  btw_tool_files_list_files(type = "file")
})
#> <btw::BtwToolResult>
#>  @ value  : chr "| path | type | size | modification_time |\n|------|------|------|-------------------|\n| mtcars.csv | file | 1"| __truncated__
#>  @ error  : NULL
#>  @ extra  :List of 2
#>  .. $ data   : tibble [1 × 4] (S3: tbl_df/tbl/data.frame)
#>  ..  ..$ path             : 'fs_path' chr "mtcars.csv"
#>  ..  ..$ type             : Factor w/ 8 levels "any","block_device",..: 7
#>  ..  ..$ size             : 'fs_bytes' num 1.74K
#>  ..  ..$ modification_time: POSIXct[1:1], format: "2025-12-16 03:10:39"
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "| path | type | size | modification_time |\n|------|------|------|-------------------|\n| mtcars.csv | file | 1"| __truncated__
#>  @ request: NULL
```
