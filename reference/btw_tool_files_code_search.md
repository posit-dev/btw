# Tool: Code Search in Project

Search through code files in the project directory for specific terms.

## Usage

``` r
btw_tool_files_code_search(
  term,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE,
  show_lines = FALSE,
  `_intent` = ""
)
```

## Arguments

- term:

  The term to search for in the code files.

- limit:

  Maximum number of matching lines to return (between 1 and 1000,
  default 100).

- case_sensitive:

  Whether the search should be case-sensitive (default is `FALSE`).

- use_regex:

  Whether to interpret the search term as a regular expression (default
  is `FALSE`).

- show_lines:

  Whether to show the matching lines in the results. Defaults to
  `FALSE`, which means only the file names and count of matching lines
  are returned.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a tool result with a data frame of search results, with columns
for `filename`, `size`, `last_modified`, `content` and `line`.

## Details

### Options

You can configure which file extensions are included and which paths are
excluded from code search by using two options:

- `btw.files_code_search.extensions`: A character vector of file
  extensions to search in (default includes R, Python, JavaScript,
  TypeScript, Markdown, SCSS, and CSS files).

- `btw.files_code_search.exclusions`: A character vector of
  gitignore-style patterns to exclude paths and directories from the
  search. The default value includes a set of common version control,
  IDE, and cache folders.

Alternatively, you can also set these options in your `btw.md` file
under the `options` section, like this:

    ---
    client:
      provider: anthropic
    tools: [files_code_search]
    options:
      files_code_search:
        extensions: ["R", "Rmd", "py", "qmd"]
        exclusions: ["DEFAULT", ".quarto/"]
    ---

Include `"DEFAULT"` in the `exclusions` option to use btw's default
exclusions, which cover common directories like `.git/`, `.vscode/`.

If the gert package is installed and the project is a Git repository,
the tool will also respect the `.gitignore` file and exclude any ignored
paths, regardless of the `btw.files_code_search.exclusions` option.

## See also

Other files tools:
[`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/reference/btw_tool_files_list_files.md),
[`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read_text_file.md),
[`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/reference/btw_tool_files_write_text_file.md)

## Examples

``` r
withr::with_tempdir({
  writeLines(state.name[1:25], "state_names_1.md")
  writeLines(state.name[26:50], "state_names_2.md")

  tools <- btw_tools("files_code_search")
  tools$btw_tool_files_code_search(
    term = "kentucky",
    case_sensitive = FALSE,
    show_lines = TRUE
  )
})
#> ℹ Indexing files in /tmp/Rtmp0PzASk/file22624f9bc8d2 for code search
#> ✔ Indexing files in /tmp/Rtmp0PzASk/file22624f9bc8d2 for code search [311ms]
#> 
#> <btw::BtwToolResult>
#>  @ value  :'data.frame': 1 obs. of  5 variables:
#>  .. $ filename     : chr "state_names_1.md"
#>  .. $ size         : 'fs_bytes' num 219
#>  .. $ last_modified: POSIXct, format: "2025-12-23 13:04:18"
#>  .. $ content      : chr "Kentucky"
#>  .. $ line         : num 17
#>  @ error  : NULL
#>  @ extra  :List of 1
#>  .. $ display:List of 1
#>  ..  ..$ markdown: chr "| filename | size | last_modified | content | line |\n|----------|------|---------------|---------|------|\n| s"| __truncated__
#>  @ request: NULL
```
