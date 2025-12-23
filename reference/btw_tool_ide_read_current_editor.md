# Tool: Read current file

Reads the current file using the rstudioapi, which works in RStudio,
Positron and VS Code (with the vscode-r extension).

## Usage

``` r
btw_tool_ide_read_current_editor(
  selection = TRUE,
  consent = FALSE,
  `_intent` = ""
)
```

## Arguments

- selection:

  Should only the selected text be included? If no text is selected, the
  full file contents are returned.

- consent:

  Boolean indicating whether the user has consented to reading the
  current file. The tool definition includes language to induce LLMs to
  confirm with the user before calling the tool. Not all models will
  follow these instructions. Users can also include the string
  `@current_file` to induce the tool.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns the contents of the current editor.

## Examples

``` r
if (FALSE) { # rstudioapi::hasFun("getSourceEditorContext")
btw_tool_ide_read_current_editor(consent = TRUE)
}
```
