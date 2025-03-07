#' Tool: Read current file
#'
#' @description
#' Reads the current file using the \pkg{rstudioapi}, which works in RStudio,
#' Positron and VS Code (with the vscode-r extension).
#'
#' @param selection Should only the selected text be included? If no text is
#'   selected, the full file contents are returned.
#' @param consent Boolean indicating whether the user has consented to reading
#'   the current file. The tool definition includes language to induce LLMs to
#'   confirm with the user before calling the tool. Not all models will follow
#'   these instructions. Users can also include the string `@current_file` to
#'   induce the tool.
#'
#' @return Returns the contents of the current editor.
#'
#' @family Tools
#' @export
btw_tool_read_current_editor <- function(selection = TRUE, consent = FALSE) {
  check_bool(selection)
  check_bool(consent)

  if (!consent) {
    cli::cli_abort(
      "Please ask the user for consent before reading from the editor."
    )
  }

  if (!rstudioapi::hasFun("getSourceEditorContext")) {
    cli::cli_abort(
      "{.field @current_file} only works in an IDE where the {.pkg rstudioapi} is available."
    )
  }

  cf <- rstudioapi::getSourceEditorContext()
  path <- fs::path_rel(cf$path)

  has_no_selection <-
    length(cf$selection) == 1 && !nzchar(cf$selection[[1]]$text)

  if (!selection || has_no_selection) {
    res <- c(
      sprintf('FILE: `%s`', path),
      md_code_block(
        type = fs::path_ext(path),
        cf$contents
      )
    )
    return(I(res))
  }

  res <- c()
  for (selection in cf$selection) {
    if (!nzchar(selection$text)) next

    line_column <- function(range) {
      sprintf("L%dC%d", range[1], range[2])
    }

    line_range <- c(selection$range$start[1], selection$range$end[1])
    lines <- paste0("L", unique(line_range), collapse = "-")
    res <- c(
      res,
      if (length(res)) "",
      sprintf(
        'FILE: %s:%s-%s',
        path,
        line_column(selection$range$start),
        line_column(selection$range$end)
      ),
      md_code_block(
        type = fs::path_ext(path),
        strsplit(selection$text, "\n")[[1]]
      )
    )
  }

  I(res)
}

.btw_add_to_tools(
  name = "btw_tool_read_current_editor",
  group = "ide",
  tool = function() {
    if (!rstudioapi::hasFun("getSourceEditorContext")) return(NULL)
    ellmer::tool(
      btw_tool_read_current_editor,
      paste(
        "Read the contents of the editor that is currently open in the user's IDE.",
        "Only use this tool when specifically asked to do so by the user.",
        "'@current_file' and '@current_selection' are considered explicit consent."
      ),
      selection = ellmer::type_boolean(
        paste(
          "Include only the selected region(s) of the current file?",
          "Default is `true`; set to `false` to retrieve the entire file contents.",
          "Always use `true` when the user requests '@current_selection'."
        ),
        required = FALSE
      ),
      consent = ellmer::type_boolean(
        "Did the user specifically request you read from their current file or editor?",
        required = FALSE
      )
    )
  }
)
