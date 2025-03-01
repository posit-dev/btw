#' Tool: Read current file
#'
#' @description
#' Reads the current file using the \pkg{rstudioapi}, which works in RStudio,
#' Positron and VS Code (with the vscode-r extension).
#'
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
btw_tool_read_current_editor <- function(consent = FALSE) {
  if (!isTRUE(consent)) {
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

  return(c(
    paste0("> ./", fs::path_rel(cf$path)),
    cf$contents
  ))
}

.btw_add_to_tools(function() {
  if (!rstudioapi::hasFun("getSourceEditorContext")) return(NULL)
  ellmer::tool(
    btw_tool_read_current_editor,
    paste(
      "Read the contents of the editor that is currently open in the user's IDE.",
      "Only use this tool when specifically asked to do so by the user.",
      "'@current_file' is considered explicit consent."
    ),
    consent = ellmer::type_boolean(
      "Did the user specifically request you read from their current file or editor?",
      required = FALSE
    )
  )
})
