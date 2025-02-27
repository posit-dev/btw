#' Tools: Register tools from btw
#'
#' @description
#' The `btw_register_tools()` function equips an ellmer chat to interface with
#' your computational environment. Chats returned by this function have access
#' to the tools:
#'
#' `r .docs_list_tools()`
#'
#' @param chat An ellmer `Chat` object.
#'
#' @returns Registers the tools with `chat`, updating the `chat` object in
#'   place. The `chat` input is returned invisibly.
#'
#' @examples
#' # requires an ANTHROPIC_API_KEY
#' \dontrun{
#' ch <- ellmer::chat_claude()
#'
#' btw_register_tools(ch)
#' }
#'
#' @family Tools
#' @export
btw_register_tools <- function(chat) {
  check_inherits(chat, "Chat")

  for (tool in .btw_tools) {
    chat$register_tool(tool())
  }

  invisible(chat)
}

.docs_list_tools <- function() {
  x <- vapply(.btw_tools, FUN.VALUE = character(1), function(tool) {
    sprintf("- **%s**: %s", tool()@name, tool()@description)
  })
  paste(x, collapse = "\n")
}
