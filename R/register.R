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
    # .btw_tools is a list of functions that create tools (at runtime not build)
    tool <- tool()

    # and some may return `NULL` if disabled or contextually unavailable
    if (is.null(tool)) next

    chat$register_tool(tool)
  }

  invisible(chat)
}

.docs_list_tools <- function() {
  x <- vapply(.btw_tools, FUN.VALUE = character(1), function(tool) {
    tool <- tool()
    desc <- strsplit(tool@description, ". ", fixed = TRUE)[[1]][1]
    if (!grepl("[.]$", desc)) {
      desc <- paste0(desc, ".")
    }
    sprintf("- **%s**: %s", tool@name, desc)
  })
  paste(x, collapse = "\n")
}
