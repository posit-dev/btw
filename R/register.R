#' Register tools from btw
#'
#' @description
#' The `register_btw_tools()` function equips an ellmer chat to interface with
#' your computational environment. Chats returned by this function have access
#' to the tools `r paste0('[', purrr::map_chr(.btw_tools, purrr::pluck, "name"), '()]')`.
#'
#' @param chat An ellmer `Chat` object.
#'
#' @returns
#' The chat object with tools registered.
#'
#' @examples
#' # requires an ANTHROPIC_API_KEY
#' \dontrun{
#' ch <- ellmer::chat_claude()
#'
#' register_btw_tools(ch)
#' }
#' @export
register_btw_tools <- function(chat) {
  check_inherits(chat, "Chat")

  for (tool in .btw_tools) {
    chat$register_tool(tool())
  }

  chat
}
