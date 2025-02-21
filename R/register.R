#' @export
register_btw_tools <- function(chat) {
  check_inherits(chat, "Chat")

  tools <- list(
    tool_get_data_frame(),
    tool_get_installed_packages(),
    tool_get_package_help(),
    tool_get_help_page()
  )

  for (tool in tools) {
    chat$register_tool(tool)
  }

  chat
}
