#' Create a btw-enhanced ellmer chat client
#'
#' Creates an [ellmer::Chat] client, enhanced with the tools from
#' [btw_register_tools()]. Use `btw_chat()` to create the chat client for
#' general or interactive use at the console, or `btw_app()` to create a chat
#' client and launch a Shiny app for chatting with a btw-enhanced LLM in your
#' local workspace.
#'
#' @examples
#' if (interactive()) {
#'   withr::local_options(list(
#'     btw.chat_client = ellmer::chat_ollama(model="llama3.1:8b")
#'   ))
#'
#'   btw_chat <- btw_client()
#'   chat$chat("How can I replace `stop()` calls with functions from the cli package?")
#' }
#'
#' @param ... Additional arguments passed to [btw_register_tools()] to control
#'   which tools are added to the chat client.
#' @param client An [ellmer::Chat] client, defaults to [ellmer::chat_claude()].
#'   You can use the `btw.chat_client` option to set a default client for new
#'   `btw_client()` calls.
#'
#' @return Returns an [ellmer::Chat] object with additional tools registered by
#'   [btw_register_tools()]. `btw_app()` returns the chat object invisibly, and
#'   the chat object with the messages added during the chat session.
#'
#' @describeIn btw_client Create a btw-enhanced [ellmer::Chat] client
#' @export
btw_client <- function(..., client = NULL) {
  if (is.null(client)) {
    default <- getOption("btw.chat_client")
    if (is.null(default)) {
      client <- ellmer::chat_claude()
    } else {
      check_inherits(default, "Chat")
      client <- default$clone()
    }
  } else {
    check_inherits(client, "Chat")
  }

  sys_prompt <- client$get_system_prompt()
  sys_prompt <- c(
    sys_prompt,
    paste(
      "You have access to tools that help you interact with the user's R session and workspace.",
      "Use these tools when they are helpful and appropriate to complete the user's request.",
      "These tools are available to augment your ability to help the user,",
      "but you are smart and capable and can answer many things on your own.",
      "It is okay to answer the user without relying on these tools."
    )
  )
  client$set_system_prompt(sys_prompt)
  btw_register_tools(client, ...)

  client
}

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @export
btw_app <- function(..., client = NULL) {
  client <- btw_client(client = client, ...)
  ellmer::live_browser(client)
}
