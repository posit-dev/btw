#' Create a btw-enhanced ellmer chat client
#'
#' @description
#' Creates an [ellmer::Chat] client, enhanced with the tools from
#' [btw_register_tools()]. Use `btw_chat()` to create the chat client for
#' general or interactive use at the console, or `btw_app()` to create a chat
#' client and launch a Shiny app for chatting with a btw-enhanced LLM in your
#' local workspace.
#'
#' ## Project Context
#'
#' You can keep track of project-specific rules, guidance and context by adding
#' a `.btw` file in your project directory. Any time you start a chat client
#' with `btw_client()` or launch a chat session with `btw_app()`, btw will
#' automatically find and include the contents of the `.btw` file in your chat.
#'
#' Use the `.btw` file to inform the LLM of your preferred code style, to
#' provide domain-specific terminology or definitions, to establish project
#' documentation, goals and constraints, to include reference materials such or
#' technical specifications, or more. Storing this kind of information in the
#' `.btw` file may help you avoid repeating yourself and can be used to maintain
#' coherence across many chat sessions.
#'
#' The `.btw` file, when present, is included as part of the system prompt for
#' your chat conversation. You can structure the file in any way you wish.
#'
#' ## Client Options
#'
#' * `btw.chat_client`: The [ellmer::Chat] client to use as the basis for new
#'   `btw_client()` or `btw_app()` chats.
#' * `btw.chat_include`: The btw tools to include by default when starting a new
#'   btw chat, see [btw_register_tools()] for details.
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
#' @param ... Objects and documentation to be included as context in the chat,
#'   via [btw()].
#' @param client An [ellmer::Chat] client, defaults to [ellmer::chat_claude()].
#'   You can use the `btw.chat_client` option to set a default client for new
#'   `btw_client()` calls.
#' @inheritParams btw_register_tools
#'
#' @return Returns an [ellmer::Chat] object with additional tools registered by
#'   [btw_register_tools()]. `btw_app()` returns the chat object invisibly, and
#'   the chat object with the messages added during the chat session.
#'
#' @describeIn btw_client Create a btw-enhanced [ellmer::Chat] client
#' @export
btw_client <- function(..., client = NULL, include = NULL) {
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

  include <- if (is.null(include)) getOption("btw.chat_include")

  btw_proj_context <- path_find_in_project(".btw")
  if (!fs::is_file(btw_proj_context)) btw_proj_context <- NULL

  sys_prompt <- client$get_system_prompt()
  sys_prompt <- c(
    sys_prompt,
    "",
    if (!is.null(btw_proj_context)) {
      c(
        "# Project Context",
        "",
        paste(readLines(btw_proj_context, warn = FALSE), collapse = "\n"),
        ""
      )
    },
    "# Tools",
    "",
    paste(
      "You have access to tools that help you interact with the user's R session and workspace.",
      "Use these tools when they are helpful and appropriate to complete the user's request.",
      "These tools are available to augment your ability to help the user,",
      "but you are smart and capable and can answer many things on your own.",
      "It is okay to answer the user without relying on these tools."
    )
  )
  client$set_system_prompt(paste(sys_prompt, collapse = "\n"))
  btw_register_tools(client, include = include)

  dots <- dots_list(..., .named = TRUE)

  if (length(dots)) {
    turns <- client$get_turns()
    turns <- c(
      turns,
      ellmer::Turn(
        "user",
        contents = list(btw(!!!dots))
      )
    )
    client$set_turns(turns)
  }

  client
}

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @export
btw_app <- function(..., client = NULL, include = NULL) {
  client <- btw_client(client = client, ..., include = include)
  ellmer::live_browser(client)
}
