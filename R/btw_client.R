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
#' You can also use the `.btw` file to choose default chat settings for your
#' project in a YAML block at the top of the file. In this YAML block you can
#' choose the default `provider`, `model` and `tools` for `btw_client()` or
#' `btw_app()`. `provider` chooses the `ellmer::chat_*()` function, e.g.
#' `provider: openai` or `provider: chat_openai` to use [ellmer::chat_openai()].
#' `tools` chooses which btw tools are included in the chat, and all other
#' values are passed to the `ellmer::chat_*()` constructor, e.g.
#' `model: gpt-4o`, `seed: 42`, or `echo: all``.
#'
#' Here's an example `.btw` file:
#'
#' ````
#' ---
#' provider: claude
#' model: claude-3-7-sonnet-20250219
#' tools: [data, docs, environment]
#' ---
#'
#' Follow these important style rules for any R code in this project:
#'
#' * Prefer solutions that use {tidyverse}
#' * Always use `<-` for assignment
#' * Always use the native base-R pipe `|>` for piped expressions
#'
#' ````
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
#'   `btw_client()` calls, or use a `.btw` project file for default chat client
#'   settings, like provider and model.
#' @param tools Names of tools or tool groups to include when registering
#'   tools, e.g. `include = "docs"` to include only the documentation related
#'   tools, or `include = c("data", "docs", "environment")`, etc. Equivalent to
#'   the `include` argument of [btw_register_tools()].
#' @param path_btw A path to a `.btw` project context file. If `NULL`, btw will
#'   find a project-specific `.btw` file in the parents of the current working
#'   directory.
#'
#' @return Returns an [ellmer::Chat] object with additional tools registered by
#'   [btw_register_tools()]. `btw_app()` returns the chat object invisibly, and
#'   the chat object with the messages added during the chat session.
#'
#' @describeIn btw_client Create a btw-enhanced [ellmer::Chat] client
#' @export
btw_client <- function(..., client = NULL, tools = NULL, path_btw = NULL) {
  config <- btw_client_config(client, tools, config = read_btw_file(path_btw))

  client <- config$client

  sys_prompt <- client$get_system_prompt()
  sys_prompt <- c(
    if (!identical(config$tools, "none")) {
      c(
        "# Tools",
        "",
        paste(
          "You have access to tools that help you interact with the user's R session and workspace.",
          "Use these tools when they are helpful and appropriate to complete the user's request.",
          "These tools are available to augment your ability to help the user,",
          "but you are smart and capable and can answer many things on your own.",
          "It is okay to answer the user without relying on these tools."
        ),
        ""
      )
    },
    if (!is.null(config$btw_context)) {
      c(
        "# Project Context",
        "",
        trimws(paste(config$btw_context, collapse = "\n")),
        ""
      )
    },
    "---\n",
    sys_prompt
  )
  client$set_system_prompt(paste(sys_prompt, collapse = "\n"))

  maybe_quiet <- if (is.null(tools)) suppressMessages else I
  maybe_quiet(btw_register_tools(client, include = config$tools))

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

# nocov start

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @export
btw_app <- function(..., client = NULL, tools = NULL, path_btw = NULL) {
  client <- btw_client(
    client = client,
    ...,
    tools = tools,
    path_btw = path_btw
  )
  ellmer::live_browser(client)
}

# nocov end

btw_client_config <- function(client = NULL, tools = NULL, config = list()) {
  config$tools <-
    tools %||%
    getOption("btw.chat_tools") %||%
    config$tools

  if (!is.null(client)) {
    check_inherits(client, "Chat")
    config$client <- client
    return(config)
  }

  default <- getOption("btw.chat_client")
  if (!is.null(default)) {
    check_inherits(default, "Chat")
    config$client <- default$clone()
    return(config)
  }

  if (!is.null(config$provider)) {
    chat_args <- config[setdiff(
      names(config),
      c("tools", "provider", "btw_context")
    )]
    chat_fn <- gsub(" ", "_", tolower(config$provider))
    if (!grepl("^chat_", chat_fn)) {
      chat_fn <- paste0("chat_", chat_fn)
    }
    chat_client <- call2(.ns = "ellmer", chat_fn, !!!chat_args)
    config$client <- eval(chat_client)
    return(config)
  }

  config$client <- ellmer::chat_claude()
  config
}

read_btw_file <- function(path = NULL) {
  must_find <- !is.null(path)

  path <- path %||% path_find_in_project(".btw")

  if (!must_find && is.null(path)) {
    return(list())
  }

  if (must_find && (is.null(path) || !fs::file_exists(path))) {
    cli::cli_abort("Invalid {.arg path}: {.path {path}} does not exist.")
  }

  config <- rmarkdown::yaml_front_matter(path)

  remove_yaml <- function(path) {
    pyfm <- asNamespace("rmarkdown")[["partition_yaml_front_matter"]]
    pyfm(readLines(path, warn = FALSE))$body
  }

  config$btw_context <- remove_yaml(path)
  config
}
