#' Create a btw-enhanced ellmer chat client
#'
#' @description
#' Creates an [ellmer::Chat] client, enhanced with the tools from
#' [btw_tools()]. Use `btw_client()` to create the chat client for
#' general or interactive use at the console, or `btw_app()` to create a chat
#' client and launch a Shiny app for chatting with a btw-enhanced LLM in your
#' local workspace.
#'
#' ## Project Context
#'
#' You can keep track of project-specific rules, guidance and context by adding
#' a `btw.md` file in your project directory. Any time you start a chat client
#' with `btw_client()` or launch a chat session with `btw_app()`, btw will
#' automatically find and include the contents of the `btw.md` file in your
#' chat.
#'
#' Use `btw.md` to inform the LLM of your preferred code style, to provide
#' domain-specific terminology or definitions, to establish project
#' documentation, goals and constraints, to include reference materials such or
#' technical specifications, or more. Storing this kind of information in
#' `btw.md` may help you avoid repeating yourself and can be used to maintain
#' coherence across many chat sessions.
#'
#' The `btw.md` file, when present, is included as part of the system prompt for
#' your chat conversation. You can structure the file in any way you wish.
#'
#' You can also use the `btw.md` file to choose default chat settings for your
#' project in a YAML block at the top of the file. In this YAML block you can
#' choose settings for the default ellmer chat `client`, e.g. `provider`,
#' `model`, as well as choose with \pkg{btw} `tools` to use in `btw_client()` or
#' `btw_app()`. `provider` chooses the `ellmer::chat_*()` function, e.g.
#' `provider: openai` or `provider: chat_openai` to use [ellmer::chat_openai()].
#' `tools` chooses which btw tools are included in the chat, and all other
#' values are passed to the `ellmer::chat_*()` constructor, e.g. `model:
#' gpt-4o`, `seed: 42`, or `echo: all``.
#'
#' Here's an example `btw.md` file:
#'
#' ````
#' ---
#' client:
#'   provider: claude
#'   model: claude-3-7-sonnet-20250219
#' tools: [data, docs, environment]
#' ---
#'
#' Follow these important style rules for any R code in this project:
#'
#' * Prefer solutions that use {tidyverse}
#' * Always use `<-` for assignment
#' * Always use the native base-R pipe `|>` for piped expressions
#' ````
#'
#' You can hide parts of the `btw.md` file from the system prompt by wrapping
#' them in HTML `<!-- HIDE -->` and `<!-- /HIDE -->` comment tags. A single
#' `<!-- HIDE -->` comment tag will hide all content after it until the next
#' `<!-- /HIDE -->` tag, or the end of the file. This is particularly useful
#' when your system prompt contains notes to yourself or future tasks that you
#' do not want to be included in the system prompt.
#'
#' For project-specific configuration, store your `btw.md` file in the root of
#' your project directory. For global configuration, you can maintain a `btw.md`
#' file in your home directory (at `btw.md` or `.config/btw/btw.md` in your home
#' directory, using `fs::path_home()`). This file will be used by default when a
#' project-specific `btw.md` file is not found.
#'
#' ## Client Options
#'
#' * `btw.client`: The [ellmer::Chat] client to use as the basis for new
#'   `btw_client()` or `btw_app()` chats.
#' * `btw.tools`: The btw tools to include by default when starting a new
#'   btw chat, see [btw_tools()] for details.
#'
#' @examplesIf rlang::is_interactive()
#' withr::local_options(list(
#'   btw.client = ellmer::chat_ollama(model="llama3.1:8b")
#' ))
#'
#' chat <- btw_client()
#' chat$chat("How can I replace `stop()` calls with functions from the cli package?")
#'
#' @param client An [ellmer::Chat] client, defaults to
#'   [ellmer::chat_anthropic()]. You can use the `btw.client` option to set a
#'   default client for new `btw_client()` calls, or use a `btw.md` project file
#'   for default chat client settings, like provider and model. We check the
#'   `client` argument, then the `btw.client` R option, and finally the `btw.md`
#'   project file, using only the client definition from the first of these that
#'   is available.
#' @param tools A list of tools to include in the chat, defaults to
#'   [btw_tools()]. Join [btw_tools()] with additional tools defined by
#'   [ellmer::tool()] to include additional tools in the chat client.
#'   Alternatively, you can use a character values to refer to specific btw
#'   tools by name or by group. For example, use `tools = "docs"` to include
#'   only the documentation related tools, or `tools = c("env", "docs")` to
#'   include the environment and documentation tools, and so on. You can also
#'   refer to btw tools by name, e.g. `tools = "btw_tool_docs_help_page"` or
#'   alternatively in the shorter form `tools = "docs_help_page"`. Finally,
#'   set `tools = FALSE` to skip registering \pkg{btw} tools with the chat
#'   client.
#' @param path_btw A path to a `btw.md` project context file. If `NULL`, btw
#'   will find a project-specific `btw.md` file in the parents of the current
#'   working directory.
#' @param ... Additional arguments are ignored. `...` are included for future
#'   feature expansion.
#'
#' @return Returns an [ellmer::Chat] object with additional tools registered
#'   from [btw_tools()]. `btw_app()` returns the chat object invisibly, and
#'   the chat object with the messages added during the chat session.
#'
#' @describeIn btw_client Create a btw-enhanced [ellmer::Chat] client
#' @export
btw_client <- function(
  ...,
  client = NULL,
  tools = NULL,
  path_btw = NULL
) {
  check_dots_empty()

  config <- btw_client_config(client, tools, config = read_btw_file(path_btw))
  skip_tools <- isFALSE(config$tools) || identical(config$tools, "none")

  client <- config$client

  withr::local_options(
    btw.__agent_client__ = client,
    btw.__agent_turns__ = client
  )

  sys_prompt <- client$get_system_prompt()
  sys_prompt <- c(
    "# System and Session Context",
    "Please account for the following R session and system settings in all responses.",
    "",
    btw_tool_session_platform_info()@value,
    "",
    if (!skip_tools) {
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
    if (!is.null(config$btw_system_prompt)) {
      c(
        "# Project Context",
        "",
        trimws(paste(config$btw_system_prompt, collapse = "\n")),
        ""
      )
    },
    "---\n",
    sys_prompt
  )
  client$set_system_prompt(paste(sys_prompt, collapse = "\n"))

  if (!skip_tools) {
    client$set_tools(tools = c(client$get_tools(), config$tools))
  }

  client
}

btw_client_config <- function(client = NULL, tools = NULL, config = list()) {
  config$tools <-
    tools %||%
    getOption("btw.tools") %||%
    config$tools %||%
    btw_tools()

  config$tools <- flatten_and_check_tools(config$tools)

  # 1. Client was provided explicitly
  if (!is.null(client)) {
    check_inherits(client, "Chat")
  } else {
    # 2. Client wasn't provided, check the `btw.client` R option
    default <- getOption("btw.client")
    if (!is.null(default)) {
      check_inherits(default, "Chat")
      config$client <- default$clone()
    }
  }

  # 3a. Check for usage of deprecated btw.md fields
  if (!is.null(config$provider)) {
    lifecycle::deprecate_stop(
      when = "0.0.3",
      what = I("`provider`"),
      details = "Use the `client` field instead, e.g. `client: {provider: 'openai'}`."
    )
  }

  if (!is.null(config$model)) {
    lifecycle::deprecate_stop(
      when = "0.0.3",
      what = I("`model`"),
      details = "Use the `client` field instead, e.g. `client: {model: 'gpt-4.1-mini`}`."
    )
  }

  # 3b. Use the `btw.md` file to configure the client
  if (is.null(client) && !is.null(config$client)) {
    client <- btw_config_client(config$client)
  }

  # 4. Default to Claude from Anthropic
  if (is.null(client)) {
    client <- ellmer::chat_anthropic(echo = "output")
  }

  withr::local_options(
    btw.__agent_client__ = client,
    btw.__agent_turns__ = client
  )

  # ---- Agents ----
  if (!is.null(config$agents)) {
    for (config_agent in config$agents) {
      # The agent inherits from the base btw client
      config_agent$client <- utils::modifyList(
        config$client,
        config_agent$client %||% list()
      )

      config_agent$client <- btw_config_client(
        config_agent$client,
        quiet = TRUE
      )

      agent_tool <- do.call(btw_tool_agent, config_agent)
      client$register_tool(agent_tool)
    }
  }

  config$client <- client
  config
}

btw_config_client <- function(config_client, quiet = FALSE) {
  chat_args <- utils::modifyList(
    list(echo = "output"), # defaults
    config_client
  )

  chat_fn <- gsub(" ", "_", tolower(chat_args$provider))
  if (!grepl("^chat_", chat_fn)) {
    chat_fn <- paste0("chat_", chat_fn)
  }
  chat_args$provider <- NULL

  chat_client <- call2(.ns = "ellmer", chat_fn, !!!chat_args)
  client <- eval(chat_client)

  if (isFALSE(quiet) && !is.null(chat_args$model)) {
    cli::cli_inform(
      "Using {.field {chat_args$model}} from {.strong {client$get_provider()@name}}."
    )
  }

  client
}

flatten_and_check_tools <- function(tools) {
  if (isFALSE(tools)) {
    return(list())
  }

  if (inherits(tools, "ellmer::ToolDef")) {
    cli::cli_abort(
      "{.arg tools} should be a list of {.help ellmer::tool} tools or character names for {.pkg btw} tools."
    )
  }

  if (is.character(tools)) {
    tools <- btw_tools(tools)
    return(tools)
  }

  if (!is.list(tools)) {
    cli::cli_abort(c(
      "Invalid {.arg tools}: Must be a character vector of {.pkg btw} tool names, or list of Tool objects and {.pkg btw} tool names.",
      i = "See {.help btw::btw_tools} for more information about available tools and names."
    ))
  }

  flat_tools <- list()
  for (i in seq_along(tools)) {
    tool <- tools[[i]]
    if (inherits(tool, "ellmer::ToolDef")) {
      flat_tools <- c(flat_tools, list(tool))
    } else if (is.character(tool)) {
      flat_tools <- c(flat_tools, btw_tools(tool))
    } else {
      cli::cli_abort(
        "Invalid tool in {.arg tools[[{i}]]}: Must be a character vector or Tool object, not {.obj_type_friendly {tools[[i]]}}."
      )
    }
  }

  flat_tools
}

read_btw_file <- function(path = NULL) {
  if (isFALSE(path)) {
    return(list())
  }

  must_find <- !is.null(path)

  path <- path %||% path_find_in_project("btw.md") %||% path_find_user("btw.md")

  if (!must_find && is.null(path)) {
    return(list())
  }

  if (must_find && (is.null(path) || !fs::file_exists(path))) {
    cli::cli_abort("Invalid {.arg path}: {.path {path}} does not exist.")
  }

  config <- rmarkdown::yaml_front_matter(path)

  read_without_yaml <- function(path) {
    pyfm <- asNamespace("rmarkdown")[["partition_yaml_front_matter"]]
    pyfm(readLines(path, warn = FALSE))$body
  }

  btw_system_prompt <- read_without_yaml(path)
  config$btw_system_prompt <- remove_hidden_content(btw_system_prompt)
  config
}

remove_hidden_content <- function(lines) {
  if (length(lines) == 0) {
    return(character(0))
  }

  starts <- cumsum(trimws(lines) == "<!-- HIDE -->")
  ends <- trimws(lines) == "<!-- /HIDE -->"

  # Shift ends to avoid including /HIDE
  shift <- function(x) c(0, x[-length(x)])

  ends[starts - cumsum(ends) < 0 & ends] <- FALSE

  lines[starts - shift(cumsum(ends)) <= 0]
}
