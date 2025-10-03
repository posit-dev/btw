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
#' a `btw.md` file or [`AGENTS.md`](https://agents.md/) in your project
#' directory. Either file name will work, so we'll refer primarily to `btw.md`.
#' \pkg{btw} will look first for `btw.md` and then for `AGENTS.md`. If both
#' files are present, only the `btw.md` file will be used.
#'
#' Any time you start a chat client with `btw_client()` or launch a chat session
#' with `btw_app()`, btw will automatically find and include the contents of the
#' `btw.md` or `AGENTS.md` file in your chat.
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
#' your project directory. You can even have multiple `btw.md` files in your
#' project, in which case the one closest to your current working directory
#' will be used. This makes it easy to have different `btw.md` files for
#' different sub-projects or sub-directories within a larger project.
#'
#' For global configuration, you can maintain a `btw.md` file in your home
#' directory (at `btw.md` or `.config/btw/btw.md` in your home directory, using
#' `fs::path_home()`). This file will be used by default when a project-specific
#' `btw.md` file is not found. Note that \pkg{btw} only looks for `btw.md` in
#' your home directory if no project-specific `btw.md` or `AGENTS.md` file is
#' present. It also does not look for `AGENTS.md` in your home directory.
#'
#' ## Client Options
#'
#' * `btw.client`: The [ellmer::Chat] client or a `provider/model` string (see
#'    [ellmer::chat()]) to use as the basis for new `btw_client()` or
#'    `btw_app()` chats.
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
#' @param client An [ellmer::Chat] client or a `provider/model` string to be
#'   passed to [ellmer::chat()] to create a chat client. Defaults to
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
#' @param path_btw A path to a `btw.md` or `AGENTS.md` project context file. If
#'   `NULL`, btw will find a project-specific `btw.md` or `AGENTS.md` file in
#'   the parents of the current working directory. Set `path_btw = FALSE` to
#'   create a chat client without using a `btw.md` file.
#' @param path_llms_txt A path to an `llms.txt` file containing context about
#'   the current project. By default, btw will look for an `llms.txt` file in
#'   the your current working directory or its parents. Set `path_llms_txt =
#'   FALSE` to skip looking for an `llms.txt` file.
#' @param ... In `btw_app()`, additional arguments are passed to
#'   [shiny::shinyApp()]. In `btw_client()`, additional arguments are ignored.
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
  path_btw = NULL,
  path_llms_txt = NULL
) {
  check_dots_empty()

  config <- btw_client_config(client, tools, config = read_btw_file(path_btw))
  client <- config$client
  skip_tools <- isFALSE(config$tools) || identical(config$tools, "none")
  withr::local_options(config$options)

  session_info <- btw_tool_session_platform_info()@value
  client_system_prompt <- client$get_system_prompt()

  llms_txt <- read_llms_txt(path_llms_txt)
  project_context <- c(llms_txt, config$btw_system_prompt)
  project_context <- paste(project_context, collapse = "\n\n")

  sys_prompt <- c(
    btw_prompt("btw-system_session.md"),
    if (!skip_tools) {
      btw_prompt("btw-system_tools.md")
    },
    if (nzchar(project_context)) {
      btw_prompt("btw-system_project.md")
    },
    if (!is.null(client_system_prompt)) "---",
    paste(client_system_prompt, collapse = "\n")
  )

  client$set_system_prompt(paste(sys_prompt, collapse = "\n\n"))

  if (!skip_tools) {
    client$set_tools(tools = c(client$get_tools(), config$tools))
  }

  client
}

btw_client_config <- function(client = NULL, tools = NULL, config = list()) {
  config$options <- flatten_config_options(config$options)
  withr::local_options(config$options)

  config$tools <-
    tools %||%
    getOption("btw.tools") %||%
    config$tools %||%
    btw_tools()

  config$tools <- flatten_and_check_tools(config$tools)

  if (!is.null(client)) {
    config$client <- as_ellmer_client(client)
    return(config)
  }

  default <- getOption("btw.client")
  if (!is.null(default)) {
    config$client <- as_ellmer_client(default)$clone()
    return(config)
  }

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

  if (!is.null(config$client)) {
    if (is_string(config$client)) {
      config$client <- as_ellmer_client(config$client)
      return(config)
    }

    chat_args <- utils::modifyList(
      list(echo = "output"), # defaults
      config$client
    )

    chat_fn <- gsub(" ", "_", tolower(chat_args$provider))
    if (!grepl("^chat_", chat_fn)) {
      chat_fn <- paste0("chat_", chat_fn)
    }
    chat_args$provider <- NULL

    chat_client <- call2(.ns = "ellmer", chat_fn, !!!chat_args)
    config$client <- eval(chat_client)

    if (!is.null(chat_args$model)) {
      cli::cli_inform(
        "Using {.field {chat_args$model}} from {.strong {config$client$get_provider()@name}}."
      )
    }
    return(config)
  }

  config$client <- ellmer::chat_anthropic(echo = "output")
  config
}

as_ellmer_client <- function(client) {
  if (inherits(client, "Chat")) {
    return(client)
  }

  if (!is_string(client)) {
    cli::cli_abort(c(
      "{.arg client} must be an {.help ellmer::Chat} client or a string naming a chat provider and model to pass to {.fn ellmer::chat}, not {.obj_type_friendly {client}}.",
      "i" = "Examples: {.or {.val {c('openai/gpt-5-mini', 'anthropic/claude-3-7-sonnet-20250219')}}}."
    ))
  }

  ellmer::chat(client, echo = "output")
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

flatten_config_options <- function(opts, prefix = "btw", sep = ".") {
  out <- list()

  recurse <- function(x, key_prefix) {
    # If x is a list, dive deeper
    if (is.list(x) && !is.data.frame(x)) {
      nm <- names2(x)
      if (!all(nzchar(nm))) {
        cli::cli_abort("All options must be named.")
      }

      for (i in seq_along(x)) {
        new_key <- paste(key_prefix, nm[i], sep = sep)
        recurse(x[[i]], new_key)
      }
    } else {
      # Leaf: assign it directly
      out[[key_prefix]] <<- x
    }
  }

  recurse(opts, prefix)
  out
}

maybe_find_in_project <- function(
  path,
  file_name,
  arg = "path",
  search_user_home = FALSE
) {
  if (isFALSE(path)) {
    return(NULL)
  }

  must_find <- !is.null(path)

  if (isTRUE(path)) {
    path <- NULL
  }

  path <- path %||% path_find_in_project(file_name)

  if (search_user_home) {
    path <- path %||% path_find_in_home(file_name)
  }

  if (!must_find && is.null(path)) {
    return(NULL)
  }

  if (must_find && (is.null(path) || !fs::file_exists(path))) {
    cli::cli_abort("Invalid {.arg {arg}}: {.path {path}} does not exist.")
  }

  path
}

read_btw_file <- function(path = NULL) {
  if (isFALSE(path)) {
    return(list())
  }

  path <- maybe_find_in_project(
    path,
    "btw.md",
    "path_btw",
    search_user_home = TRUE
  )

  if (is.null(path)) {
    path <- maybe_find_in_project(NULL, "AGENTS.md", "path_btw")
  }

  if (is.null(path)) {
    return(list())
  }

  config <- rmarkdown::yaml_front_matter(path)

  read_without_yaml <- function(path) {
    pyfm <- asNamespace("rmarkdown")[["partition_yaml_front_matter"]]
    pyfm(readLines(path, warn = FALSE))$body
  }

  btw_system_prompt <- read_without_yaml(path)
  btw_system_prompt <- remove_hidden_content(btw_system_prompt)
  btw_system_prompt <- paste(btw_system_prompt, collapse = "\n")
  btw_system_prompt <- trimws(btw_system_prompt)
  if (nzchar(btw_system_prompt)) {
    config$btw_system_prompt <- btw_system_prompt
  }

  config
}

read_llms_txt <- function(path = NULL) {
  path <- maybe_find_in_project(path, "llms.txt", "path_llms_txt")

  if (is.null(path)) {
    return(NULL)
  }

  llms_txt <- readLines(path, warn = FALSE)
  llms_txt <- paste(llms_txt, collapse = "\n")
  llms_txt <- trimws(llms_txt)

  if (nzchar(llms_txt)) llms_txt else NULL
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
