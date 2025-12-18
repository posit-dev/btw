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
#' a `btw.md` file, [`AGENTS.md`](https://agents.md/), or `CLAUDE.md` in your
#' project directory. See [use_btw_md()] for help creating a `btw.md` file in
#' your project, or use `path_btw` to tell `btw_client()` to use a specific
#' context file. Note that `CLAUDE.md` files will have their YAML frontmatter
#' stripped but not used for configuration.
#'
#' `btw_client()` will also include context from an `llms.txt` file in the
#' system prompt, if one is found in your project directory or as specified by
#' the `path_llms_txt` argument.
#'
#' ## Client Settings with User-Level Fallback
#'
#' Client settings in `client` and `tools` from a project-level `btw.md` or
#' `AGENTS.md` file take precedence. If a project file doesn't specify a
#' setting, btw will fall back to settings in a user-level `btw.md` file
#' (typically in `~/btw.md` or `~/.config/btw/btw.md`). Project-level btw tool
#' options under the `options` key are merged with user-level options, with
#' project-level options taking precedence.
#'
#' Project-specific instructions from both files are combined with a divider,
#' allowing you to maintain global guidelines in your user file and
#' project-specific rules in your project file.
#'
#' ## Client Options
#'
#' The following R options are consulted when creating a new btw chat client and
#' take precedence over settings in a `btw.md` file:
#'
#' * `btw.client`: The [ellmer::Chat] client or a `provider/model` string (see
#'    [ellmer::chat()]) to use as the basis for new `btw_client()` or
#'    `btw_app()` chats.
#' * `btw.tools`: The btw tools to include by default when starting a new
#'   btw chat, see [btw_tools()] for details.`
#'
#' @examplesIf rlang::is_interactive()
#' withr::local_options(list(
#'   btw.client = ellmer::chat_ollama(model="llama3.1:8b")
#' ))
#'
#' chat <- btw_client()
#' chat$chat(
#'   "How can I replace `stop()` calls with functions from the cli package?"
#' )
#'
#' @param client An [ellmer::Chat] client or a `provider/model` string to be
#'   passed to [ellmer::chat()] to create a chat client. Defaults to
#'   [ellmer::chat_anthropic()]. You can use the `btw.client` option to set a
#'   default client for new `btw_client()` calls, or use a `btw.md` project file
#'   for default chat client settings, like provider and model. We check the
#'   `client` argument, then the `btw.client` R option, and finally the `btw.md`
#'   project file (falling back to user-level `btw.md` if needed), using only the
#'   client definition from the first of these that is available.
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
#' @param path_btw A path to a `btw.md`, `AGENTS.md`, or `CLAUDE.md` project
#'   context file. If `NULL`, btw will find a project-specific `btw.md`,
#'   `AGENTS.md`, or `CLAUDE.md` file in the parents of the current working
#'   directory, with fallback to user-level `btw.md` if no project file is
#'   found. Set `path_btw = FALSE` to create a chat client without using a
#'   `btw.md` file.
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
  # Options should be flattened and btw-prefixed by `read_btw_file()`.
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

    if (!is.null(chat_args$model) && !isTRUE(getOption("btw.client.quiet"))) {
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

    if (is.null(tool)) {
      next
    }

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
        if (nzchar(key_prefix)) {
          new_key <- paste(key_prefix, nm[i], sep = sep)
        } else {
          new_key <- nm[i]
        }
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

maybe_find_in_project <- function(path, file_name, arg = "path") {
  if (isFALSE(path)) {
    return(NULL)
  }

  must_find <- !is.null(path)

  if (isTRUE(path)) {
    path <- NULL
  }

  path <- path %||% path_find_in_project(file_name)

  if (!must_find && is.null(path)) {
    return(NULL)
  }

  if (must_find && (is.null(path) || !fs::file_exists(path))) {
    cli::cli_abort("Invalid {.arg {arg}}: {.path {path}} does not exist.")
  }

  path
}

find_btw_context_file <- function(path = NULL, search_user = TRUE) {
  # 1. Local closest btw.md file
  path <- maybe_find_in_project(path, "btw.md", "path_btw")

  # 2. Local closest AGENTS.md file
  if (is.null(path)) {
    path <- maybe_find_in_project(NULL, "AGENTS.md", "path_btw")
  }

  # 3. Local closest CLAUDE.md file
  if (is.null(path)) {
    path <- maybe_find_in_project(NULL, "CLAUDE.md", "path_btw")
  }

  # 4. User btw.md file
  if (search_user && is.null(path)) {
    path <- path_find_user("btw.md")
  }

  path
}

# Read a single btw.md file and extract its YAML front matter and body
read_single_btw_file <- function(path) {
  if (is.null(path) || !fs::file_exists(path)) {
    return(list())
  }

  # For CLAUDE.md files, ignore YAML frontmatter for config
  is_claude_md <- basename(path) == "CLAUDE.md"

  config <- if (!is_claude_md) rmarkdown::yaml_front_matter(path) else list()

  read_without_yaml <- function(path) {
    pyfm <- asNamespace("rmarkdown")[["partition_yaml_front_matter"]]
    pyfm(read_lines(path))$body
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

read_btw_file <- function(path = NULL) {
  if (isFALSE(path)) {
    return(list())
  }

  # Find project-level and user-level files
  project_path <- maybe_find_in_project(path, "btw.md", "path_btw")
  if (is.null(project_path)) {
    project_path <- maybe_find_in_project(NULL, "AGENTS.md", "path_btw")
  }
  if (is.null(project_path)) {
    project_path <- maybe_find_in_project(NULL, "CLAUDE.md", "path_btw")
  }

  user_path <- path_find_user("btw.md")

  # If no files found, return empty config
  if (is.null(project_path) && is.null(user_path)) {
    return(list())
  }

  project_config <- read_single_btw_file(project_path)
  user_config <- read_single_btw_file(user_path)

  # Merge configs ----
  # 1. Shallow merge for 'client' and 'tools': project wins if present
  config <- list(
    client = project_config$client %||% user_config$client,
    tools = project_config$tools %||% user_config$tools
  )

  # 2. Deep merge for 'options': both are flattened, project overrides
  user_options <- flatten_config_options(user_config$options)
  project_options <- flatten_config_options(project_config$options)
  config$options <- utils::modifyList(user_options, project_options)

  # 3. System prompts are concatenated with a separator
  prompts <- c(user_config$btw_system_prompt, project_config$btw_system_prompt)
  prompts <- prompts[nzchar(prompts)]
  if (length(prompts) > 0) {
    config$btw_system_prompt <- paste(prompts, collapse = "\n\n---\n\n")
  }

  # Copy over any other config keys that aren't special-cased above
  # (project wins, then user)
  for (key in setdiff(names(project_config), names(config))) {
    config[[key]] <- project_config[[key]]
  }
  for (key in setdiff(names(user_config), names(config))) {
    config[[key]] <- user_config[[key]]
  }

  config
}

read_llms_txt <- function(path = NULL) {
  path <- maybe_find_in_project(path, "llms.txt", "path_llms_txt")

  if (is.null(path)) {
    return(NULL)
  }

  llms_txt <- read_file(path)
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
