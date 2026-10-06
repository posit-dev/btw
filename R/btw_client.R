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
#' setting, btw will fall back to settings in a user-level `btw.md` file. This
#' is `~/btw.md` if present, otherwise `~/.btw/btw.md`, `~/.config/btw/btw.md`,
#' or `tools::R_user_dir("btw")`, in that order. If no user-level `btw.md`
#' exists anywhere, btw falls back to a user-level `AGENTS.md` in the
#' cross-tool `~/.agents/` directory; a user-level `CLAUDE.md` is never used.
#' See `?btw-config` for the complete list of user-level locations.
#' Project-level btw tool options under
#' the `options` key are merged with user-level options, with project-level
#' options taking precedence.
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
#' One additional option is consulted when resolving context files:
#'
#' * `btw.client.path_btw_user`: `TRUE` (default) to include user-level context
#'   in new chats, `FALSE` to skip it, or a path to a specific user-level
#'   context file. This is the default for the `user` field of `path_btw`.
#'
#' ## Multiple Providers and Models
#'
#' You can configure multiple client options in your `btw.md` file. When
#' `btw_client()` is called interactively from the console, you'll be presented
#' with a menu to choose which client to use. In non-interactive contexts, the
#' first client is used automatically.
#'
#' **Array format** (unnamed list):
#' ```yaml
#' client:
#'   - anthropic/claude-sonnet-4
#'   - openai/gpt-4.1
#'   - aws_bedrock/us.anthropic.claude-sonnet-4-20250514-v1:0
#' ```
#'
#' **Alias format** (named list):
#' ```yaml
#' client:
#'   haiku: aws_bedrock/us.anthropic.claude-haiku-4-5-20251001-v1:0
#'   sonnet:
#'     provider: aws_bedrock
#'     model: us.anthropic.claude-sonnet-4-5-20250929-v1:0
#' ```
#'
#' With aliases, you can select a client by name in the interactive menu or pass
#' the alias directly: `btw_client(client = "sonnet")`.
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
#' @param client An [ellmer::Chat] client, or a `provider/model` string to be
#'   passed to [ellmer::chat()] to create a chat client, or an alias to a client
#'   setting in your `btw.md` file (see "Multiple Providers" section). Defaults
#'   to [ellmer::chat_anthropic()]. You can use the `btw.client` option to set a
#'   default client for new `btw_client()` calls, or use a `btw.md` project file
#'   for default chat client settings, like provider and model. We check the
#'   `client` argument, then the `btw.client` R option, and finally the `btw.md`
#'   project file (falling back to user-level `btw.md` if needed), using only
#'   the client definition from the first of these that is available.
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
#' @param path_btw Path to a `btw.md`, `AGENTS.md`, or `CLAUDE.md` project
#'   context file, or a named list with `project` and/or `user` fields to
#'   control project-level and user-level context independently. Each field
#'   can be `TRUE` (search the default locations), `FALSE` (skip that scope),
#'   or a path to a specific file; a missing (or `NULL`) field keeps the
#'   default for that scope: `project` searches the project locations, and
#'   `user` follows the `btw.client.path_btw_user` option described below.
#'   If `NULL`, btw will find a
#'   project-specific context file by walking up the parents of the current
#'   working directory, preferring `btw.md`, then `AGENTS.md`, then
#'   `CLAUDE.md`, and always combines it with the user-level context file:
#'   the highest-priority user-level `btw.md`, or if none exists, a
#'   user-level `AGENTS.md` (a user-level `CLAUDE.md` is never used). The
#'   default for the `user` scope can be changed globally with the
#'   `btw.client.path_btw_user` option, e.g. `options(btw.client.path_btw_user
#'   = FALSE)`. A scalar path uses only that file, skipping user-level context
#'   (equivalent to `list(project = path, user = FALSE)`), and a scalar `FALSE`
#'   skips all context files. See `?btw-config` for the complete list of
#'   locations.
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
#' @inheritSection btw-config User (global) locations
#' @inheritSection btw-config Project locations
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

  is_user_interactive <-
    is_interactive() &&
    identical(caller_env(), global_env()) ||
    (identical(fn_env(caller_fn()), fn_env(btw_client)) &&
      identical(caller_env(n = 2), global_env()))

  config <- btw_client_config(
    client,
    tools,
    config = read_btw_file(path_btw),
    is_user_interactive = is_user_interactive
  )
  client <- config$client
  skip_tools <- isFALSE(config$tools) || identical(config$tools, "none")
  withr::local_options(config$options)

  session_info <- btw_tool_sessioninfo_platform()@value
  client_system_prompt <- client$get_system_prompt()

  if (!skip_tools) {
    client$set_tools(tools = c(client$get_tools(), config$tools))
  }

  llms_txt <- read_llms_txt(path_llms_txt)
  project_context <- c(llms_txt, config$btw_system_prompt)
  project_context <- paste(project_context, collapse = "\n\n")

  has_skill_tool <- some(client$get_tools(), \(t) {
    identical(t@name, "btw_tool_skill")
  })
  skills_prompt <- if (has_skill_tool) btw_skills_system_prompt() else ""

  sys_prompt <- c(
    btw_prompt("btw-system_session.md"),
    if (!skip_tools) {
      btw_prompt("btw-system_tools.md")
    },
    if (nzchar(skills_prompt)) {
      skills_prompt
    },
    if (nzchar(project_context)) {
      btw_prompt("btw-system_project.md")
    },
    if (!is.null(client_system_prompt)) "---",
    paste(client_system_prompt, collapse = "\n")
  )

  client$set_system_prompt(paste(sys_prompt, collapse = "\n\n"))

  wrap_built_in_tools(client)
  warn_skills_without_read_file(client$get_tools())

  client
}

btw_client_config <- function(
  client = NULL,
  tools = NULL,
  config = list(),
  is_user_interactive = FALSE
) {
  # Options should be flattened and btw-prefixed by `read_btw_file()`.
  withr::local_options(config$options)

  config$tools <-
    tools %||%
    getOption("btw.tools") %||%
    config$tools %||%
    btw_tools()

  config$tools <- flatten_and_check_tools(config$tools)

  if (!is.null(client)) {
    # Check if client is an alias name that should be resolved from config
    resolved <- resolve_client_alias(client, config$client)
    if (!is.null(resolved)) {
      config$client <- as_ellmer_client(resolved)
    } else {
      config$client <- as_ellmer_client(client)
    }
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
    # Check if this is an array of client configs
    if (is_client_array(config$client)) {
      config$client <- choose_client_from_array(
        config$client,
        is_user_interactive
      )
    }

    # Show informational message for list configs with model specified
    show_model_info <-
      is.list(config$client) &&
      !is.null(config$client$model) &&
      !isTRUE(getOption("btw.client.quiet"))

    config$client <- as_ellmer_client(config$client)

    if (show_model_info) {
      cli::cli_inform(
        "Using {.field {config$client$get_model()}} from {.strong {config$client$get_provider()@name}}."
      )
    }
    return(config)
  }

  config$client <- btw_default_chat_client()
  config
}

btw_default_chat_client <- function() {
  ellmer::chat_anthropic(echo = "output")
}

as_ellmer_client <- function(client) {
  if (inherits(client, "Chat")) {
    return(client)
  }

  if (is_string(client)) {
    return(ellmer::chat(client, echo = "output"))
  }

  # Handle list/mapping configuration (e.g., from YAML frontmatter)
  # Example: client: {provider: aws_bedrock, model: claude-sonnet-4}
  if (is.list(client) && !is.null(client$provider)) {
    chat_args <- utils::modifyList(
      list(echo = "output"),
      client
    )

    chat_fn <- gsub(" ", "_", tolower(chat_args$provider))
    if (!grepl("^chat_", chat_fn)) {
      chat_fn <- paste0("chat_", chat_fn)
    }
    chat_args$provider <- NULL

    chat_client <- call2(.ns = "ellmer", chat_fn, !!!chat_args)
    return(eval(chat_client))
  }

  cli::cli_abort(c(
    "{.arg client} must be an {.help ellmer::Chat} client, a {.val provider/model} string, or a list with {.field provider} (and optionally {.field model}).",
    "i" = "Examples: {.or {.val {c('openai/gpt-4.1-mini', 'anthropic/claude-sonnet-4-20250514')}}}.",
    "i" = "Or as a list: {.code list(provider = 'anthropic', model = 'claude-sonnet-4-20250514')}"
  ))
}

# --- Client array/alias utilities ---
# A "client array" is multiple client configs: character vector, unnamed list,
# or a named alias map. Single configs are: strings, Chat objects, or lists
# with a 'provider' key.

client_aliases <- function(clients) {
  # Returns alias names if clients is an alias map, NULL otherwise.
  # An alias map is a named list where names are not 'provider' or 'model'.
  if (!is.list(clients) || length(clients) == 0) {
    return(NULL)
  }
  nms <- names(clients)
  if (
    is.null(nms) || !all(nzchar(nms)) || any(nms %in% c("provider", "model"))
  ) {
    return(NULL)
  }
  nms
}

is_client_array <- function(client) {
  if (is.character(client)) {
    return(length(client) > 1)
  }
  if (
    !is.list(client) || inherits(client, "Chat") || !is.null(client$provider)
  ) {
    return(FALSE)
  }
  # Alias map or unnamed list with elements
  !is.null(client_aliases(client)) ||
    (is.null(names(client)) && length(client) > 0)
}

resolve_client_alias <- function(name, clients) {
  # Resolve an alias name from a client alias map (case-insensitive).
  # Returns the resolved config, or NULL if not found.
  if (!is_string(name)) {
    return(NULL)
  }
  aliases <- client_aliases(clients)
  if (is.null(aliases)) {
    return(NULL)
  }
  idx <- match(tolower(name), tolower(aliases))
  if (is.na(idx)) NULL else clients[[idx]]
}

format_client_label <- function(client, alias = NULL, default = FALSE) {
  # Parse string format into list
  if (is_string(client)) {
    parts <- strsplit(client, "/", fixed = TRUE)[[1]]
    client <- list(
      provider = parts[1],
      model = if (length(parts) > 1) paste(parts[-1], collapse = "/")
    )
  }

  default <- if (default) cli::col_red(" [default]") else ""

  # Format provider/model
  if (is.list(client) && !is.null(client$provider)) {
    label <- if (!is.null(client$model)) {
      cli::format_inline("{.field {client$provider}}/{.strong {client$model}}")
    } else {
      cli::format_inline("{.field {client$provider}}")
    }
    if (!is.null(alias)) {
      alias <- cli::col_magenta(alias)
      label <- cli::format_inline("{.strong {alias}}: {label}")
    }
  } else if (!is.null(alias)) {
    label <- cli::format_inline("{.strong {alias}}: <unknown>")
  } else {
    label <- "<unknown>"
  }

  paste0(label, default)
}

choose_client_from_array <- function(clients, is_user_interactive = FALSE) {
  if (length(clients) == 0) {
    cli::cli_abort("No client configurations provided.")
  }
  if (length(clients) == 1 || !is_user_interactive || !is_interactive()) {
    return(clients[[1]])
  }

  aliases <- client_aliases(clients)
  labels <- vapply(
    seq_along(clients),
    function(i) {
      format_client_label(clients[[i]], alias = aliases[i], default = i == 1)
    },
    character(1)
  )

  # Display menu
  cli::cli_h2("Select a client")
  cli::cli_ol(labels)
  cli::cli_text("")

  cli::cli_div(
    theme = list(span.subtle = list(color = "silver"))
  )
  prompt <- cli::format_inline(
    "Enter choice {.subtle [1-{n_clients}{alias}, or 0 to exit]}: ",
    .envir = env(
      n_clients = length(clients),
      alias = if (!is.null(aliases)) ", alias name" else ""
    )
  )

  repeat {
    choice <- readline(prompt = prompt)

    if (choice == "0") {
      cli::cli_abort("Aborted by user.")
    }
    if (choice == "") {
      return(clients[[1]])
    }

    # Try alias match
    resolved <- resolve_client_alias(choice, clients)
    if (!is.null(resolved)) {
      return(resolved)
    }

    # Try numeric
    idx <- suppressWarnings(as.integer(choice))
    if (!is.na(idx) && idx >= 1 && idx <= length(clients)) {
      return(clients[[idx]])
    }

    if (!is.null(aliases)) {
      cli::cli_alert_warning(
        "Invalid choice. Enter a number (1-{length(clients)}) or alias name."
      )
    } else {
      cli::cli_alert_warning(
        "Invalid choice. Enter a number between 1 and {length(clients)}."
      )
    }
  }
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
  # Keys that should be treated as leaf values (not recursed into)
  # even if they contain nested lists
  leaf_keys <- c("client")

  out <- list()

  recurse <- function(x, key_prefix, current_key = "") {
    is_leaf_key <- current_key %in% leaf_keys

    # If x is a list and not a leaf key, dive deeper
    if (is.list(x) && !is.data.frame(x) && !is_leaf_key) {
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
        recurse(x[[i]], new_key, current_key = nm[i])
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

# Default for the `user` scope of `path_btw` when it isn't specified: an R
# option so users can opt out of (or pin) user-level context globally, e.g.
# `options(btw.client.path_btw_user = FALSE)` in ~/.Rprofile.
btw_user_path_default <- function() {
  user <- getOption("btw.client.path_btw_user", TRUE)
  if (isTRUE(user) || isFALSE(user)) {
    return(user)
  }
  check_string(user, arg = "btw.client.path_btw_user")
  user
}

# Normalize the `path_btw` argument of btw_client()/btw_app() into a list with
# `project` and `user` fields, each TRUE (search the default locations), FALSE
# (skip that scope), or a path to a specific file.
#
# Scalar values: NULL/TRUE search project and user locations (with the user
# default from `btw.client.path_btw_user`), FALSE skips context entirely, and
# a string uses only that file as the project context, skipping user-level
# context -- a shortcut for list(project = path, user = FALSE).
#
# A named list controls each scope independently: a missing (or NULL) field
# keeps the default for that scope -- `project` searches project locations,
# and `user` uses the `btw.client.path_btw_user` option default.
normalize_path_btw <- function(path_btw, arg = "path_btw") {
  if (is.list(path_btw)) {
    nms <- names(path_btw)
    if (
      length(path_btw) == 0 ||
        is.null(nms) ||
        any(!nzchar(nms)) ||
        !all(nms %in% c("project", "user"))
    ) {
      cli::cli_abort(c(
        "{.arg {arg}} must be a named list with {.code project} and/or {.code user} fields.",
        "i" = "Each field can be {.code TRUE} (search the default locations), {.code FALSE} (skip), or a path to a file."
      ))
    }
    return(list(
      project = normalize_path_btw_field(path_btw$project, TRUE, paste0(arg, "$project")),
      user = normalize_path_btw_field(path_btw$user, btw_user_path_default(), paste0(arg, "$user"))
    ))
  }

  if (is.null(path_btw) || isTRUE(path_btw)) {
    return(list(
      project = TRUE,
      user = btw_user_path_default()
    ))
  }

  if (isFALSE(path_btw)) {
    return(list(project = FALSE, user = FALSE))
  }

  check_string(path_btw, arg = arg)
  list(project = path_btw, user = FALSE)
}

normalize_path_btw_field <- function(value, default, arg) {
  if (is.null(value)) {
    default
  } else if (isTRUE(value) || isFALSE(value)) {
    value
  } else {
    check_string(value, arg = arg)
    value
  }
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

  # 4. User-level btw.md file, falling back to a user-level AGENTS.md
  if (search_user && is.null(path)) {
    path <- path_find_user_context()
  }

  path
}

# Read a single btw.md file and extract its YAML front matter and body
read_single_btw_file <- function(path) {
  if (is.null(path) || !fs::file_exists(path)) {
    return(list())
  }

  fm <- frontmatter::read_front_matter(path)

  # For CLAUDE.md files, ignore YAML frontmatter for config
  is_claude_md <- basename(path) == "CLAUDE.md"
  config <- if (!is_claude_md) fm$data %||% list() else list()

  btw_system_prompt <- remove_hidden_content(fm$body %||% "")
  btw_system_prompt <- trimws(btw_system_prompt)
  if (nzchar(btw_system_prompt)) {
    config$btw_system_prompt <- btw_system_prompt
  }

  config
}

read_btw_file <- function(path = NULL) {
  paths <- normalize_path_btw(path)

  # Locate the project-level context file: search, skip, or an explicit path
  project_path <- if (isTRUE(paths$project)) {
    find_btw_context_file(NULL, search_user = FALSE)
  } else if (isFALSE(paths$project)) {
    NULL
  } else {
    maybe_find_in_project(paths$project, "btw.md", "path_btw")
  }

  # Locate the user-level context file: the highest-priority user-level
  # btw.md, or if none exists anywhere, the highest-priority user-level
  # AGENTS.md (a user-level CLAUDE.md is never used)
  user_path <- if (isTRUE(paths$user)) {
    path_find_user_context()
  } else if (isFALSE(paths$user)) {
    NULL
  } else {
    user_path <- fs::path_norm(fs::path_expand(paths$user))
    if (!fs::file_exists(user_path)) {
      cli::cli_abort(c(
        "x" = "Invalid {.arg path_btw$user}: {.path {user_path}} does not exist.",
        "i" = "Point {.code path_btw$user} at an existing file, or set {.code user = FALSE} to skip user-level context."
      ))
    }
    user_path
  }

  # If the project path was set explicitly to the same file as the user-level
  # config, don't read it twice.
  if (!is.null(user_path) && path_same(user_path, project_path)) {
    user_path <- NULL
  }

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

remove_hidden_content <- function(text) {
  if (!nzchar(text)) {
    return("")
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  starts <- cumsum(trimws(lines) == "<!-- HIDE -->")
  ends <- trimws(lines) == "<!-- /HIDE -->"

  # Shift ends to avoid including /HIDE
  shift <- function(x) c(0, x[-length(x)])

  ends[starts - cumsum(ends) < 0 & ends] <- FALSE

  paste(lines[starts - shift(cumsum(ends)) <= 0], collapse = "\n")
}

warn_skills_without_read_file <- function(tools) {
  tool_names <- names(tools)
  has_skills <- "btw_tool_skill" %in% tool_names
  has_read_file <- "btw_tool_files_read" %in% tool_names
  if (has_skills && !has_read_file) {
    cli::cli_warn(c(
      "The {.fn btw_tool_skill} tool is enabled but {.fn btw_tool_files_read} is not.",
      "i" = "Skills work best with the read file tool, which lets the model read skill resource files.",
      "i" = "Add {.code btw_tools(\"files\")} or enable {.fn btw_tool_files_read} to get full skill support."
    ))
  }
}
