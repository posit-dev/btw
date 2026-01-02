#' @include tool-agent-subagent.R
NULL

#' Discover agent-*.md files from project and user directories
#'
#' Scans for custom agent definition files in:
#' - `.btw/agent-*.md` (project level)
#' - `~/.btw/agent-*.md` (user level)
#' - `~/.config/btw/agent-*.md` (user level)
#'
#' @return Character vector of absolute paths to agent-*.md files
#' @noRd
discover_agent_md_files <- function() {
  project_files <- find_project_agent_files()
  user_files <- find_user_agent_files()
  unique(c(project_files, user_files))
}

#' Read and parse an agent-*.md file
#'
#' Wrapper around `read_single_btw_file()` that extracts YAML frontmatter
#' and body content from an agent definition file.
#'
#' @param path Path to the agent-*.md file
#' @return List with YAML config and body content (system_prompt)
#' @noRd
read_agent_md_file <- function(path) {
  if (!fs::file_exists(path)) {
    return(NULL)
  }

  config <- read_single_btw_file(path)

  # Rename btw_system_prompt to system_prompt for agent configs
  if (!is.null(config$btw_system_prompt)) {
    config$system_prompt <- config$btw_system_prompt
    config$btw_system_prompt <- NULL
  }

  config
}

#' Validate agent name
#'
#' Ensures the agent name is a valid R identifier and not reserved.
#'
#' @param name The agent name from YAML frontmatter
#' @param path Path to the file (for error messages)
#' @return TRUE if valid, otherwise signals an error
#' @noRd
validate_agent_name <- function(name, path) {
  check_string(name, allow_null = TRUE)

  if (is.null(name) || !nzchar(name)) {
    cli::cli_warn(c(
      "Agent file has no name: {.path {path}}",
      "i" = "Add {.code name: agent_name} to the YAML frontmatter.",
      "i" = "Skipping this file."
    ))
    return(FALSE)
  }

  # Check for reserved name
  if (name %in% names(.btw_tools)) {
    cli::cli_warn(c(
      "Agent name cannot be {.val {name}}: {.path {path}}",
      "i" = "The name {.val {name}} is reserved. Skipping this file."
    ))
    return(FALSE)
  }

  # Check if valid R identifier
  if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", name)) {
    cli::cli_warn(c(
      "Invalid agent name {.val {name}}: {.path {path}}",
      "i" = "Agent names must be valid R identifiers (letters, numbers, underscores).",
      "i" = "Names must start with a letter.",
      "i" = "Skipping this file."
    ))
    return(FALSE)
  }

  TRUE
}

#' Create a custom agent tool from a markdown file
#'
#' @description
#' Creates an [ellmer::tool()] from a markdown file that defines a custom agent.
#' The tool can be registered with a chat client to delegate tasks to a
#' specialized assistant with its own system prompt and tool configuration.
#'
#' ## Agent File Format
#'
#' Agent files use YAML frontmatter to configure the agent, with the markdown
#' body becoming the agent's system prompt. The file should be named
#' `agent-{name}.md`.
#'
#' ### Required Fields
#'
#' * `name`: A valid R identifier (letters, numbers, underscores) that becomes
#'   part of the tool name. Cannot be `"subagent"` (reserved).
#'
#' ### Optional Fields
#'
#' * `description`: Tool description shown to the LLM. Defaults to a generic
#'   delegation message.
#' * `title`: User-facing title for the tool. Defaults to title-cased name.
#' * `icon`: Font Awesome icon name (e.g., `"robot"`, `"code"`). Defaults to
#'   the standard agent icon.
#' * `client`: Model specification like `"anthropic/claude-sonnet-4-20250514"`.
#'   Falls back to `btw.subagent.client` or `btw.client` options.
#' * `tools`: List of tool names or groups available to this agent. Defaults to
#'   all non-agent tools.
#'
#' ### Example Agent File
#'
#' ```yaml
#' ---
#' name: code_reviewer
#' description: Reviews code for best practices and potential issues.
#' title: Code Reviewer
#' icon: magnifying-glass-chart
#' tools:
#'   - files
#'   - docs
#' ---
#'
#' You are a code reviewer. Analyze code for:
#' - Best practices and style

#' - Potential bugs or issues
#' - Performance considerations
#'
#' Provide specific, actionable feedback.
#' ```
#'
#' ## Automatic Discovery
#'
#' Agent files are automatically discovered by [btw_tools()] when placed in:
#'
#' * **Project level**: `.btw/agent-*.md` in your project directory
#' * **User level**: `~/.btw/agent-*.md` or `~/.config/btw/agent-*.md`
#'
#' Project-level agents take precedence over user-level agents with the same
#' name.
#'
#' @param path Path to an agent markdown file (`agent-*.md`).
#'
#' @return An `ellmer::ToolDef` object that can be registered with a chat
#'   client, or `NULL` if the file is invalid (with a warning).
#'
#' @seealso [btw_tools()] for automatic agent discovery, [btw_client()] for
#'   creating chat clients with tools.
#'
#' @examples
#' \dontrun{
#' # Create a tool from a specific agent file
#' tool <- btw_agent_tool("path/to/agent-reviewer.md")
#'
#' # Register with a chat client
#' chat <- ellmer::chat_anthropic()
#' chat$register_tool(tool)
#'
#' # Or include with other btw tools
#' chat$register_tools(c(btw_tools("docs"), tool))
#' }
#'
#' @export
btw_agent_tool <- function(path) {
  check_string(path)

  if (!fs::file_exists(path)) {
    cli::cli_abort("Agent file not found: {.path {path}}")
  }

  config <- read_agent_md_file(path)

  if (is.null(config)) {
    return(NULL)
  }

  name <- config$name

  if (!validate_agent_name(name, path)) {
    return(NULL)
  }

  # Build tool name: btw_tool_agent_{name}
  tool_name <- paste0("btw_tool_agent_", name)

  # Description: use provided or generate default
  description <- config$description %||%
    sprintf("Delegate a task to the %s specialized assistant.", name)

  # Title: use provided or generate from name
  title <- config$title %||% to_title_case(gsub("_", " ", name))

  # Build the agent configuration for btw_tool_agent_custom_impl
  agent_config <- list(
    name = name,
    client = config$client,
    tools = config$tools,
    system_prompt = config$system_prompt,
    tools_default = getOption("btw.subagent.tools_default") %||%
      getOption("btw.tools"),
    tools_allowed = getOption("btw.subagent.tools_allowed")
  )

  # Create the tool function with agent_config captured in closure
  tool_fn <- btw_tool_agent_custom_config(agent_config)

  # Build the ellmer::tool()
  tool <- ellmer::tool(
    tool_fn,
    name = tool_name,
    description = description,
    annotations = ellmer::tool_annotations(
      title = title,
      read_only_hint = FALSE,
      open_world_hint = TRUE
    ),
    arguments = list(
      prompt = ellmer::type_string(
        "The complete task description for the agent. Be specific and clear about requirements and expected output."
      ),
      session_id = ellmer::type_string(
        "Optional: session_id from a previous call to continue that conversation. Omit to start a new session.",
        required = FALSE
      )
    )
  )

  # Set icon if specified, otherwise use default agent icon
  if (!is.null(config$icon) && nzchar(config$icon)) {
    tryCatch(
      {
        tool@annotations$icon <- shiny::icon(config$icon)
      },
      error = function(e) {
        cli::cli_warn(c(
          "Invalid icon {.val {config$icon}} for agent {.val {name}}: {.path {path}}",
          "i" = "Using default agent icon.",
          "x" = conditionMessage(e)
        ))
        tool@annotations$icon <<- tool_group_icon("agent")
      }
    )
  } else {
    tool@annotations$icon <- tool_group_icon("agent")
  }

  tool
}

#' Execute custom subagent
#'
#' Implementation function that executes a custom agent with its configuration.
#' This reuses the session management and execution logic from btw_tool_agent_subagent_impl.
#'
#' @param prompt Task description for the agent
#' @param session_id Optional session ID to resume a conversation
#' @param agent_config Configuration for this custom agent
#' @return A BtwSubagentResult object
#' @noRd
btw_tool_agent_custom_impl <- function(
  prompt,
  session_id = NULL,
  agent_config
) {
  check_string(prompt)
  check_string(session_id, allow_null = TRUE)

  if (!is.null(session_id)) {
    session <- retrieve_session(session_id)

    if (is.null(session)) {
      cli::cli_abort(c(
        "Session not found: {.val {session_id}}",
        "i" = "The session may have expired or the ID is incorrect.",
        "i" = "Omit {.arg session_id} to start a new session."
      ))
    }

    chat <- session$chat
  } else {
    session_id <- generate_session_id()
    chat <- btw_custom_agent_client_config(agent_config)
    store_session(session_id, chat)
  }

  response <- chat$chat(prompt)

  last_turn <- chat$last_turn()
  message_text <- if (is.null(last_turn) || length(last_turn@contents) == 0) {
    "(The agent completed successfully but returned no message.)"
  } else {
    ellmer::contents_markdown(last_turn)
  }
  message_text <- sprintf(
    '<agent-response agent="%s" session_id="%s">\n%s\n</agent-response>',
    agent_config$name,
    session_id,
    message_text
  )

  # Get tokens for just this round
  idx_prompt <- which(map_lgl(chat$get_turns(), function(t) {
    t@role == "user" && identical(ellmer::contents_text(t), prompt)
  }))
  chat2 <- chat$clone()
  if (idx_prompt > 1) {
    chat2$set_turns(chat2$get_turns()[-seq_len(idx_prompt - 1)])
  }
  tokens <- chat2$get_tokens()
  for (i in seq_len(ncol(tokens))) {
    if (is.numeric(tokens[[i]])) {
      tokens[[i]] <- format(tokens[[i]], big.mark = ",")
    }
  }

  tool_calls <- map(chat2$get_turns(), function(turn) {
    keep(turn@contents, S7::S7_inherits, ellmer::ContentToolRequest)
  })

  provider <- chat$get_provider()@name
  model <- chat$get_model()
  tool_names <- paste(
    sprintf("`%s`", names(chat$get_tools())),
    collapse = ", "
  )

  display_md <- glue_(
    r"(
  #### Prompt

  **Agent:** {{ agent_config$name }}<br>
  **Session ID:** {{ session_id }}<br>
  **Provider:** {{ provider }}<br>
  **Model:** `{{ model }}`<br>
  **Tools:** {{ tool_names }}

  {{ prompt }}

  #### Tokens

  **Tool Calls:** {{ length(unlist(tool_calls)) }}

  {{ md_table(tokens) }}

  #### Response

  {{ message_text }}
  )"
  )

  BtwSubagentResult(
    value = message_text,
    session_id = session_id,
    extra = list(
      prompt = prompt,
      agent = agent_config$name,
      provider = provider,
      model = model,
      tokens = tokens,
      display = list(
        markdown = display_md,
        show_request = FALSE
      )
    )
  )
}

#' Configure custom agent client
#'
#' Creates and configures an ellmer Chat client for a custom agent session.
#' Similar to btw_subagent_client_config but uses agent-specific configuration.
#'
#' @param agent_config List with agent configuration
#' @return A configured Chat object with system prompt and tools attached
#' @noRd
btw_custom_agent_client_config <- function(agent_config) {
  # Determine client
  custom_client <-
    agent_config$client %||%
    getOption("btw.subagent.client") %||%
    getOption("btw.client")

  # Determine tools
  tools_default <-
    agent_config$tools_default %||%
    getOption("btw.subagent.tools_default") %||%
    getOption("btw.tools")

  tools_allowed <-
    agent_config$tools_allowed %||%
    getOption("btw.subagent.tools_allowed")

  # If agent specifies tools, use them; otherwise use defaults (non-agent tools)
  configured_tools <- if (!is.null(agent_config$tools)) {
    agent_config$tools
  } else if (!is.null(tools_default)) {
    tools_default
  } else {
    # Default: all non-agent tools
    compact(map(.btw_tools, function(t) {
      if (!grepl("^btw_tool_agent_", t$name)) t$tool()
    }))
  }

  configured_tools <- flatten_and_check_tools(configured_tools)

  # Apply tools_allowed whitelist if set
  if (!is.null(tools_allowed)) {
    allowed_tools <- flatten_and_check_tools(tools_allowed)
    allowed_names <- map_chr(allowed_tools, function(t) t@name)
    configured_names <- map_chr(configured_tools, function(t) t@name)

    # Filter to only allowed tools (no error for custom agents)
    configured_tools <- keep(configured_tools, function(t) {
      t@name %in% allowed_names
    })
  }

  # Create chat client
  chat <- if (!is.null(custom_client)) {
    as_ellmer_client(custom_client)$clone()
  } else {
    btw_default_chat_client()
  }

  # Build system prompt: base subagent prompt + custom agent prompt
  base_prompt <- btw_prompt("btw-subagent.md")
  custom_prompt <- agent_config$system_prompt %||% ""

  system_prompt <- if (nzchar(custom_prompt)) {
    paste(base_prompt, custom_prompt, sep = "\n\n---\n\n")
  } else {
    base_prompt
  }

  chat$set_system_prompt(system_prompt)
  chat$set_tools(configured_tools)

  chat
}

#' Create tool function with captured agent configuration
#'
#' Returns a closure that captures the agent_config and calls btw_tool_agent_custom_impl.
#'
#' @param agent_config List with agent configuration
#' @return Function that implements the tool
#' @noRd
btw_tool_agent_custom_config <- function(agent_config) {
  force(agent_config)

  function(prompt, session_id = NULL) {
    btw_tool_agent_custom_impl(
      prompt = prompt,
      session_id = session_id,
      agent_config = agent_config
    )
  }
}

#' Get custom agent tools with lazy discovery and caching
#'
#' Discovers agent-*.md files, validates and loads them, and returns
#' a list of tool definitions ready to be added to .btw_tools.
#'
#' Called during tool registration to dynamically add custom agent tools.
#'
#' @return Named list of tool definitions compatible with .btw_add_to_tools
#' @noRd
get_custom_agent_tools <- function() {
  files <- discover_agent_md_files()

  if (length(files) == 0) {
    return(list())
  }

  tools <- list()

  for (file in files) {
    tryCatch(
      {
        tool <- btw_agent_tool(file)

        if (!is.null(tool)) {
          tool_name <- tool@name
          # Use local() to properly capture tool in closure
          tools[[tool_name]] <- local({
            captured_tool <- tool
            list(
              name = tool_name,
              group = "agent",
              tool = function() captured_tool
            )
          })
        }
      },
      error = function(e) {
        cli::cli_warn(c(
          "Error loading custom agent: {.path {file}}",
          "x" = conditionMessage(e),
          "i" = "Skipping this file."
        ))
      }
    )
  }

  tools
}

#' Register custom agent tools
#'
#' This function is called to dynamically register custom agents found in
#' agent-*.md files. It's separated from the discovery logic to allow
#' registration to happen at the appropriate time during package load.
#'
#' @noRd
register_custom_agent_tools <- function() {
  tools <- get_custom_agent_tools()

  for (tool_name in names(tools)) {
    tool_def <- tools[[tool_name]]
    .btw_add_to_tools(
      name = tool_def$name,
      group = tool_def$group,
      tool = tool_def$tool
    )
  }

  invisible(NULL)
}
