#' @include tool-agent-subagent.R
NULL

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
#'   part of the tool name: `btw_tool_agent_{name}`. The final name cannot
#'   conflict with any existing [btw_tools()] names.
#'
#' ### Optional Fields
#'
#' * `description`: Tool description shown to the LLM. Defaults to a generic
#'   delegation message.
#' * `title`: User-facing title for the tool. Defaults to title-cased name.
#' * `icon`: Icon specification for the agent (see **Icon Specification**
#'   below). Defaults to the standard agent icon.
#' * `client`: Model specification like `"anthropic/claude-sonnet-4-20250514"`.
#'   Falls back to `btw.subagent.client` or `btw.client` options.
#' * `tools`: List of tool names or groups available to this agent. Defaults to
#'   all non-agent tools.
#'
#' ### Icon Specification
#'
#' The `icon` field supports three formats:
#'
#' 1. **Plain icon name**: Uses `shiny::icon()` (Font Awesome icons). Example:
#'    `icon: robot` or `icon: code`
#'
#' 2. **Raw SVG**: Starts with `<svg` and is used literally. Example:
#'    `icon: '<svg viewBox="0 0 24 24">...</svg>'`
#'
#' 3. **Package-prefixed icon**: Uses `pkg::icon-name` format to specify icons
#'    from other icon packages. Supported packages:
#'
#'    | Package        | Syntax                      | Function Called          |
#'    |----------------|-----------------------------|--------------------------
#'    | fontawesome    | `fontawesome::home`         | [fontawesome::fa()]      |
#'    | bsicons        | `bsicons::house`            | [bsicons::bs_icon()]     |
#'    | phosphoricons  | `phosphoricons::house`      | [phosphoricons::ph()]    |
#'    | rheroicons     | `rheroicons::home`          | [rheroicons::rheroicon()]|
#'    | tabler         | `tabler::home`              | [tabler::icon()]         |
#'    | shiny          | `shiny::home`               | [shiny::icon()]          |
#'
#'    The specified package must be installed. If the package is missing or the
#'    icon name is invalid, a warning is issued and the default agent icon is
#'    used.
#'
#' ### Example Agent File
#'
#' ```yaml
#' ---
#' name: code_reviewer
#' description: Reviews code for best practices and potential issues.
#' title: Code Reviewer
#' icon: magnifying-glass
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
#' Agent files are automatically discovered by [btw_tools()] when placed in
#' the following locations (in order of priority):
#'
#' * **Project level (btw)**: `.btw/agent-*.md` in your project directory
#' * **User level (btw)**: `~/.btw/agent-*.md` or `~/.config/btw/agent-*.md`
#' * **Project level (Claude Code)**: `.claude/agents/*.md` in your project directory
#' * **User level (Claude Code)**: `~/.claude/agents/*.md`
#'
#' btw-style agents take precedence over Claude Code agents with the same name.
#' When duplicate agent names are found, a warning is issued.
#'
#' ## Claude Code Compatibility
#'
#' btw supports loading agent files from Claude Code's `.claude/agents/`
#' directory for compatibility. However, some Claude Code fields are not
#' supported:
#'
#' * **Name normalization**: Agent names with hyphens (e.g., `code-reviewer`)
#'   are automatically converted to underscores (`code_reviewer`) for R
#'   compatibility.
#' * **Ignored fields**: The following Claude Code fields are ignored (with
#'   a warning): `model`, `tools`, `permissionMode`, `skills`. Use btw's
#'   `client` field instead of `model`, and btw agents use default tools.
#' * **`client` argument**: Use the `client` argument to manually override
#'   the model for any agent file.
#'
#' @param path Path to an agent markdown file.
#' @param client Optional. A client specification to override the agent's
#'   configured client. Can be a string like
#'   `"anthropic/claude-sonnet-4-20250514"`, an [ellmer::Chat] object, or a list
#'   with `provider` and `model` keys. If `NULL` (default), uses the `client`
#'   field from the agent file or falls back to btw's default client resolution.
#'
#' @return An `ellmer::ToolDef` object that can be registered with a chat
#'   client, or `NULL` if the file is invalid (with a warning).
#'
#' @seealso [btw_tools()] for automatic agent discovery, [btw_client()] for
#'   creating chat clients with tools.
#'
#' @examples
#' # Create a btw-style agent file
#' withr::with_tempdir({
#'   dir.create(".btw")
#'   writeLines(
#'     c(
#'       "---",
#'       "name: code_reviewer",
#'       "description: Reviews code for best practices.",
#'       "---",
#'       "",
#'       "You are a code reviewer. Analyze code for best practices."
#'     ),
#'     ".btw/agent-code_reviewer.md"
#'   )
#'
#'   tool <- btw_agent_tool(".btw/agent-code_reviewer.md")
#'   # Use `chat$register_tool(tool)` to register with an ellmer chat client
#'
#'   tool
#' })
#'
#' # Create a Claude Code-style agent file (name with hyphens)
#' withr::with_tempdir({
#'   dir.create(".claude/agents", recursive = TRUE)
#'   writeLines(
#'     c(
#'       "---",
#'       "name: test-helper",
#'       "description: Helps write tests.",
#'       "model: sonnet",
#'       "---",
#'       "",
#'       "You help write tests for R code."
#'     ),
#'     ".claude/agents/test-helper.md"
#'   )
#'
#'   tool <- btw_agent_tool(".claude/agents/test-helper.md")
#'   # Use `chat$register_tool(tool)` to register with an ellmer chat client
#'
#'   tool
#' })
#'
#' @export
btw_agent_tool <- function(path, client = NULL) {
  check_string(path)

  if (!fs::file_exists(path)) {
    cli::cli_abort("Agent file not found: {.path {path}}")
  }

  config <- read_agent_md_file(path)

  if (is.null(config)) {
    return(NULL)
  }

  # Normalize agent name: convert hyphens to underscores for R identifier
  name <- normalize_agent_name(config$name)

  if (!validate_agent_name(name, path)) {
    return(NULL)
  }

  # Warn about unsupported Claude Code fields
  warn_claude_code_unsupported_fields(config, path)

  # Build tool name: btw_tool_agent_{name}
  tool_name <- paste0("btw_tool_agent_", name)

  # Description: use provided or generate default
  description <- config$description %||%
    sprintf("Delegate a task to the %s specialized assistant.", name)

  # Title: use provided or generate from name
  title <- config$title %||% to_title_case(gsub("_", " ", name))

  # Build the agent configuration for btw_tool_agent_custom_impl
  # Note: client argument takes precedence over config$client
  # Note: tools from Claude Code format are ignored (incompatible tool names)
  agent_config <- list(
    name = name,
    client = client %||% config$client,
    tools = if (is_claude_code_agent_file(path)) NULL else config$tools,
    system_prompt = config$system_prompt,
    tools_default = getOption("btw.subagent.tools_default") %||%
      getOption("btw.tools"),
    tools_allowed = getOption("btw.subagent.tools_allowed")
  )

  # Create the tool function with agent_config captured in closure
  tool_fn <- btw_tool_agent_custom_from_config(agent_config)

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
  tool@annotations$icon <- custom_icon(config$icon) %||%
    tool_group_icon("agent")

  tool
}

btw_tool_agent_custom_impl <- function(
  prompt,
  session_id = NULL,
  agent_config
) {
  check_string(prompt)

  session <- subagent_get_or_create_session(
    session_id,
    create_chat_fn = function() {
      custom_agent_client_from_config(agent_config)
    }
  )

  chat <- session$chat
  session_id <- session$session_id

  response <- chat$chat(prompt)

  result <- subagent_process_result(chat, prompt, agent_config$name, session_id)

  display_md <- subagent_display_result(
    result = result,
    session_id = session_id,
    agent_name = agent_config$name,
    prompt = prompt
  )

  BtwSubagentResult(
    value = result$message_text,
    session_id = session_id,
    extra = list(
      prompt = prompt,
      agent = agent_config$name,
      provider = result$provider,
      model = result$model,
      tokens = result$tokens,
      display = list(markdown = display_md, show_request = FALSE)
    )
  )
}

# Create a configured ellmer Chat client for a custom agent session
custom_agent_client_from_config <- function(agent_config) {
  chat <- subagent_resolve_client(agent_config$client)

  # Determine tools
  tools_default <- agent_config$tools_default %||%
    getOption("btw.subagent.tools_default") %||%
    getOption("btw.tools")

  tools_allowed <- agent_config$tools_allowed %||%
    getOption("btw.subagent.tools_allowed")

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
    configured_tools <- keep(configured_tools, function(t) {
      t@name %in% allowed_names
    })
  }

  # Build system prompt
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

btw_tool_agent_custom_from_config <- function(agent_config) {
  force(agent_config)

  function(prompt, session_id = NULL) {
    btw_tool_agent_custom_impl(
      prompt = prompt,
      session_id = session_id,
      agent_config = agent_config
    )
  }
}

# Discover agent definition files from project and user directories.
# Priority order (highest first): project .btw/, user .btw/, user .config/btw/,
# project .claude/agents/, user .claude/agents/
discover_agent_md_files <- function() {
  # btw locations (highest priority)
  project_btw <- find_project_agent_files()
  user_btw <- find_user_agent_files()

  # Claude Code locations (lower priority)
  project_cc <- find_project_claude_code_agent_files()
  user_cc <- find_user_claude_code_agent_files()

  unique(c(project_btw, user_btw, project_cc, user_cc))
}

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

# Ensures the agent name is a valid R identifier and not reserved.
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

# Converts Claude Code style names (with hyphens) to valid R identifiers
normalize_agent_name <- function(name) {
  if (is.null(name)) {
    return(NULL)
  }
  gsub("-", "_", name, fixed = TRUE)
}

#' Check if file is from Claude Code agent directory
#'
#' Determines if the agent file is from a Claude Code `.claude/agents/` directory
#' rather than btw's `.btw/` directory.
#'
#' @param path Path to the agent file
#' @return TRUE if file is from Claude Code directory, FALSE otherwise
#' @noRd
is_claude_code_agent_file <- function(path) {
  grepl("[\\/]\\.claude[\\/]agents[\\/]", path)
}

#' Warn about unsupported fields in Claude Code agent files
#'
#' Issues a warning if a Claude Code agent file contains fields that
#' btw does not support: model, tools, permissionMode, skills.
#' Only warns for files from `.claude/agents/` directories.
#'
#' @param config Agent configuration list from YAML frontmatter
#' @param path Path to the agent file (for error messages)
#' @return NULL (called for side effect)
#' @noRd
warn_claude_code_unsupported_fields <- function(config, path) {
  # Only warn for Claude Code agent files
  if (!is_claude_code_agent_file(path)) {
    return(invisible(NULL))
  }

  unsupported_fields <- c("model", "tools", "permissionMode", "skills")
  present <- intersect(names(config), unsupported_fields)

  if (length(present) > 0) {
    cli::cli_warn(c(
      "Unsupported Claude Code fields in {.path {path}}",
      "i" = "btw ignores: {.field {present}}"
    ))
  }

  invisible(NULL)
}

#' Resolve custom icon from string specification
#'
#' Parses an icon specification and returns the appropriate icon object.
#'
#' @param icon_spec String specifying the icon. Can be:
#'   - Raw SVG: starts with `<svg` - wrapped in `htmltools::HTML()`
#'   - Package icon: `pkg::icon-name` format (e.g., `fontawesome::home`)
#'   - Shiny icon: just the icon name (uses `shiny::icon()`)
#'
#' Supported packages:
#' - `fontawesome`: uses `fa("icon-name")`
#' - `bsicons`: uses `bs_icon("icon-name")`
#' - `phosphoricons`: uses `ph("icon-name")`
#' - `rheroicons`: uses `rheroicon("icon-name")`
#' - `tabler`: uses `icon("icon-name")`
#' - `shiny`: uses `icon("icon-name")`
#'
#' @return An icon object (shiny.tag) or NULL if the icon cannot be resolved
#' @noRd
custom_icon <- function(icon_spec) {
  if (is.null(icon_spec) || !nzchar(icon_spec)) {
    return(NULL)
  }

  # Raw SVG: wrap in HTML()
  if (grepl("^\\s*<svg", icon_spec, ignore.case = TRUE)) {
    return(htmltools::HTML(icon_spec))
  }

  # Default to shiny::icon()
  pkg <- "shiny"
  icon_fn <- "icon"
  icon_name <- icon_spec

  # Parse package prefix if present: pkg::icon-name
  if (grepl("::", icon_spec, fixed = TRUE)) {
    parts <- strsplit(icon_spec, "::", fixed = TRUE)[[1]]
    if (length(parts) != 2) {
      cli::cli_warn("Invalid icon specification: {.val {icon_spec}}")
      return(NULL)
    }

    pkg <- parts[1]
    icon_name <- parts[2]

    # Map package to function name
    icon_fn <- switch(
      pkg,
      fontawesome = "fa",
      bsicons = "bs_icon",
      phosphoricons = "ph",
      rheroicons = "rheroicon",
      tabler = "icon",
      shiny = "icon",
      NULL
    )

    if (is.null(icon_fn)) {
      cli::cli_warn("Unknown icon package: {.val {pkg}}")
      return(NULL)
    }
  }

  # Check if package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_warn(
      "Package {.pkg {pkg}} is not installed for icon {.val {icon_spec}}"
    )
    return(NULL)
  }

  # Get the function from the package namespace
  ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
  if (is.null(ns)) {
    cli::cli_warn("Cannot access namespace for package {.pkg {pkg}}")
    return(NULL)
  }

  fn <- ns[[icon_fn]]
  if (is.null(fn) || !is.function(fn)) {
    cli::cli_warn(
      "Function {.fn {icon_fn}} not found in package {.pkg {pkg}}"
    )
    return(NULL)
  }

  # Call the icon function, catching errors and "unknown icon" messages
  unknown_icon <- FALSE

  result <- tryCatch(
    withCallingHandlers(
      fn(icon_name),
      message = function(m) {
        if (grepl("does not correspond to a known icon", conditionMessage(m))) {
          unknown_icon <<- TRUE
          invokeRestart("muffleMessage")
        }
      }
    ),
    error = function(e) {
      cli::cli_warn(c(
        "Error creating icon {.val {icon_name}} from {.pkg {pkg}}",
        "x" = conditionMessage(e)
      ))
      NULL
    }
  )

  if (unknown_icon) {
    pkg_fn <- paste0(pkg, "::", icon_fn)
    cli::cli_warn(
      "Icon {.val {icon_name}} is not supported by {.fn {pkg_fn}}."
    )
    return(NULL)
  }

  result
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
custom_agent_discover_tools <- function() {
  files <- discover_agent_md_files()

  if (length(files) == 0) {
    return(list())
  }

  tools <- list()
  # Track which file each tool came from for conflict warnings
  tool_sources <- list()

  for (file in files) {
    tryCatch(
      {
        tool <- btw_agent_tool(file)

        if (!is.null(tool)) {
          tool_name <- tool@name

          # Check for name conflict (duplicate agent after normalization)
          if (tool_name %in% names(tools)) {
            cli::cli_warn(c(
              "Skipping duplicate agent {.val {tool_name}} from {.path {file}}",
              "i" = "An agent with this name was already loaded from {.path {tool_sources[[tool_name]]}}."
            ))
            next
          }

          # Track source file for conflict warnings
          tool_sources[[tool_name]] <- file

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
