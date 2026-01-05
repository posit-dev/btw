#' @include tool-result.R
NULL

BtwSubagentResult <- S7::new_class(
  "BtwSubagentResult",
  parent = BtwToolResult,
  properties = list(
    session_id = S7::class_character
  )
)

#' Tool: Subagent
#'
#' @description
#' `btw_tool_agent_subagent()` is a btw tool that enables hierarchical agent
#' workflows. When used by an LLM assistant (like [btw_app()], [btw_client()],
#' or third-party tools like Claude Code), this tool allows the orchestrating
#' agent to delegate complex tasks to specialized subagents, each with their own
#' isolated conversation thread and tool access.
#'
#' This function is primarily intended to be called by LLM assistants via tool
#' use, not directly by end users.
#'
#' ## How Subagents Work
#'
#' When an LLM calls this tool:
#'
#' 1. A new chat session is created (or an existing one is resumed)
#' 2. The subagent receives the `prompt` and begins working with only the tools
#'    specified in the `tools` parameter
#' 3. The subagent works independently, making tool calls until it completes
#'    the task
#' 4. The function returns the subagent's final message text and a `session_id`
#' 5. The orchestrating agent can resume the session later by providing the
#'    `session_id`
#'
#' Each subagent maintains its own conversation context, separate from the
#' orchestrating agent's context. Subagent sessions persist for the duration of
#' the R session.
#'
#' ## Tool Access
#'
#' The orchestrating agent must specify which tools the subagent can use via
#' the `tools` parameter. The subagent is restricted to only these tools - it
#' cannot access tools from the parent session. Tools can be specified by:
#'
#' * **Specific tool names**: `c("btw_tool_files_read_text_file",
#'   "btw_tool_files_write_text_file")`
#' * **Tool groups**: `"files"` includes all file-related tools
#' * **NULL** (default): Uses the default tool set from options or
#'   `btw_tools()`
#'
#' ## Configuration Options
#'
#' Subagent behavior can be configured via R options:
#'
#' * `btw.subagent.client`: The ellmer::Chat client or `provider/model` string
#'   to use for subagents. If not set, falls back to `btw.client`, then to the
#'   default Anthropic client.
#'
#' * `btw.subagent.tools_default`: Default tools to provide to subagents when
#'   the orchestrating agent doesn't specify tools via the `tools` parameter.
#'   If not set, falls back to `btw.tools`, then all btw tools from
#'   `btw_tools()`. This is a convenience option for setting reasonable
#'   defaults.
#'
#' * `btw.subagent.tools_allowed`: An allowlist of tools that subagents are
#'   allowed to use at all. When set, any tools requested (either explicitly via
#'   the `tools` parameter or from defaults) will be filtered against this list.
#'   If disallowed tools are requested, an error is thrown. This provides a
#'   security boundary to restrict subagent capabilities. If not set, all
#'   [btw_tools()] are allowed.
#'
#' These options follow the precedence: function argument > `btw.subagent.*`
#' option > `btw.*` option > default value. The `tools_allowed` option acts as a
#' filter on top of the resolved tools, regardless of their source.
#'
#' @examples
#' \dontrun{
#' # Typically used by LLMs via tool use, but can be called directly for testing
#' result <- btw_tool_agent_subagent(
#'   prompt = "List all R files in the current directory",
#'   tools = c("btw_tool_files_list_files")
#' )
#'
#' # Access the subagent's response and session ID
#' cat(result@value)
#' session_id <- result@session_id
#'
#' # Resume the same session with a follow-up
#' result2 <- btw_tool_agent_subagent(
#'   prompt = "Now read the first file you found",
#'   tools = c("btw_tool_files_read_text_file"),
#'   session_id = session_id
#' )
#'
#' # Configure default tools for subagents
#' withr::local_options(list(
#'   btw.subagent.client = "anthropic/claude-sonnet-4-20250514",
#'   btw.subagent.tools_default = "files"  # Default to file tools only
#' ))
#'
#' result3 <- btw_tool_agent_subagent(
#'   prompt = "Find all TODO comments in R files"
#' )
#'
#' # Restrict subagents to a whitelist of allowed tools
#' withr::local_options(list(
#'   btw.subagent.tools_allowed = c("files", "search"),
#'   btw.subagent.tools_default = "files"
#' ))
#'
#' # This works - files tools are allowed
#' result4 <- btw_tool_agent_subagent(
#'   prompt = "List R files",
#'   tools = "files"
#' )
#'
#' # This would error - github tools are not in the allowed list
#' tryCatch(
#'   btw_tool_agent_subagent(
#'     prompt = "Create a GitHub issue",
#'     tools = "github"
#'   ),
#'   error = function(e) message("Error: ", e$message)
#' )
#' }
#'
#' @param prompt Character string with the task description for the subagent.
#'   The subagent will work on this task using only the tools specified in
#'   `tools`. The subagent does not have access to the orchestrating agent's
#'   conversation history.
#' @param tools Optional character vector of tool names or tool groups that the
#'   subagent is allowed to use. Can be specific tool names (e.g.,
#'   `"btw_tool_files_read_text_file"`), tool group names (e.g., `"files"`), or
#'   `NULL` to use the default tools from `btw.subagent.tools_default`,
#'   `btw.tools`, or `btw_tools()`.
#' @param session_id Optional character string with a session ID from a
#'   previous call. When provided, resumes the existing subagent conversation
#'   instead of starting a new one. Session IDs are returned in the result and
#'   have the format "adjective_noun" (e.g., "swift_falcon").
#' @param _intent Optional string describing the intent of the tool call. Added
#'   automatically by the ellmer framework when tools are called by LLMs.
#'
#' @return A `BtwSubagentResult` object (inherits from `BtwToolResult`) with:
#' * `value`: The final message text from the subagent
#' * `session_id`: The session identifier for resuming this conversation
#'
#' @seealso [btw_tools()] for available tools and tool groups
#' @family agent tools
#' @export
btw_tool_agent_subagent <- function(
  prompt,
  tools = NULL,
  session_id = NULL,
  `_intent`
) {}


#' Get existing session or create new one
#'
#' @param session_id Optional session ID to retrieve
#' @param create_chat_fn Function that creates a new Chat when called
#' @return List with `chat`, `session_id`, and `is_new`
#' @noRd
subagent_get_or_create_session <- function(session_id, create_chat_fn) {
  check_string(session_id, allow_null = TRUE)

  if (!is.null(session_id)) {
    session <- subagent_get_session(session_id)

    if (is.null(session)) {
      cli::cli_abort(c(
        "Session not found: {.val {session_id}}",
        "i" = "The session may have expired or the ID is incorrect.",
        "i" = "Omit {.arg session_id} to start a new session."
      ))
    }

    return(list(chat = session$chat, session_id = session_id, is_new = FALSE))
  }

  session_id <- subagent_new_session_id()
  chat <- create_chat_fn()
  subagent_store_session(session_id, chat)

  list(chat = chat, session_id = session_id, is_new = TRUE)
}


#' Process agent chat response into result components
#'
#' @param chat The Chat object after running
#' @param prompt The original prompt
#' @param agent_name Name for the response tag (e.g., "subagent" or custom agent name)
#' @param session_id The session ID
#' @return List with message_text, tokens, tool_calls, provider, model, tool_names
#' @noRd
subagent_process_result <- function(chat, prompt, agent_name, session_id) {
  # Extract last turn message
  last_turn <- chat$last_turn()
  message_text <- if (is.null(last_turn) || length(last_turn@contents) == 0) {
    "(The agent completed successfully but returned no message.)"
  } else {
    ellmer::contents_markdown(last_turn)
  }

  # Format with XML-like wrapper - "subagent" uses <subagent-response>, others use <agent-response>
  if (agent_name == "subagent") {
    message_text <- sprintf(
      '<subagent-response session_id="%s">\n%s\n</subagent-response>',
      session_id,
      message_text
    )
  } else {
    message_text <- sprintf(
      '<agent-response agent="%s" session_id="%s">\n%s\n</agent-response>',
      agent_name,
      session_id,
      message_text
    )
  }

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

  list(
    message_text = message_text,
    tokens = tokens,
    tool_calls = tool_calls,
    chat_round = chat2,
    provider = chat$get_provider()@name,
    model = chat$get_model(),
    tool_names = paste(
      sprintf("`%s`", names(chat$get_tools())),
      collapse = ", "
    )
  )
}


#' Generate display markdown for agent result
#'
#' @param result List returned from subagent_process_result() containing
#'   message_text, tokens, tool_calls, provider, model, and tool_names
#' @param session_id Session ID
#' @param agent_name Agent name (NULL or "subagent" for subagent, otherwise custom agent name)
#' @param prompt The prompt text
#' @return Markdown string for display
#' @noRd
subagent_display_result <- function(result, session_id, agent_name, prompt) {
  # Only show agent line for custom agents, not for subagent
  agent_line <- if (!is.null(agent_name) && agent_name != "subagent") {
    sprintf("**Agent:** %s<br>\n  ", agent_name)
  } else {
    ""
  }

  chat <- result$chat_round$clone()
  chat$set_turns(chat$get_turns()[-1]) # remove prompt
  chat$set_turns(chat$get_turns()[-length(chat$get_turns())]) # and final response

  full_results <- map(chat$get_turns(), function(turn) {
    turn <- shinychat::contents_shinychat(turn)
    map(turn, function(c) as.character(htmltools::as.tags(c)))
  })
  full_results <- paste(unlist(full_results), collapse = "\n\n")

  glue_(
    r"(
  {{ agent_line }}**Session ID:** {{ session_id }}<br>
  **Provider:** {{ result$provider }}<br>
  **Model:** `{{ result$model }}`<br>
  **Tools:** {{ result$tool_names }}

  #### Prompt

  {{ prompt }}

  #### Tokens

  **Tool Calls:** {{ length(unlist(result$tool_calls)) }}

  {{ md_table(result$tokens) }}

  #### Response

  <details class="mb-2"><summary>Full Conversation</summary>

  {{ full_results }}

  ---

  </details>

  {{ result$message_text }}
  )"
  )
}


#' Resolve agent chat client from options hierarchy
#'
#' Checks for client configuration in the following order:
#' 1. Explicit `client` argument (from agent-*.md file)
#' 2. `btw.subagent.client` R option
#' 3. `btw.md` file's `options.subagent.client`
#' 4. `btw.client` R option
#' 5. `btw.md` file's `client`
#' 6. Default Anthropic client
#'
#' @param client Optional explicit client
#' @return A Chat object
#' @noRd
subagent_resolve_client <- function(client = NULL) {
  # Check explicit argument and R options first
  resolved <- client %||%
    getOption("btw.subagent.client") %||%
    getOption("btw.client")

  if (!is.null(resolved)) {
    return(as_ellmer_client(resolved)$clone())
  }

  # Fall back to btw.md file configuration

  btw_config <- read_btw_file()

  # Check for subagent-specific client in btw.md options
  resolved <- btw_config$options[["btw.subagent.client"]] %||%
    btw_config$client

  if (!is.null(resolved)) {
    return(as_ellmer_client(resolved)$clone())
  }

  btw_default_chat_client()
}


btw_tool_agent_subagent_impl <- function(
  prompt,
  tools = NULL,
  session_id = NULL,
  config = NULL
) {
  check_string(prompt)

  session <- subagent_get_or_create_session(
    session_id,
    create_chat_fn = function() {
      subagent_client(
        client = config$client,
        tools = tools,
        tools_default = config$tools_default,
        tools_allowed = config$tools_allowed
      )
    }
  )

  chat <- session$chat
  session_id <- session$session_id

  response <- chat$chat(prompt)

  result <- subagent_process_result(chat, prompt, "subagent", session_id)

  display_md <- subagent_display_result(
    result = result,
    session_id = session_id,
    agent_name = "subagent",
    prompt = prompt
  )

  BtwSubagentResult(
    value = result$message_text,
    session_id = session_id,
    extra = list(
      prompt = prompt,
      provider = result$provider,
      model = result$model,
      tokens = result$tokens,
      display = list(markdown = display_md, show_request = FALSE)
    )
  )
}

#' Capture subagent configuration from current R options
#'
#' Reads the relevant btw.subagent.* and btw.* options and returns them as a
#' named list for later use by btw_tool_agent_subagent_impl().
#'
#' @return A list with captured configuration
#' @noRd
subagent_config_options <- function() {
  list(
    client = getOption("btw.subagent.client") %||% getOption("btw.client"),
    tools_default = getOption("btw.subagent.tools_default") %||%
      getOption("btw.tools"),
    tools_allowed = getOption("btw.subagent.tools_allowed")
  )
}


#' Configure subagent client
#'
#' Creates and configures an ellmer Chat client for a subagent session. The
#' returned chat object has the system prompt and tools already attached.
#' Follows the precedence: argument > btw.subagent.* option > btw.* option > default
#'
#' @param client Optional Chat object or provider/model string
#' @param tools Optional character vector or list of tool definitions
#' @param tools_default Optional default tools from captured config
#' @param tools_allowed Optional allowed tools whitelist from captured config
#' @return A configured Chat object with system prompt and tools attached
#'
#' @noRd
subagent_client <- function(
  client = NULL,
  tools = NULL,
  tools_default = NULL,
  tools_allowed = NULL
) {
  # Track whether tools were explicitly provided
  tools_explicit <- !is.null(tools)

  # Error immediately if subagent is explicitly requested
  # This provides clear feedback rather than silent filtering
  if (subagent_is_explicitly_requested(tools)) {
    cli::cli_abort(c(
      "Subagents cannot spawn other subagents.",
      "x" = "The {.arg tools} parameter includes {.val btw_tool_agent_subagent}.",
      "i" = "Remove the subagent tool from the tools list."
    ))
  }

  subagent_client_resolved <-
    client %||%
    getOption("btw.subagent.client") %||%
    getOption("btw.client")

  tools_default <-
    tools_default %||%
    getOption("btw.subagent.tools_default") %||%
    getOption("btw.tools")
  tools_default <- subagent_disallow_recursion(tools_default)

  tools_allowed <-
    tools_allowed %||%
    getOption("btw.subagent.tools_allowed")
  # Note: Don't filter subagent from tools_allowed here.
  # The allowed list should be used as-is for validation.
  # The final subagent_disallow_recursion() at the end handles the actual filtering.

  configured_tools <-
    tools %||%
    tools_default %||%
    compact(map(.btw_tools, function(t) {
      if (t$name != "btw_tool_agent_subagent") t$tool()
    }))

  configured_tools <- flatten_and_check_tools(configured_tools)

  # Apply tools_allowed whitelist if set
  if (!is.null(tools_allowed)) {
    # Convert tools_allowed to a flat list of tool names
    allowed_tools <- flatten_and_check_tools(tools_allowed)
    allowed_names <- map_chr(allowed_tools, function(t) t@name)

    # Get names of configured tools
    configured_names <- map_chr(configured_tools, function(t) t@name)

    # Check if any requested tools are not allowed
    disallowed <- setdiff(configured_names, allowed_names)

    # Only error if tools were explicitly provided and include disallowed tools
    if (length(disallowed) > 0 && tools_explicit) {
      cli::cli_abort(c(
        "Subagent requested disallowed tools.",
        "x" = "The following tools are not in {.code btw.subagent.tools_allowed}: {.val {disallowed}}",
        "i" = "Allowed tools: {.val {allowed_names}}",
        "i" = "Set {.code options(btw.subagent.tools_allowed = NULL)} to remove restrictions."
      ))
    }

    # Filter to only allowed tools
    configured_tools <- keep(configured_tools, function(t) {
      t@name %in% allowed_names
    })
  }

  # Never allow subagents to create subagents (prevents infinite recursion)
  # This filtering happens after all tool resolution and allowed-list filtering
  # to ensure the subagent tool is always removed, regardless of how tools were specified
  configured_tools <- subagent_disallow_recursion(configured_tools)

  chat <- if (!is.null(subagent_client_resolved)) {
    as_ellmer_client(subagent_client_resolved)$clone()
  } else {
    btw_default_chat_client()
  }

  system_prompt <- btw_prompt("btw-subagent.md")
  chat$set_system_prompt(system_prompt)
  chat$set_tools(configured_tools)

  chat
}

subagent_disallow_recursion <- function(tools) {
  if (is.null(tools)) {
    return(NULL)
  }

  if (is.character(tools)) {
    return(setdiff(tools, c("btw_tool_agent_subagent", "subagent")))
  }

  keep(tools, function(tool) {
    !inherits(tool, "ellmer::ToolDef") || tool@name != "btw_tool_agent_subagent"
  })
}

#' Check if subagent tool is explicitly requested
#'
#' Detects explicit requests for the subagent tool by name (not via group).
#' Used to provide clear error messages when users try to give subagents
#' the ability to spawn other subagents.
#'
#' @param tools Character vector of tool names/groups or list of ToolDef objects
#' @return TRUE if subagent is explicitly requested by name, FALSE otherwise
#' @noRd
subagent_is_explicitly_requested <- function(tools) {
  if (is.null(tools)) {
    return(FALSE)
  }

  if (is.character(tools)) {
    return("btw_tool_agent_subagent" %in% tools || "subagent" %in% tools)
  }

  if (is.list(tools)) {
    for (t in tools) {
      if (
        inherits(t, "ellmer::ToolDef") && t@name == "btw_tool_agent_subagent"
      ) {
        return(TRUE)
      }
    }
  }

  FALSE
}

#' Build dynamic tool description for btw_tool_agent_subagent
#'
#' Generates a description that includes available tool groups dynamically.
#'
#' @return Character string with the tool description
#'
#' @noRd
subagent_build_description <- function(tools = .btw_tools) {
  desc_tool_use <- if (length(tools) == 0) {
    "No tools are available for use in the subagent."
  } else {
    r"(
CRITICAL - TOOL SELECTION:
You MUST specify which tools the subagent needs using the 'tools' parameter. Choosing the right tools is essential for success:
- Analyze the task requirements carefully
- Select only the specific tools needed for the task
- If uncertain which tools are needed, include relevant tool groups
- The subagent can ONLY use the tools you provide - wrong tools = task failure

AVAILABLE TOOLS FOR SUBAGENT USE:)"
  }

  tool_summary <- if (length(tools) == 0) {
    ""
  } else {
    map_chr(tools, function(tool) {
      if (!inherits(tool, "ellmer::ToolDef")) {
        if (is.function(tool$tool)) {
          tool <- tool$tool()
        } else {
          rlang::abort("Unknown tool definition format.")
        }
      }
      desc <- strsplit(tool@description, "\n|[.](\\s|$)")[[1]][1]
      sprintf("- %s: %s", tool@name, desc)
    })
  }
  tool_summary <- paste(tool_summary, collapse = "\n")

  desc_base <- r"(
Delegate a task to a specialized assistant that can work independently with its own conversation thread.

WHEN TO USE:
- For complex, multi-step tasks that would benefit from focused attention
- When you need to isolate work on a specific subtask
- To resume previous work by providing the session_id from an earlier call
- When you can handle the task yourself with available tools, do so directly instead

BEST PRACTICES:
- Write clear, complete task descriptions in the prompt
- Specify expected output format if important
- Store the returned session_id if you need to continue the work later
- The subagent returns its final answer as plain text
- Each subagent session is independent with its own context)"

  paste0(desc_base, "\n", desc_tool_use, "\n", tool_summary)
}

btw_tool_agent_subagent_from_config <- function(config) {
  force(config)

  function(prompt, tools = NULL, session_id = NULL) {
    btw_tool_agent_subagent_impl(
      prompt = prompt,
      tools = tools,
      session_id = session_id,
      config = config
    )
  }
}

# Helper: Check if subagent tool can register
btw_can_register_subagent_tool <- function() {
  # Prevent registration when resolving tools for subagent description.
  # This breaks the infinite recursion chain that occurs when the tool's
  # $tool() function calls btw_tools() which would try to instantiate
  # this tool again.
  !isTRUE(getOption(".btw_resolving_for_subagent"))
}

# Register the tool
.btw_add_to_tools(
  name = "btw_tool_agent_subagent",
  group = "agent",
  can_register = function() btw_can_register_subagent_tool(),
  tool = function() {
    # Set context flag before any tool resolution to prevent recursion
    withr::local_options(.btw_resolving_for_subagent = TRUE)

    config <- subagent_config_options()
    tools_allowed <- config$tools_allowed

    if (is.null(tools_allowed)) {
      btw_other_tools <- setdiff(names(.btw_tools), "btw_tool_agent_subagent")
      tools_allowed <- map(.btw_tools[btw_other_tools], function(t) t$tool())
    } else {
      tools_allowed <- subagent_disallow_recursion(tools_allowed)
    }

    tools_allowed <- flatten_and_check_tools(tools_allowed)

    ellmer::tool(
      btw_tool_agent_subagent_from_config(config),
      name = "btw_tool_agent_subagent",
      description = subagent_build_description(tools_allowed),
      annotations = ellmer::tool_annotations(
        title = "Subagent",
        read_only_hint = FALSE,
        open_world_hint = TRUE
        # btw_can_register is propagated from can_register by as_ellmer_tools()
      ),
      arguments = list(
        prompt = ellmer::type_string(
          "The complete task description for the subagent. Be specific and clear about requirements and expected output."
        ),
        tools = ellmer::type_array(
          "REQUIRED (in practice): Array of specific tool names to provide to the subagent (e.g., ['btw_tool_files_read_text_file', 'btw_tool_code_search']). Choose tools that match the task requirements. The subagent can ONLY use these tools.",
          items = ellmer::type_string(),
          required = FALSE
        ),
        session_id = ellmer::type_string(
          "Optional: session_id from a previous call to continue that conversation. Omit to start a new session.",
          required = FALSE
        )
      )
    )
  }
)

.btw_subagent_sessions <- new.env(parent = emptyenv())


#' Generate a word-based session ID
#'
#' Creates a human-readable session identifier in the format "adjective-noun"
#' (e.g., "stable-genius", "swift-falcon"). Checks for uniqueness against
#' currently active sessions.
#'
#' @return A character string containing the generated session ID
#' @noRd
subagent_new_session_id <- function() {
  # Try up to 100 times to generate a unique ID
  for (i in seq_len(100)) {
    adj <- sample(.btw_memoids$adjective, 1)
    noun <- sample(.btw_memoids$noun, 1)
    id <- paste(adj, noun, sep = "_")

    if (!env_has(.btw_subagent_sessions, id)) {
      return(id)
    }
  }

  # If we couldn't generate a unique ID after 100 tries, fall back to UUID-style
  cli::cli_warn(c(
    "Could not generate unique word-based ID after 100 attempts.",
    "i" = "Falling back to random suffix."
  ))

  suffix <- sample(1000:9999, 1)
  paste(c(adj, noun, suffix), collapse = "_")
}

#' Store a subagent session
#'
#' Stores a chat object and associated metadata in the session environment.
#'
#' @param session_id Character string with the session identifier
#' @param chat An ellmer Chat object
#' @param metadata Optional list of additional metadata to store
#' @return The session_id (invisibly)
#'
#' @noRd
subagent_store_session <- function(session_id, chat, metadata = list()) {
  check_string(session_id)
  check_inherits(chat, "Chat")

  session_data <- c(
    list(
      id = session_id,
      chat = chat,
      created = Sys.time()
    ),
    metadata
  )

  assign(session_id, session_data, envir = .btw_subagent_sessions)
  invisible(session_id)
}

#' Retrieve a subagent session
#'
#' Retrieves a stored session from the session environment.
#'
#' @param session_id Character string with the session identifier
#' @return A list containing the session data, or NULL if not found
#'
#' @noRd
subagent_get_session <- function(session_id) {
  check_string(session_id)

  env_get(.btw_subagent_sessions, session_id, default = NULL)
}

#' List all active subagent sessions
#'
#' Returns a list with information about all currently active subagent
#' sessions. Useful for debugging and monitoring.
#'
#' @return A list of sessions with: id, chat, created
#'
#' @noRd
subagent_list_sessions <- function() {
  env_get_list(.btw_subagent_sessions, env_names(.btw_subagent_sessions))
}

#' Clear a specific subagent session
#'
#' Explicitly removes a session from the session store. This is optional -
#' sessions will be automatically cleaned up when the R session ends.
#'
#' @param session_id Character string with the session identifier
#' @return TRUE if session was found and removed, FALSE otherwise
#'
#' @noRd
subagent_clear_session <- function(session_id) {
  check_string(session_id)

  if (!env_has(.btw_subagent_sessions, session_id)) {
    return(FALSE)
  }

  rm(list = session_id, envir = .btw_subagent_sessions)
  TRUE
}

#' Clear all subagent sessions
#'
#' Removes all sessions from the session store. This is optional - sessions
#' will be automatically cleaned up when the R session ends.
#'
#' @noRd
subagent_clear_all_sessions <- function() {
  session_ids <- env_names(.btw_subagent_sessions)
  count <- length(session_ids)

  if (count > 0) {
    rm(list = session_ids, envir = .btw_subagent_sessions)
  }

  invisible(count)
}
