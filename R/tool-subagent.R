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
#' `btw_tool_subagent()` is a btw tool that enables hierarchical agent
#' workflows. When used by an LLM assistant (like Claude), this tool allows the
#' orchestrating agent to delegate complex tasks to specialized subagents, each
#' with their own isolated conversation thread and tool access.
#'
#' This function is primarily intended to be called by LLM assistants via tool
#' use, not directly by end users. However, it can be useful for testing and
#' debugging hierarchical workflows in R.
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
#' * `btw.subagent.tools`: Default tools to make available to subagents. If not
#'   set, falls back to `btw.tools`, then to all btw tools from `btw_tools()`.
#'
#' These options follow the precedence: function argument > `btw.subagent.*`
#' option > `btw.*` option > default value.
#'
#' @examples
#' \dontrun{
#' # Typically used by LLMs via tool use, but can be called directly for testing
#' result <- btw_tool_subagent(
#'   prompt = "List all R files in the current directory",
#'   tools = c("btw_tool_files_list_files")
#' )
#'
#' # Access the subagent's response and session ID
#' cat(result@value)
#' session_id <- result@session_id
#'
#' # Resume the same session with a follow-up
#' result2 <- btw_tool_subagent(
#'   prompt = "Now read the first file you found",
#'   tools = c("btw_tool_files_read_text_file"),
#'   session_id = session_id
#' )
#'
#' # Configure the subagent client via options
#' withr::local_options(list(
#'   btw.subagent.client = "anthropic/claude-sonnet-4-20250514",
#'   btw.subagent.tools = "files"  # Default to file tools only
#' ))
#'
#' result3 <- btw_tool_subagent(
#'   prompt = "Find all TODO comments in R files"
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
#'   `NULL` to use the default tools from `btw.subagent.tools` or `btw_tools()`.
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
btw_tool_subagent <- function(
  prompt,
  tools = NULL,
  session_id = NULL,
  `_intent`
) {}


btw_tool_subagent_impl <- function(
  prompt,
  tools = NULL,
  session_id = NULL
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

    # TODO: Add turn limit tracking. Currently we can't limit turns within a subagent
    # because the chat$chat() method doesn't expose turn count control.
  } else {
    session_id <- generate_session_id()
    chat <- btw_subagent_client_config(client = NULL, tools = tools)
    store_session(session_id, chat)
  }

  response <- chat$chat(prompt)

  last_turn <- chat$last_turn()
  message_text <- if (is.null(last_turn) || length(last_turn@contents) == 0) {
    ""
  } else {
    text_contents <- keep(
      last_turn@contents,
      function(x) S7::S7_inherits(x, ellmer::ContentText)
    )
    if (length(text_contents) > 0) {
      paste(map_chr(text_contents, function(x) x@text), collapse = "\n\n")
    } else {
      ""
    }
  }

  # We could update session metadata here, but `chat` is stateful

  BtwSubagentResult(
    value = message_text,
    session_id = session_id,
    extra = list(
      data = list(
        chat = chat
      )
    )
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
#' @return A configured Chat object with system prompt and tools attached
#'
#' @noRd
btw_subagent_client_config <- function(client = NULL, tools = NULL) {
  configured_tools <-
    tools %||%
    getOption("btw.subagent.tools") %||%
    getOption("btw.tools") %||%
    btw_tools()

  configured_tools <- flatten_and_check_tools(configured_tools)

  chat <- if (!is.null(client)) {
    as_ellmer_client(client)$clone()
  } else if (!is.null(subagent_client <- getOption("btw.subagent.client"))) {
    as_ellmer_client(subagent_client)$clone()
  } else if (!is.null(default_client <- getOption("btw.client"))) {
    as_ellmer_client(default_client)$clone()
  } else {
    btw_default_chat_client()
  }

  system_prompt <- btw_prompt("btw-subagent.md")
  chat$set_system_prompt(system_prompt)
  chat$set_tools(configured_tools)

  chat
}

#' Build dynamic tool description for btw_tool_subagent
#'
#' Generates a description that includes available tool groups dynamically.
#'
#' @return Character string with the tool description
#'
#' @noRd
build_subagent_description <- function() {
  # Get unique tool groups from registered tools
  tool_groups <- unique(map_chr(.btw_tools, function(x) x$group))
  tool_groups <- sort(tool_groups)

  # Build tool groups summary
  if (length(tool_groups) > 0) {
    tool_summary <- paste(
      "\n\nAvailable tool groups:",
      paste(tool_groups, collapse = ", ")
    )
  } else {
    tool_summary <- "\n\nNo tool groups currently registered."
  }

  base_desc <- "Delegate a task to a specialized assistant that can work independently with its own conversation thread.

WHEN TO USE:
- For complex, multi-step tasks that would benefit from focused attention
- When you need to isolate work on a specific subtask
- To resume previous work by providing the session_id from an earlier call
- When you can handle the task yourself with available tools, do so directly instead

CRITICAL - TOOL SELECTION:
You MUST specify which tools the subagent needs using the 'tools' parameter. Choosing the right tools is essential for success:
- Analyze the task requirements carefully
- Select only the specific tools needed (e.g., ['btw_tool_files_read_text_file', 'btw_tool_files_write_text_file'] for file tasks)
- If uncertain which tools are needed, include relevant tool groups
- The subagent can ONLY use the tools you provide - wrong tools = task failure

BEST PRACTICES:
- Write clear, complete task descriptions in the prompt
- Specify expected output format if important
- Store the returned session_id if you need to continue the work later
- The subagent returns its final answer as plain text
- Each subagent session is independent with its own context"

  paste0(base_desc, tool_summary)
}

# Register the tool
.btw_add_to_tools(
  name = "btw_tool_subagent",
  group = "agent",
  tool = function() {
    ellmer::tool(
      function(prompt, tools = NULL, session_id = NULL) {
        btw_tool_subagent_impl(
          prompt = prompt,
          tools = tools,
          session_id = session_id
        )
      },
      name = "btw_tool_subagent",
      description = build_subagent_description(),
      annotations = ellmer::tool_annotations(
        title = "Subagent",
        read_only_hint = FALSE,
        open_world_hint = TRUE,
        btw_can_register = function() TRUE
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

.btw_adjectives <- c(
  "agile",
  "bold",
  "bright",
  "calm",
  "clever",
  "daring",
  "eager",
  "elegant",
  "fair",
  "fierce",
  "gentle",
  "happy",
  "jolly",
  "keen",
  "lively",
  "merry",
  "nimble",
  "noble",
  "placid",
  "quick",
  "quiet",
  "rapid",
  "serene",
  "shy",
  "silent",
  "smooth",
  "stable",
  "steady",
  "swift",
  "tranquil",
  "valiant",
  "vibrant",
  "vigilant",
  "vivid",
  "warm",
  "wise",
  "witty",
  "zealous"
)

.btw_nouns <- c(
  "aardvark",
  "badger",
  "beaver",
  "cheetah",
  "dolphin",
  "eagle",
  "falcon",
  "gazelle",
  "hawk",
  "jaguar",
  "kangaroo",
  "leopard",
  "lynx",
  "meerkat",
  "otter",
  "panther",
  "penguin",
  "puffin",
  "rabbit",
  "raven",
  "salmon",
  "sparrow",
  "squirrel",
  "starling",
  "swift",
  "tiger",
  "turtle",
  "viper",
  "walrus",
  "weasel",
  "whale",
  "wolf",
  "wombat",
  "zebra"
)

#' Generate a word-based session ID
#'
#' Creates a human-readable session identifier in the format "adjective-noun"
#' (e.g., "stable-genius", "swift-falcon"). Checks for uniqueness against
#' currently active sessions.
#'
#' @return A character string containing the generated session ID
#' @keywords internal
generate_session_id <- function() {
  # Try up to 100 times to generate a unique ID
  for (i in seq_len(100)) {
    adj <- sample(.btw_adjectives, 1)
    noun <- sample(.btw_nouns, 1)
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

  adj <- sample(.btw_adjectives, 1)
  noun <- sample(.btw_nouns, 1)
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
store_session <- function(session_id, chat, metadata = list()) {
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
#' @keywords noRd
retrieve_session <- function(session_id) {
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
list_subagent_sessions <- function() {
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
clear_subagent_session <- function(session_id) {
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
clear_all_subagent_sessions <- function() {
  session_ids <- env_names(.btw_subagent_sessions)
  count <- length(session_ids)

  if (count > 0) {
    rm(list = session_ids, envir = .btw_subagent_sessions)
  }

  invisible(count)
}
