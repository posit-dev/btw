#' Summarize Chat Conversation
#'
#' Creates a concise summary of a chat conversation, removing false starts and
#' focusing on key outcomes and findings. This is a direct function that can be
#' called without the agent wrapper.
#'
#' @param turns A list of [ellmer::Turn]s or an [ellmer::Chat] object that
#'   provides the conversation history to summarize. If not provided, uses the
#'   chat history from `client`.
#' @param client An [ellmer::Chat] client that will be used to summarize the
#'   chat. Must be provided if `turns` is not an [ellmer::Chat] object.
#' @param additional_guidance Optional prompt to guide the summarization focus.
#'   If not provided, creates a general comprehensive summary.
#' @param start_turn Integer specifying which turn to start summarizing from.
#'   Default is 0, meaning summarize the entire conversation history.
#'
#' @returns A character string containing the conversation summary.
#'
#' @examples
#' \dontrun{
#' # Summarize an entire chat
#' summary <- btw_task_summarize_chat(my_chat)
#'
#' # Summarize with specific focus
#' summary <- btw_task_summarize_chat(my_chat, "Focus on the R programming solutions")
#'
#' # Summarize starting from turn 5
#' summary <- btw_task_summarize_chat(my_chat, start_turn = 5)
#' }
#'
#' @family tasks
#' @export
btw_task_summarize_chat <- function(
  turns,
  client = NULL,
  additional_guidance = "",
  start_turn = 0
) {
  if (inherits(turns, "Chat")) {
    if (is.null(client)) {
      client <- turns
    } else {
      check_inherits(client, "Chat")
    }
  } else if (is.null(client)) {
    stop("If 'turns' is not a Chat object, 'client' must be provided.")
  }

  check_string(additional_guidance)
  turns <- turns_simplify(turns)
  check_number_whole(start_turn, min = 0, max = as.numeric(length(turns)))

  # Create clone of the client to avoid modifying the original client
  summary_client <- client$clone()
  # No tools required
  summary_client$set_tools(list())

  if (start_turn > 0) {
    turns <- turns[-seq_len(start_turn - 1)]
  }

  summary_client$set_turns(turns)

  summary_client$set_system_prompt(NULL)

  res <- summary_client$chat(
    ellmer::interpolate_file(
      system.file("prompts", "chat-summary.md", package = "btw"),
      additional_guidance = additional_guidance
    )
  )

  attr(res, "client") <- summary_client
  invisible(res)
}

turns_simplify <- function(turns) {
  if (inherits(turns, "Chat")) {
    turns <- turns$get_turns()
  }

  # Simplify contents to flatten tool requests/results, avoiding API errors that
  # happen when there are tool requests/results in the chat history, but no tools.
  for (i in seq_along(turns)) {
    turns[[i]]@contents <- map(turns[[i]]@contents, function(content) {
      is_tool_request <- S7::S7_inherits(content, ellmer::ContentToolRequest)
      is_tool_result <- S7::S7_inherits(content, ellmer::ContentToolResult)

      if (is_tool_request || is_tool_result) {
        content <- ellmer::ContentText(format(content))
      }

      content
    })
  }

  turns
}
