#' Tool: Create An Agent
#'
#' A btw agent is simply an [ellmer::Chat] client wrapped into a tool call.
#'
#' @param client An [ellmer::Chat] client. If not provided, uses `btw_client()`
#'   by default.
#' @param system_prompt Additional instructions added to the system prompt of
#'   `client` that describe the agent's task and how it should operate.
#' @param turns A list of [ellmer::Turn]s or an [ellmer::Chat] object that
#'   provides the initial turns used by the `client`. For example, you can use
#'   this argument to pass a chat client with a long chat history to ask the
#'   agent to summarize the chat history.
#' @param mirai_profile The name of the \pkg{mirai} `.compute` profile to use
#'   for this agent, see [mirai::mirai()] for details.
#' @param setup_code Additional code to run first before invoking the `$chat()`
#'   method of `client`. Use this argument to load required packages, for
#'   example if `client` uses tools from custom packages.
#' @param ... Additional arguments passed to [mirai::mirai()]. Can be used to
#'   adjust [mirai::mirai()] options or to pass values from the parent session
#'   when they are used in `setup_code`.
#'
#' @return Returns an [ellmer::tool()] that creates an agent
btw_tool_agent <- function(
  ...,
  client = NULL,
  system_prompt = NULL,
  turns = NULL,
  mirai_profile = "sidechat",
  setup_code = NULL
) {
  rlang::check_installed("mirai")
  check_character(system_prompt, allow_null = TRUE)
  check_string(mirai_profile)

  setup_code <- rlang::enexpr(setup_code)

  client <- client %||% getOption("btw.__this_client__", btw_client())
  check_inherits(client, "Chat")

  if (!is.null(turns)) {
    ok_turns <- is_list(turns) || is_function(turns) || inherits(turns, "Chat")
    if (!ok_turns) {
      cli::cli_abort(
        "{.var turns} must be a list of {.fn ellmer::Turn}s, an {.code ellmer::Chat}, or a function, not {.obj_type_friendly {turns}}."
      )
    }
  }

  if (!is.null(system_prompt)) {
    client$set_system_prompt(c(client$get_system_prompt(), system_prompt))
  }

  sidechat_fn <- function(prompt) {
    the_client <- client$clone()

    if (!is.null(turns)) {
      # Set turns as late as possible, `turns` could be a function or a ref to
      # a chat that may have changed since the tool definition was created.
      if (inherits(turns, "Chat")) {
        turns <- turns$get_turns()
      }
      if (is_function(turns)) {
        turns <- turns()
      }
      the_client$set_turns(turns)
    }

    m <- mirai::mirai(
      {
        library(btw)
        !!!setup_code
        the_client$chat(prompt)
      },
      the_client = the_client,
      prompt = prompt,
      ...,
      .compute = mirai_profile
    )

    promises::as.promise(m)
  }

  ellmer::tool(
    sidechat_fn,
    .name = "sidechat",
    .description = r"(Research R packages and how to use them.

Use this tool to research R package and provide code snippets for how to use
them to perform a specific task. Give each function call a specific task or
question. For best results, call this tool in parallel with each research task.
    )",
    .annotations = ellmer::tool_annotations(
      title = "Sidechat Researcher",
      read_only_hint = TRUE,
      open_world_hint = FALSE,
      idempotent_hint = FALSE
    ),
    prompt = ellmer::type_string(
      "The prompt to send to the research assistant. This should be a specific task or question about R packages."
    )
  )
}

# chat_claude_sonnet <- ellmer::chat_aws_bedrock(
#   model = "us.anthropic.claude-sonnet-4-20250514-v1:0"
# )
# chat_claude_haiku <- ellmer::chat_aws_bedrock(
#   model = "us.anthropic.claude-3-5-haiku-20241022-v1:0"
# )

# tool_sidechat_researcher <- btw_tool_sidechat(
#   client = btw_client(
#     tools = c("docs", "session", "search"),
#     client = chat_claude_haiku$clone()
#   ),
#   system_prompt = r"(
# You are a helpful research assistant whose job it is to find the right R
# package for a user. When asked for a package that performs a specific task,
# you'll look for the most relevant package. If the package is installed and
# available to the user, research how to use the package and provide a short code
# snippet that demonstrates how to use it. If the package is not installed,
# describe the package and how to install it.
#    )"
# )

# client <- btw_client(client = chat_claude_sonnet)
# client$register_tool(tool_sidechat_researcher)

# # btw_app(client = client)
# # Ask:
# # Explain how I can scrape a table from a website using R. The table is in a wide format and I know I'll need to make it tidy.
