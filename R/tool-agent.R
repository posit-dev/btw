#' Tool: Create An Agent
#'
#' A btw agent is simply an [ellmer::Chat] client wrapped into a tool call.
#'
#' @param name The name of the agent tool. This is used to identify the tool
#'   in the chat client or in the tool registry.
#' @param ... Ignored.
#' @param description A description of the agent tool, describing how and when
#'   it should be used. This provides context to the chat client that will
#'   invoke the agent.
#' @param title The title of the agent tool, used when displaying the tool
#'   in a tool listing or when showing the agent tool's response.
#' @param system_prompt Additional instructions added to the system prompt of
#'   `client` that describe the agent's task and how it should operate.
#' @param client An [ellmer::Chat] client. If not provided, uses `btw_client()`
#'   by default.
#' @param turns A list of [ellmer::Turn]s or an [ellmer::Chat] object that
#'   provides the initial turns used by the `client`. For example, you can use
#'   this argument to pass a chat client with a long chat history to ask the
#'   agent to summarize the chat history.
#' @param tools A list of tools to use with the agent. See [btw_client()] for
#'   details on how to specify tools or [btw_tools()] for the list of built-in
#'   tools. If provided, `tools` replaces the set of tools in `client`; if
#'   `NULL` the `client` retains its existing registered tools.
#' @param mirai_profile The name of the \pkg{mirai} `.compute` profile to use
#'   for this agent, see [mirai::mirai()] for details.
#' @param setup_code Additional code to run first before invoking the `$chat()`
#'   method of `client`. Use this argument to load required packages, for
#'   example if `client` uses tools from custom packages.
#'
#' @return Returns an agent as an [ellmer::tool()].
#'
#' @family agents
#' @export
btw_tool_agent <- function(
  name,
  ...,
  description = "",
  title = NULL,
  system_prompt = NULL,
  client = NULL,
  turns = NULL,
  tools = NULL,
  mirai_profile = "btw_agent",
  setup_code = NULL
) {
  rlang::check_installed("mirai")
  check_dots_empty()
  check_character(system_prompt, allow_null = TRUE)
  check_string(mirai_profile)
  check_string(setup_code, allow_null = TRUE, allow_empty = TRUE)

  client <- client %||% getOption("btw.__agent_client__", btw_client())
  check_inherits(client, "Chat")

  # Clone the client to avoid modifying the original client. It would be
  # surprising if this function modified the client in place (e.g. by adding
  # tools or updating the system prompt).
  client <- client$clone()

  if (isFALSE(turns) || is_na(turns) || identical(turns, "")) {
    turns <- NULL
  } else if (isTRUE(turns)) {
    turns <- getOption("btw.__agent_turns__", NULL)
  }

  if (!is.null(tools)) {
    tools <- flatten_and_check_tools(tools)
    client$set_tools(tools)
  }

  if (!is.null(turns)) {
    ok_turns <- is_list(turns) || is_function(turns) || inherits(turns, "Chat")
    if (!ok_turns) {
      cli::cli_abort(
        "{.var turns} must be a list of {.fn ellmer::Turn}s, an {.code ellmer::Chat}, or a function, not {.obj_type_friendly {turns}}."
      )
    }
  }

  if (!is.null(system_prompt)) {
    if (inherits(system_prompt, "AsIs")) {
      client$set_system_prompt(system_prompt)
    } else {
      client$set_system_prompt(c(client$get_system_prompt(), system_prompt))
    }
  }

  agent_fn <- function(prompt) {
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

        # Evaluate the setup code if provided
        if (!is.null(setup_code) && nzchar(setup_code)) {
          eval(parse(text = setup_code))
        }

        tool_result <- function(x) {
          force(x)

          tokens <- the_client$get_tokens()

          tokens_input <- sum(tokens$tokens_total[tokens$role == "user"])
          tokens_output <- sum(tokens$tokens_total[tokens$role == "assistant"])

          asNamespace("btw")[["BtwAgentToolResult"]](
            value = if (!inherits(x, "error")) x,
            error = if (inherits(x, "error")) x,
            extra = list(
              turns = the_client$get_turns(),
              tokens = list(
                input = tokens_input,
                output = tokens_output,
                all = tokens
              ),
              cost = the_client$get_cost()
            )
          )
        }

        tryCatch(
          tool_result(the_client$chat(prompt)),
          error = tool_result
        )
      },
      the_client = the_client,
      prompt = prompt,
      setup_code = expr_text(setup_code),
      .compute = mirai_profile
    )

    promises::as.promise(m)
  }

  ellmer::tool(
    agent_fn,
    .description = description,
    prompt = ellmer::type_string(
      "The prompt to send to the agent. This should be a specific task or question."
    ),
    .name = name,
    .annotations = ellmer::tool_annotations(
      title = title %||% glue_("btw Agent ({{name}})")
    ),
    .convert = TRUE
  )
}

BtwAgentToolResult <- S7::new_class(
  "BtwAgentToolResult",
  parent = ellmer::ContentToolResult
)

S7::method(print, BtwAgentToolResult) <- function(x, ...) {
  title <- x@request@tool@annotations$title %||%
    glue_("Agent {{x@request@tool@name}}")
  prompt <- x@request@arguments$prompt

  input <- cli::col_green(paste0(x@extra$tokens$input, "\u2191"))
  output <- cli::col_red(paste0(x@extra$tokens$output, "\u2193"))

  cli::cli_rule(
    left = title,
    right = cli::format_inline(
      "tokens={.strong {input}}/{.strong {output}}"
    )
  )
  cli::cli_text(
    "{.strong Task:} {.emph {prompt}}"
  )
  if (!is.null(x@value)) {
    cli::cli_text("{.strong Response \u2500\u2500\u2500\u2500}")
    cli::cli_verbatim(x@value)
  } else if (!is.null(x@error)) {
    cli::cli_text(
      "{.strong Error:} {conditionMessage(x@error)}"
    )
  }
  cli::cli_text("\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500")

  invisible(x)
}
