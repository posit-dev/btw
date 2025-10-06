#' Task: Initialize Project Context File
#'
#' @description
#' Create a comprehensive context `btw.md` or `AGENTS.md` file for your project.
#' If launched in app or console mode, this task will start an interactive chat
#' session to guide you through the process of creating a context file.
#'
#' This task focuses on documenting project context for developers and agents.
#' See [btw_client()] for additional details about the format and usage of the
#' `btw.md` context file, including choosing the default LLM provider and model
#' or the default set of tools to use with [btw_client()].
#'
#' @examples
#' withr::with_envvar(list(ANTHROPIC_API_KEY = "example"), {
#'   btw_task_btw_init(mode = "tool", client = "anthropic")
#' })
#'
#' @param ... Additional context to provide to the AI. This can be any text or
#'   R objects that can be converted to text using [btw()].
#' @param path The path to the context file to create. Defaults to `btw.md`.
#' @param mode The mode to run the task in, which affects what is returned from
#'   this function. `"app"` and `"console"` modes launch interactive sessions,
#'   while `"client"` and `"tool"` modes return objects for programmatic use.
#' @inheritParams btw_client
#'
#' @return When `mode` is `"app"` or `"console"`, this function launches an
#'   interactive session in the browser or the R console, respectively. The
#'   ellmer chat object with the conversation history is returned invisibly
#'   when the session ends.
#'
#'   When `mode` is `"client"`, this function returns the configured ellmer
#'   chat client object. When `mode` is `"tool"`, this function returns an
#'   ellmer tool object that can be used in other chat instances.
#'
#' @family task and agent functions
#' @export
btw_task_btw_init <- function(
  ...,
  path = "btw.md",
  client = NULL,
  mode = c("app", "console", "client", "tool")
) {
  arg_match(mode)
  check_path_within_current_wd(path)
  path <- fs::path_rel(path)

  client <- btw_client(
    client = client,
    tools = c(
      "btw_tool_files_list_files",
      "btw_tool_files_read_text_file",
      "btw_tool_files_code_search",
      "btw_tool_files_write_text_file",
      "docs",
      "btw_tool_env_describe_data_frame"
    )
  )

  if (mode == "tool") {
    # Don't interpolate the prompt yet, we'll do that inside the tool
    sys_prompt <- paste(
      readLines(system.file("prompts/btw-init-tool.md", package = "btw")),
      collapse = "\n"
    )
  } else {
    sys_prompt <- btw_prompt("btw-init.md", path_summary_file = path)
  }

  dots <- dots_list(...)
  if (length(dots) > 0) {
    user_context <- btw(..., clipboard = FALSE)
    sys_prompt <- paste0(
      sys_prompt,
      "\n\n---\n\n",
      "# Additional context provided by the user\n\n",
      user_context
    )
  }

  sys_prompt <- paste0(
    sys_prompt,
    "\n\n---\n\n",
    "Begin Phase 1 by listing the root directory contents."
  )

  client$set_system_prompt(sys_prompt)

  if (mode == "client") {
    return(client)
  }

  if (mode == "tool") {
    path_default <- path

    tool <- ellmer::tool(
      function(prompt, path = NULL) {
        sys_prompt <- ellmer::interpolate(
          this_client$get_system_prompt(),
          path_summary_file = path %||% path_default
        )

        sys_prompt <- paste0(
          sys_prompt,
          "\n\n---\n\n",
          "YOU ARE NOW OPERATING IN TOOL MODE. ",
          "The user cannot respond directly to you. ",
          "Because you cannot talk to the user, you will need to make your own decisions using the information available to you and the best of your abilities. ",
          "You may do additional file exploration if needed."
        )

        this_client <- client$clone()
        this_client$set_system_prompt(sys_prompt)
        this_client$chat(prompt)
      },
      name = "btw_task_btw_init",
      description = "Create a comprehensive context file for your project.",
      arguments = list(
        prompt = ellmer::type_string(
          "Additional instructions to the AI. Leave empty to proceed automatically."
        ),
        path = ellmer::type_string(
          "The path to the context file to create. Defaults to `btw.md`.",
          required = FALSE
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Create Project Context File",
        icon = tool_icon("quick-reference")
      )
    )
    return(tool)
  }

  if (mode == "console") {
    cli::cli_text(
      "Starting {.strong btw_task_btw_init()} in live console mode."
    )
    cli::cli_text(
      "{cli::col_yellow(cli::symbol$play)} ",
      "Say \"{.strong {cli::col_magenta(\"Let's get started.\")}}\" to begin."
    )
    ellmer::live_console(client)
  } else {
    btw_app(
      client = client,
      tools = FALSE,
      messages = list(list(
        role = "assistant",
        content = paste(
          "\U1F44B Hi! I'm ready to help you create a comprehensive",
          sprintf("`./%s`", path),
          "context file for your project.",
          "Say <span class='suggestion submit'>Let's get started.</span> to begin."
        )
      ))
    )
  }
}
