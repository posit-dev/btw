#' Task: Create a Polished README
#'
#' @description
#' Create a compelling, user-focused README file for your project.
#' If launched in app or console mode, this task will start an interactive chat
#' session to guide you through the process of creating a polished README that
#' clearly communicates value and helps potential users make informed decisions.
#'
#' This task focuses on creating READMEs for END USERS, not developers, with
#' emphasis on clarity, accessibility, and authentic communication of value.
#' The process involves exploring your project files, understanding your target
#' audience and goals, proposing a structure, and then iteratively drafting
#' each section with your input.
#'
#' @examples
#' withr::with_envvar(list(ANTHROPIC_API_KEY = "example"), {
#'   btw_task_create_readme(mode = "tool", client = "anthropic")
#' })
#'
#' @param ... Additional context to provide to the AI. This can be any text or
#'   R objects that can be converted to text using [btw()].
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
btw_task_create_readme <- function(
  ...,
  client = NULL,
  mode = c("app", "console", "client", "tool")
) {
  mode <- arg_match(mode)

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

  sys_prompt <- btw_prompt("btw-create-readme.md")

  dots <- dots_list(...)
  if (length(dots) > 0) {
    user_context <- btw(..., clipboard = FALSE)
    if (nzchar(user_context)) {
      user_context <- paste0(
        "\n\n---\n\n",
        "# Additional context provided by the user\n\n",
        user_context
      )
    }
  } else {
    user_context <- ""
  }

  sys_prompt <- sub(
    "<!-- ADDITIONAL_USER_CONTEXT -->",
    user_context,
    sys_prompt,
    fixed = TRUE
  )

  client$set_system_prompt(sys_prompt)

  if (mode == "client") {
    return(client)
  }

  if (mode == "tool") {
    tool <- ellmer::tool(
      function(prompt = "") {
        this_client <- client$clone()

        sys_prompt <- paste0(
          this_client$get_system_prompt(),
          "\n\n---\n\n",
          "YOU ARE NOW OPERATING IN TOOL MODE. ",
          "The user cannot respond directly to you. ",
          "Because you cannot talk to the user, you will need to make your own decisions using the information available to you and the best of your abilities. ",
          "You may compensate by doing additional file exploration as needed."
        )

        this_client$set_system_prompt(sys_prompt)
        this_client$chat(prompt)
      },
      name = "btw_task_create_readme",
      description = "Create a polished, user-focused README file for your project.",
      arguments = list(
        prompt = ellmer::type_string(
          "Additional instructions to the AI. Leave empty to proceed automatically.",
          required = FALSE,
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Create Polished README",
        icon = tool_icon("post-add")
      )
    )
    return(tool)
  }

  if (mode == "console") {
    cli::cli_text(
      "Starting {.strong btw_task_create_readme()} in live console mode."
    )
    cli::cli_text(
      "{cli::col_yellow(cli::symbol$play)} ",
      "Say \"{.strong {cli::col_magenta(\"Let's get started.\")}}\" to begin."
    )
    ellmer::live_console(client)
  } else {
    btw_app_from_client(
      client = client,
      messages = list(list(
        role = "assistant",
        content = paste(
          "\U1F44B Hi! I'm ready to help you create a polished, user-focused",
          "README for your project.",
          "Say <span class='suggestion submit'>Let's get started.</span> to begin."
        )
      ))
    )
  }
}
