#' Task: Create a Skill
#'
#' @description
#' Create a new skill for your project using interactive guidance. If launched
#' in app or console mode, this task will start an interactive chat session to
#' guide you through the process of creating a skill that extends Claude's
#' capabilities with specialized knowledge, workflows, or tool integrations.
#'
#' @examples
#' withr::with_envvar(list(ANTHROPIC_API_KEY = "example"), {
#'   btw_task_create_skill(mode = "tool", client = "anthropic")
#' })
#'
#' @param name Optional skill name. If provided, the AI will skip the naming
#'   step and use this name directly.
#' @param tools Optional list or character vector of tools to allow the task to
#'   use when creating the skill. By default documentation tools are included to
#'   allow the task to help create package-based skills. You can include
#'   additional tools as needed.
#'
#'   Because the task requires file tools to create skills with resources, tools
#'   for listing, reading and writing files are always included.
#' @inheritParams btw_task_create_readme
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
btw_task_create_skill <- function(
  ...,
  name = NULL,
  client = NULL,
  mode = c("app", "console", "client", "tool"),
  tools = "docs"
) {
  mode <- arg_match(mode)

  client <- btw_client(
    client = client,
    tools = list2(
      !!!tools,
      "btw_tool_files_list",
      "btw_tool_files_read",
      "btw_tool_files_write",
      "btw_tool_files_replace"
    )
  )

  skill_path <- fs::path_package("btw", "skills", "skill-creator")
  skill_info <- validate_skill(skill_path)

  fm <- frontmatter::read_front_matter(fs::path(skill_path, "SKILL.md"))
  skill_text <- fm$body %||% ""

  resources <- list_skill_resources(skill_path)
  resources_listing <- format_resources_listing(resources, skill_path)

  sys_prompt <- paste0(skill_text, resources_listing)

  dots <- dots_list(...)
  if (length(dots) > 0) {
    user_context <- btw(..., clipboard = FALSE)
    if (nzchar(user_context)) {
      sys_prompt <- paste0(
        sys_prompt,
        "\n\n---\n\n",
        "# Additional context provided by the user\n\n",
        user_context
      )
    }
  }

  extra_instructions <- r"---(
## Additional Instructions

If you don't have access to a bash tool, do not attempt to run the bundled Python scripts. Instead, read the script contents and accomplish the same tasks using the file tools available to you.

Do not attempt to package the skill (step 5). The user will handle distribution.

You can only create skills within the current project directory (e.g. `.btw/skills/` or `.agents/skills/`). If the user wants a user-level skill, create it in the project first and advise them to move it to their user skills directory (e.g. `~/.config/btw/skills/` or similar).
)---"

  if (!is.null(name)) {
    check_string(name)
    extra_instructions <- c(
      extra_instructions,
      "",
      sprintf(
        "The user has already chosen the skill name: %s. Skip the naming step.",
        name
      )
    )
  }

  sys_prompt <- paste0(
    sys_prompt,
    "\n\n---\n\n",
    paste(extra_instructions, collapse = "\n")
  )

  client$set_system_prompt(sys_prompt)

  if (mode == "client") {
    return(client)
  }

  if (mode == "tool") {
    btw_task_create_skill_tool <- function(prompt = "", name = NULL) {
      this_client <- client$clone()

      tool_prompt <- this_client$get_system_prompt()

      if (!is.null(name)) {
        tool_prompt <- paste0(
          tool_prompt,
          "\n\n",
          sprintf(
            "The user has already chosen the skill name: %s. Skip the naming step.",
            name
          )
        )
      }

      tool_prompt <- paste0(
        tool_prompt,
        "\n\n---\n\n",
        "YOU ARE NOW OPERATING IN TOOL MODE. ",
        "The user cannot respond directly to you. ",
        "Because you cannot talk to the user, you will need to make your own decisions using the information available to you and the best of your abilities. ",
        "You may compensate by doing additional file exploration as needed."
      )

      this_client$set_system_prompt(tool_prompt)
      this_client$chat(prompt)
    }

    tool <- ellmer::tool(
      function(prompt, name = NULL) btw_task_create_skill_tool(prompt, name),
      name = "btw_task_create_skill",
      description = "Create a new skill for your project with interactive guidance.",
      arguments = list(
        prompt = ellmer::type_string(
          "Additional instructions to the AI. Leave empty to proceed automatically.",
          required = FALSE
        ),
        name = ellmer::type_string(
          "Optional skill name. If provided, the AI will skip the naming step.",
          required = FALSE
        )
      ),
      annotations = ellmer::tool_annotations(
        title = "Create Skill",
        icon = tool_icon("post-add")
      )
    )
    return(tool)
  }

  if (mode == "console") {
    cli::cli_text(
      "Starting {.strong btw_task_create_skill()} in live console mode."
    )
    cli::cli_text(
      "{cli::col_yellow(cli::symbol$play)} ",
      "Say {.strong {cli::col_magenta(\"Let's get started.\")}} to begin."
    )
    ellmer::live_console(client)
  } else {
    btw_app_from_client(
      client = client,
      messages = list(list(
        role = "assistant",
        content = paste(
          "\U1F44B Hi! I'm ready to help you create a new skill for your project.",
          "Say <span class='suggestion submit'>Let's get started.</span> to begin."
        )
      ))
    )
  }
}
