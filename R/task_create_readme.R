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

  readme_badge_tool <- NULL
  if (detect_project_is_r_package()) {
    readme_badge_tool <- tool_readme_add_badge()
  }

  client <- btw_client(
    client = client,
    tools = list(
      "btw_tool_files_list_files",
      "btw_tool_files_read_text_file",
      "btw_tool_files_code_search",
      "btw_tool_files_write_text_file",
      "docs",
      "btw_tool_env_describe_data_frame",
      readme_badge_tool
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

#' Generate README Badge Markup
#'
#' A tool that wraps usethis badge functions to generate markdown badge markup
#' for README files. The LLM calls the appropriate usethis function by
#' specifying the function name and its arguments as JSON.
#'
#' @examples
#' chat <- ellmer::chat_openai()
#' chat$register_tool(tool_readme_add_badge())
#' chat$chat("Generate a CRAN badge")
#' chat$chat("Generate a lifecycle badge for a stable package")
#' chat$chat("Generate a GitHub Actions badge for R-CMD-check.yaml")
#'
#' @return An `ellmer::tool()` object if the `usethis` package is installed.
#' @noRd
tool_readme_add_badge <- function() {
  if (!is_installed("usethis")) {
    cli::cli_inform(
      "Install the {.pkg usethis} package to use the readme badge tool."
    )
    return(invisible())
  }

  ellmer::tool(
    function(usethis_function, args = "{}") {
      valid_functions <- c(
        "use_cran_badge",
        "use_bioc_badge",
        "use_lifecycle_badge",
        "use_binder_badge",
        "use_r_universe_badge",
        "use_posit_cloud_badge",
        "use_github_actions_badge",
        "use_badge"
      )

      usethis_function <- arg_match(usethis_function, valid_functions)

      # Parse the JSON arguments
      args_list <- jsonlite::fromJSON(
        args,
        simplifyVector = TRUE,
        simplifyDataFrame = FALSE,
        simplifyMatrix = FALSE
      )

      # Suppress usethis clipboard operations
      withr::local_options(usethis.clipboard = FALSE)

      # Get the function from usethis namespace
      fn <- asNamespace("usethis")[[usethis_function]]

      # Call the function with the provided arguments
      result <- capture.output(do.call(fn, args_list), type = "message")
      result <- paste(result, collapse = "\n")
      cli::ansi_strip(result)
    },
    name = "readme_add_badge",
    description = "Generate markdown badge markup by calling usethis badge functions.

Use this tool after you have created a README.md or README.Rmd file, which should already contain `<!-- badges: start -->` and `<!-- badges: end -->` markers where you want the badges to appear.
If the README file does not contain these markers, the output from this tool will show you the badge markdown, but you will need to manually add it to the README file.

Available functions:
- use_cran_badge(): CRAN version badge (no args)
- use_bioc_badge(): Bioconductor build status (no args)
- use_lifecycle_badge(stage): Package lifecycle. stage = 'experimental'|'stable'|'superseded'|'deprecated'
- use_binder_badge(ref, urlpath): Binder environment. ref = git ref, urlpath = optional UI path
- use_r_universe_badge(repo_spec): R-universe version. repo_spec = 'owner/repo' (optional)
- use_posit_cloud_badge(url): Posit Cloud project. url = project link (required)
- use_github_actions_badge(name, repo_spec): GitHub Actions workflow. name = workflow file, repo_spec = 'owner/repo' (optional)
- use_badge(badge_name, href, src): Custom badge. All args required

This tool returns the output from the usethis function.
The output will indicate whether or not usethis was able to automatically update the README file.",
    arguments = list(
      usethis_function = ellmer::type_string(
        "The usethis badge function name (e.g., 'use_cran_badge')"
      ),
      args = ellmer::type_string(
        "JSON string of function arguments as key-value pairs. Use {} for functions with no arguments. Example: {\"stage\": \"stable\"} or {\"url\": \"https://posit.cloud/project/123456\"}"
      )
    ),
    annotations = ellmer::tool_annotations(
      title = "Add README Badge",
      icon = tool_icon("new-label"),
      read_only_hint = FALSE,
      idempotent_hint = TRUE,
      destructive_hint = FALSE
    )
  )
}
