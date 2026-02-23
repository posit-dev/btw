#' Run a pre-formatted btw task
#'
#' @description
#' Runs a btw task defined in a file with YAML frontmatter configuration and
#' a markdown body containing the task prompt. The task file format is similar
#' to `btw.md` files, with client and tool configuration in the frontmatter and
#' the task instructions in the body.
#'
#' ## Task File Format
#'
#' Task files use the same format as `btw.md` files:
#'
#' ```yaml
#' ---
#' client:
#'   provider: anthropic
#'   model: claude-sonnet-4
#' tools: [docs, files]
#' ---
#'
#' Your task prompt here with {{ variable }} interpolation...
#' ```
#'
#' ## Template Variables
#'
#' The task prompt body supports template variable interpolation using
#' `{{ variable }}` syntax via [ellmer::interpolate()]. Pass named arguments
#' to provide values for template variables:
#'
#' ```r
#' btw_task("my-task.md", package_name = "dplyr", version = "1.1.0")
#' ```
#'
#' ## Additional Context
#'
#' Unnamed arguments are treated as additional context and converted to text
#' using [btw()]. This context is appended to the system prompt:
#'
#' ```r
#' btw_task("analyze.md", dataset_name = "mtcars", mtcars, my_function)
#' #                      ^-- template var        ^-- additional context
#' ```
#'
#' @param path Path to the task file containing YAML configuration and prompt.
#' @param ... Named arguments become template variables for interpolation in the
#'   task prompt. Unnamed arguments are treated as additional context objects
#'   and converted to text via [btw()].
#' @param client An [ellmer::Chat] client to override the task file's client
#'   configuration. If `NULL`, uses the client specified in the task file's
#'   YAML frontmatter, falling back to the default client resolution of
#'   [btw_client()].
#' @param mode The execution mode for the task:
#'   - `"app"`: Launch interactive Shiny app (default)
#'   - `"console"`: Interactive console chat with [ellmer::live_console()]
#'   - `"client"`: Return configured [ellmer::Chat] client without running
#'   - `"tool"`: Return an [ellmer::tool()] object for programmatic use
#'
#' @return Depending on `mode`:
#'   - `"app"`: Returns the chat client invisibly after launching the app
#'   - `"console"`: Returns the chat client after console interaction
#'   - `"client"`: Returns the configured chat client
#'   - `"tool"`: Returns an [ellmer::tool()] object
#'
#' @examples
#' # Create a simple task file
#' tmp_task_file <- tempfile(fileext = ".md")
#'
#' cat(file = tmp_task_file, '---
#' client: anthropic/claude-sonnet-4-6
#' tools: [docs, files]
#' ---
#'
#' Analyze the {{ package_name }} package and create a summary.
#' ')
#'
#' # Task with template interpolation
#' btw_task(tmp_task_file, package_name = "dplyr", mode = "tool")
#'
#' # Include additional context
#' btw_task(
#'   tmp_task_file,
#'   package_name = "ggplot2",
#'   mtcars,  # Additional context
#'   mode = "tool"
#' )
#'
#' @family task and agent functions
#' @export
btw_task <- function(
  path,
  ...,
  client = NULL,
  mode = c("app", "console", "client", "tool")
) {
  check_string(path)
  mode <- arg_match(mode)

  # Read and parse the task file using existing utility
  if (!fs::file_exists(path)) {
    cli::cli_abort("Task file not found: {.path {path}}")
  }

  # Use the existing read_single_btw_file function
  task_config <- read_single_btw_file(path)

  # Extract the task prompt from btw_system_prompt field
  task_prompt <- task_config$btw_system_prompt

  if (is.null(task_prompt) || !nzchar(task_prompt)) {
    cli::cli_abort(
      "Task file must contain a prompt in the body: {.path {path}}"
    )
  }

  # Remove btw_system_prompt from config to avoid confusion with other fields
  task_config$btw_system_prompt <- NULL

  # Separate named (template vars) and unnamed (context) arguments
  dots <- dots_list(...)
  dot_names <- names2(dots)

  # Named args are template variables
  template_vars <- dots[nzchar(dot_names)]

  # Unnamed args are additional context
  context_objects <- dots[!nzchar(dot_names)]

  # Interpolate template variables in the task prompt
  if (length(template_vars) > 0) {
    task_prompt <- ellmer::interpolate(task_prompt, !!!template_vars)
  }

  # Create the client with configuration from task file
  # Use provided client, or task file config, or btw defaults
  if (is.null(client)) {
    # Build client from task config
    if (!is.null(task_config$client)) {
      client <- task_config$client
    }
  }

  # Get tools from task config
  tools <- task_config$tools %||% btw_tools()

  # Create btw client with resolved configuration
  chat_client <- btw_client(
    client = client,
    tools = tools
  )

  # Build system prompt
  base_system_prompt <- chat_client$get_system_prompt()

  # Add task prompt to system
  sys_prompt_parts <- c(
    base_system_prompt,
    "---",
    "# Task Instructions",
    "",
    task_prompt
  )

  # Add additional context if provided
  if (length(context_objects) > 0) {
    user_context <- do.call(btw, c(context_objects, list(clipboard = FALSE)))
    if (nzchar(user_context)) {
      sys_prompt_parts <- c(
        sys_prompt_parts,
        "",
        "---",
        "# Additional Context",
        "",
        user_context
      )
    }
  }

  final_system_prompt <- paste(sys_prompt_parts, collapse = "\n")
  chat_client$set_system_prompt(final_system_prompt)

  # Mode dispatch
  if (mode == "client") {
    return(chat_client)
  }

  if (mode == "tool") {
    # Create a tool wrapper function (simplified without ... to avoid ellmer argument mismatch)
    task_tool_fn <- function(prompt = "") {
      # Clone to avoid state pollution
      this_client <- chat_client$clone()

      # Append tool mode instructions
      sys_prompt <- paste0(
        this_client$get_system_prompt(),
        "\n\n---\n\n",
        "YOU ARE NOW OPERATING IN TOOL MODE. ",
        "The user cannot respond directly to you. ",
        "Because you cannot talk to the user, you will need to make ",
        "your own decisions using the information available to you ",
        "and the best of your abilities. ",
        "You may do additional exploration if needed."
      )

      this_client$set_system_prompt(sys_prompt)

      # Add any additional prompt instructions
      if (nzchar(prompt)) {
        initial_message <- paste0(
          "Additional instructions: ",
          prompt
        )
      } else {
        initial_message <- "Please complete the task as instructed."
      }

      this_client$chat(initial_message)
    }

    # Create the tool definition
    tool <- ellmer::tool(
      task_tool_fn,
      name = paste0("btw_task_", fs::path_ext_remove(basename(path))),
      description = paste0(
        "Run the task defined in ",
        basename(path),
        ". ",
        substr(task_prompt, 1, 100),
        if (nchar(task_prompt) > 100) "..."
      ),
      arguments = list(
        prompt = ellmer::type_string(
          "Additional instructions for the task. Leave empty to proceed with default instructions.",
          required = FALSE
        )
      )
    )

    return(tool)
  }

  # Console or app mode
  if (mode == "console") {
    task_name <- fs::path_ext_remove(basename(path))
    cli::cli_text(
      "Starting {.strong btw_task()} for {.file {task_name}} in live console mode."
    )
    cli::cli_text(
      "{cli::col_yellow(cli::symbol$play)} ",
      "Say \"{.strong {cli::col_magenta('Let\\'s get started.')}}\" to begin the task."
    )
    ellmer::live_console(chat_client)
  } else {
    # App mode
    task_name <- fs::path_ext_remove(basename(path))
    btw_app_from_client(
      client = chat_client,
      messages = list(list(
        role = "assistant",
        content = paste0(
          "\U1F4CB Hi! I'm ready to help with the <strong>",
          htmltools::htmlEscape(task_name),
          "</strong> task.<br><br>",
          "Say <span class='suggestion submit'>Let's get started.</span> to begin."
        )
      ))
    )
  }
}
