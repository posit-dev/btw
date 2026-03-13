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

  if (!fs::file_exists(path)) {
    cli::cli_abort("Task file not found: {.path {path}}")
  }

  task_config <- read_single_btw_file(path)
  task_prompt <- task_config$btw_system_prompt
  task_config$btw_system_prompt <- NULL

  if (is.null(task_prompt) || !nzchar(task_prompt)) {
    cli::cli_abort(
      "Task file must contain a prompt in the body: {.path {path}}"
    )
  }

  # Capture dots as quosures to preserve expressions (needed for btw() naming)
  all_quos <- enquos(...)
  quo_names <- names2(all_quos)

  # Named quos → template variables (evaluated); unnamed quos → context objects
  template_vars <- lapply(all_quos[nzchar(quo_names)], eval_tidy)
  context_quos <- all_quos[!nzchar(quo_names)]

  # Interpolate template variables in the task prompt
  if (length(template_vars) > 0) {
    task_prompt <- ellmer::interpolate(task_prompt, !!!template_vars)
  }

  # Build final system prompt: task body + optional additional context
  final_system_prompt <- task_prompt

  if (length(context_quos) > 0) {
    context_values <- lapply(context_quos, eval_tidy)
    context_names <- vapply(context_quos, as_label, character(1))
    names(context_values) <- context_names
    context_env <- new_environment(context_values, parent = parent.frame())
    user_context_text <- trimws(paste(
      btw_this(context_env, items = context_names),
      collapse = "\n"
    ))
    if (nzchar(user_context_text)) {
      final_system_prompt <- paste(
        c(
          final_system_prompt,
          "",
          "---",
          "# Additional Context",
          "",
          user_context_text
        ),
        collapse = "\n"
      )
    }
  }

  # Resolve client: explicit arg > task file YAML > btw defaults
  if (is.null(client) && !is.null(task_config$client)) {
    client <- task_config$client
  }

  tools <- task_config$tools %||% btw_tools()

  # Call btw_client() to register tools, then replace the system prompt
  # with the task-specific content (matches btw_task_create_* pattern)
  chat_client <- btw_client(
    client = client,
    tools = tools
  )
  chat_client$set_system_prompt(final_system_prompt)

  # Derive identity fields from task config or file basename
  tool_name_raw <- task_config$name %||% fs::path_ext_remove(basename(path))
  tool_name_normalized <- validate_task_tool_name(tool_name_raw, path)
  display_name <- task_config$title %||%
    to_title_case(gsub("[_-]", " ", tool_name_raw))

  if (mode == "client") {
    return(chat_client)
  }

  if (mode == "tool") {
    task_tool_fn <- function(prompt = "") {
      this_client <- chat_client$clone()

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

      if (nzchar(prompt)) {
        this_client$chat(paste0("Additional instructions: ", prompt))
      } else {
        this_client$chat("Please complete the task as instructed.")
      }
    }

    description <- task_config$description %||%
      {
        lines <- strsplit(task_prompt, "\n")[[1]]
        first_nonempty <- trimws(lines[nzchar(trimws(lines))])
        if (length(first_nonempty) > 0) {
          gsub("^#+\\s*", "", first_nonempty[1])
        } else {
          paste0("Run the task defined in ", basename(path))
        }
      }

    tool <- ellmer::tool(
      task_tool_fn,
      name = paste0("btw_task_", tool_name_normalized),
      description = description,
      annotations = ellmer::tool_annotations(
        title = display_name,
        read_only_hint = FALSE,
        open_world_hint = TRUE
      ),
      arguments = list(
        prompt = ellmer::type_string(
          "Additional instructions for the task. Leave empty to proceed with default instructions.",
          required = FALSE
        )
      )
    )

    if (!is.null(task_config$icon)) {
      icon <- custom_icon(task_config$icon)
      if (is.null(icon)) {
        cli::cli_warn(c(
          "Invalid icon in task file {.path {path}}",
          "i" = "Ignoring {.field icon}: {.val {task_config$icon}}"
        ))
      }
      tool@annotations$icon <- icon
    }

    return(tool)
  }

  if (mode == "console") {
    cli::cli_text(
      "Starting {.strong btw_task()} for {.file {display_name}} in live console mode."
    )
    cli::cli_text(
      "{cli::col_yellow(cli::symbol$play)} ",
      "Say \"{.strong {cli::col_magenta('Let\\'s get started.')}}\" to begin."
    )
    ellmer::live_console(chat_client)
  } else {
    btw_app_from_client(
      client = chat_client,
      messages = list(list(
        role = "assistant",
        content = paste0(
          "\U1F4CB Hi! I'm ready to help with the <strong>",
          htmltools::htmlEscape(display_name),
          "</strong> task.<br><br>",
          "Say <span class='suggestion submit'>Let's get started.</span> to begin."
        )
      ))
    )
  }
}

validate_task_tool_name <- function(name, path) {
  check_string(name)

  name <- trimws(name)
  if (!nzchar(name)) {
    cli::cli_abort(c(
      "Task name cannot be empty: {.path {path}}",
      "i" = "Provide a non-empty {.field name} in YAML frontmatter or use a file name with letters."
    ))
  }

  if (!grepl("^[a-zA-Z0-9_-]+$", name)) {
    cli::cli_abort(c(
      "Invalid task name {.val {name}} in {.path {path}}",
      "i" = "{.field name} must contain only letters, numbers, - and _."
    ))
  }

  name
}
