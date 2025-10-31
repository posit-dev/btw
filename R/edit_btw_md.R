#' Create or edit a btw.md context file
#'
#' @description
#' Create or edit a `btw.md` or `AGENTS.md` context file for your project or
#' user-level configuration. These functions help you set up the context files
#' that [btw_client()] and [btw_app()] use to configure chat clients.
#'
#' `use_btw_md()` creates a new context file with a default template. If the
#' file already exists, it will not overwrite it, but will still ensure the file
#' is added to `.Rbuildignore` if you're in an R package.
#'
#' `edit_btw_md()` opens an existing context file for editing. Without
#' arguments, it opens the same file that [btw_client()] would use by default.
#'
#' @section Additional Examples:
#'
#' ```r
#' # Create a project-level btw.md
#' use_btw_md()
#'
#' # Create a user-level btw.md
#' use_btw_md("user")
#'
#' # Create an AGENTS.md file
#' use_btw_md("AGENTS.md")
#'
#' # Edit the context file that btw_client() would use
#' edit_btw_md()
#'
#' # Edit a specific context file
#' edit_btw_md("user")
#' ```
#'
#' @section Project Context:
#'
#' You can use a `btw.md` or [`AGENTS.md`](https://agents.md) file to keep track
#' of project-specific rules, guidance and context in your project. Either file
#' name will work, so we'll refer primarily to `btw.md`. These files are used
#' automatically by [btw_client()] and [btw_app()]: they look first for `btw.md`
#' and then for `AGENTS.md`. If both files are present, only the `btw.md` file
#' will be used.
#'
#' Any time you start a chat client with `btw_client()` or launch a chat session
#' with `btw_app()`, btw will automatically find and include the contents of the
#' `btw.md` or `AGENTS.md` file in the system prompt of your chat. This helps
#' maintain context and consistency across chat sessions.
#'
#' Use `btw.md` to inform the LLM of your preferred code style, to provide
#' domain-specific terminology or definitions, to establish project
#' documentation, goals and constraints, to include reference materials such or
#' technical specifications, or more. Storing this kind of information in
#' `btw.md` may help you avoid repeating yourself and can be used to maintain
#' coherence across many chat sessions.
#'
#' Write in markdown and structure the file in any way you wish, or use
#' [btw_task_create_btw_md()] to help you create a project context file for an
#' existing project with the help of an AI agent.
#'
#' @section Chat Settings:
#'
#' You can also use the `btw.md` file to choose default chat settings for your
#' project in a YAML front matter block at the top of the file. In this YAML
#' block you can choose settings for the default ellmer chat `client`, e.g.
#' `provider`, `model`, as well as choose which [btw_tools()] to use in
#' `btw_client()` or `btw_app()`.
#'
#' **Chat client settings**
#'
#' Use the `client` field to set options for the chat client. This can be a
#' single string in `provider` or `provider/model` format -- as used by
#' [ellmer::chat()] -- or a list of client options with `provider` and
#' `model` fields, as well as any other options supported by the underlying
#' `ellmer::chat_*()` function you choose. Note that `provider` maps to the
#' `ellmer::chat_*()` function, while `model` maps to the `model` argument of
#' that function.
#'
#' * Using ellmer's default model for a provider:
#'
#'   ```md
#'   ---
#'   client: openai
#'   ---
#'   ```
#'
#'   ```md
#'   ---
#'   client:
#'     provider: openai
#'   ---
#'   ```
#'
#' * Using a specific model:
#'
#'   ```md
#'   ---
#'   client: anthropic/claude-4-5-sonnet-latest
#'   ---
#'   ```
#'
#'   ```md
#'   ---
#'   client:
#'     provider: anthropic
#'     model: claude-4-5-sonnet-latest
#'   ---
#'   ```
#'
#' * Using additional client options:
#'
#'   ```md
#'   ---
#'   client:
#'     provider: ollama
#'     model: "gpt-oss:20b"
#'     echo: output
#'     base_url: "http://my-company.example.com:11434"
#'   ---
#'   ```
#'
#' **Tool Settings**
#'
#' The top-level `tools` field is used to specify which btw tools
#' are included in the chat. This should be a list of tool groups or tool names
#' (with or without the `btw_tool_` prefix). See [btw_tools()] for a list of
#' available tools and tool groups.
#'
#' Here's an example `btw.md` file:
#'
#' ````
#' ---
#' client: claude/claude-4-5-sonnet-latest
#' tools: [docs, env, files, git, ide, search, session, web]
#' ---
#'
#' Follow these important style rules when writing R code:
#'
#' * Prefer solutions that use {tidyverse}
#' * Always use `<-` for assignment
#' * Always use the native base-R pipe `|>` for piped expressions
#' ````
#'
#' @section Selective Context:
#'
#' One use-case for `btw.md` is to provide stable context for an on-going task
#' that might span multiple chat sessions. In this case, you can use `btw.md` to
#' hold the complete project plan, with background information, requirements,
#' and specific tasks to be completed. This can help maintain continuity across
#' chat sessions, especially if you update the `btw.md` file as the project
#' progresses.
#'
#' In this use case, however, you might want to hide parts of the project plan
#' from the system prompt, for example to hide completed or future tasks when
#' their description would distract the LLM from the current task.
#'
#' You can hide parts of the `btw.md` file from the system prompt by wrapping
#' them in HTML `<!-- HIDE -->` and `<!-- /HIDE -->` comment tags. A single
#' `<!-- HIDE -->` comment tag will hide all content after it until the next
#' `<!-- /HIDE -->` tag, or the end of the file. This is particularly useful
#' when your system prompt contains notes to yourself or future tasks that you
#' do not want to be included in the system prompt.
#'
#' @section Project or User Scope:
#'
#' For project-specific configuration, store your `btw.md` file in the root of
#' your project directory. You can even have multiple `btw.md` files in your
#' project, in which case the one closest to your current working directory
#' will be used. This makes it easy to have different `btw.md` files for
#' different sub-projects or sub-directories within a larger project.
#'
#' For global configuration, you can maintain a `btw.md` file in your home
#' directory (at `btw.md` or `.config/btw/btw.md` in your home directory, using
#' `fs::path_home()`). This file will be used by default when a project-specific
#' `btw.md` file is not found. Note that \pkg{btw} only looks for `btw.md` in
#' your home directory if no project-specific `btw.md` or `AGENTS.md` file is
#' present. It also does not look for `AGENTS.md` in your home directory.
#'
#' @section Interactive Setup:
#'
#' For an interactive guided setup, consider using [btw_task_create_btw_md()] to use
#' an LLM to help you create a `btw.md` file for your project.
#'
#' @examples
#' # See additional examples in the sections above
#'
#' withr::with_tempdir({
#'   withr::with_tempfile("btw_md_tmp", fileext = ".md", {
#'     use_btw_md(btw_md_tmp)
#'   })
#' })
#'
#' @param scope The scope of the context file. Can be:
#'   - `"project"` (default): Creates/opens `btw.md` (by default) or `AGENTS.md`
#'     in the project root
#'   - `"user"`: Creates/opens `btw.md` in your home directory
#'   - A directory path: Creates/opens `btw.md` in that directory
#'   - A file path: Creates/opens that specific file
#'
#'   For `edit_btw_md()`, `scope = NULL` (default) will find and open the
#'   context file that [btw_client()] would use, searching first for `btw.md`
#'   and then `AGENTS.md` in the project directory and then for `btw.md` in your
#'   home directory.
#'
#' @return `use_btw_md()` returns the path to the context file, invisibly.
#'   `edit_btw_md()` is called for its side effect of opening the file.
#'
#' @seealso Project context files are discovered automatically and included in
#'   the system prompt by [btw_client()]. See [btw_tools()] for a list of
#'   available tools.
#'
#' @describeIn use_btw_md Create a new `btw.md` or `AGENTS.md` context file in
#'   the current directory, the project directory or your home directory.
#' @export
use_btw_md <- function(scope = "project") {
  check_string(scope)

  path <- resolve_btw_md_path(scope, for_creation = TRUE)

  # Check if file already exists
  file_exists <- fs::file_exists(path)

  if (!file_exists) {
    # Create the file from template
    template_path <- btw_md_template(path)
    fs::file_copy(template_path, path)
    cli::cli_inform(c(
      "v" = "Created {.file {path_display(path)}}"
    ))
  } else {
    cli::cli_inform(c("v" = "{.file {path_display(path)}} already exists"))
  }

  if (scope != "user") {
    # Always handle .Rbuildignore (even if file exists)
    use_build_ignore_btw_md(path)
  }

  if (!file_exists) {
    cli::cli_inform(c(
      "i" = "See {.help btw::btw_client} for format details",
      "i" = "See {.help btw::btw_tools} for available tools",
      "i" = "Call {.fn btw::btw_task_create_btw_md} to use an LLM to help you initialize the project context."
    ))
  } else {
    cmd <- sprintf('btw::edit_btw_md("%s")', path_display(path))
    if (scope == "user") {
      cmd <- 'btw::edit_btw_md("user")'
    }
    cli::cli_inform(c("i" = "Call {.run {cmd}} to edit it"))
  }

  invisible(path)
}

#' @describeIn use_btw_md Open an existing `btw.md` or `AGENTS.md` context file
#'   for editing.
#' @export
edit_btw_md <- function(scope = NULL) {
  path <- resolve_btw_md_path(scope, for_creation = FALSE)

  if (!fs::file_exists(path)) {
    cli::cli_abort(c(
      "x" = "{.file {fs::path_rel(path)}} does not exist",
      "i" = "Call {.fn use_btw_md('{scope}')} to create it"
    ))
  }

  # Try to use rstudioapi if available, otherwise use file.edit
  if (rlang::is_installed("rstudioapi") && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(path)
  } else {
    utils::file.edit(path)
  }

  cli::cli_inform(c("v" = "Opening {.file {path_display(path)}}"))
  invisible(path)
}

# Helpers -----------------------------------------------------------------

resolve_btw_md_path <- function(scope, for_creation = FALSE) {
  # Handle NULL scope - find like btw_client() does
  if (is.null(scope)) {
    if (for_creation) {
      cli::cli_abort(
        "{.arg scope} must be specified when creating a file"
      )
    }

    path <- find_btw_context_file(NULL)

    if (is.null(path)) {
      cli::cli_abort(c(
        "x" = "Could not find {.file btw.md} or {.file AGENTS.md}",
        "i" = "Call {.fn use_btw_md} to create one"
      ))
    }

    return(path)
  }

  # Handle named scopes
  if (identical(scope, "project")) {
    project_root <-
      path_find_in_project("DESCRIPTION") %||%
      path_find_in_project(".git") %||%
      getwd()
    project_root <- fs::path_dir(project_root)
    path_btw <- fs::path(project_root, "btw.md")
    if (fs::file_exists(path_btw)) {
      return(path_btw)
    }
    path_agents <- fs::path(project_root, "AGENTS.md")
    if (fs::file_exists(path_agents)) {
      return(path_agents)
    }
    return(path_btw)
  }

  if (identical(scope, "user")) {
    return(fs::path_home("btw.md"))
  }

  scope <- fs::path_expand(scope)

  # If it's a directory, append btw.md
  if (fs::dir_exists(scope)) {
    return(fs::path(scope, "btw.md"))
  }

  # Otherwise, assume it's a file path
  scope
}

btw_md_template <- function(path) {
  filename <- basename(path)
  name_without_ext <- tools::file_path_sans_ext(filename)

  template_name <- if (name_without_ext == "AGENTS") {
    "AGENTS.md"
  } else {
    "btw.md"
  }

  template_path <- system.file("templates", template_name, package = "btw")

  if (!nzchar(template_path)) {
    cli::cli_abort("Could not find template {.file {template_name}}")
  }

  template_path
}

use_build_ignore_btw_md <- function(path) {
  if (!detect_project_is_r_package()) {
    # Only if we're in an R package
    return(invisible())
  }

  if (!is_installed("usethis")) {
    cli::cli_inform(c(
      "i" = "Add {.file {path_display(path)}} to {.file .Rbuildignore} manually (or install {.pkg usethis} and try again)"
    ))
    return(invisible())
  }

  # Get the relative path from project root
  project_root <- path_find_in_project("DESCRIPTION")
  project_dir <- fs::path_dir(project_root)
  rel_path <- fs::path_rel(path, start = project_dir)

  # Use usethis to add to .Rbuildignore
  tryCatch(
    usethis::use_build_ignore(rel_path),
    error = function(e) {
      cli::cli_warn(c(
        "!" = "Could not add {.file {rel_path}} to {.file .Rbuildignore}",
        "i" = "You may need to add it manually"
      ))
    }
  )

  invisible()
}

path_display <- function(path, parent = getwd()) {
  path_og <- path
  path <- fs::path_real(path)
  parent <- fs::path_real(parent)
  home <- fs::path_real(fs::path_home())

  if (fs::path_has_parent(path, parent)) {
    fs::path_rel(path, parent)
  } else if (fs::path_has_parent(path, home)) {
    fs::path(sub(home, "~", path))
  } else {
    path_og
  }
}
