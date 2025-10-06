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
#' @section Context File Format:
#'
#' Files named `btw.md` include YAML front matter for configuring the default
#' chat client and tools. Files named `AGENTS.md` follow the
#' [AGENTS.md](https://agents.md/) convention and do not include YAML
#' front matter by default (although you are welcome to add it if you want to).
#'
#' See [btw_client()] for more information about the format and available
#' options. See [btw_tools()] for a list of available tools.
#'
#' @section Interactive Setup:
#'
#' For an interactive guided setup, consider using `btw_task_btw_init()` to use
#' an LLM to help you create a `btw.md` file for your project.
#'
#' @examples
#' \dontrun{
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
#' }
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
      "i" = "Call {.fn btw::btw_task_btw_init} to use an LLM to help you initialize the project context."
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
    file.edit(path)
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
  # Check if we're in an R package
  project_root <- path_find_in_project("DESCRIPTION")

  if (is.null(project_root)) {
    return(invisible())
  }

  if (!is_installed("usethis")) {
    cli::cli_inform(c(
      "i" = "Add {.file {path_display(path)}} to {.file .Rbuildignore} manually (or install {.pkg usethis} and try again)"
    ))
    return(invisible())
  }

  # Get the relative path from project root
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
