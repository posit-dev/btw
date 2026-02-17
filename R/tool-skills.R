#' @include tool-result.R
NULL

#' Tool: Fetch a skill
#'
#' @description
#' Fetch a skill's instructions and list its bundled resources.
#'
#' Skills are modular capabilities that extend Claude's functionality with
#' specialized knowledge, workflows, and tools. Each skill is a directory
#' containing a `SKILL.md` file with instructions and optional bundled
#' resources (scripts, references, assets).
#'
#' @param skill_name The name of the skill to fetch.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return A `btw_tool_result` containing the skill instructions and a listing
#'   of bundled resources with their paths.
#'
#' @family skills
#' @export
btw_tool_fetch_skill <- function(skill_name, `_intent`) {}

btw_tool_fetch_skill_impl <- function(skill_name) {
  check_string(skill_name)

  skill_info <- find_skill(skill_name)

  if (is.null(skill_info)) {
    available <- btw_list_skills()
    skill_names <- vapply(available, function(x) x$name, character(1))
    cli::cli_abort(
      c(
        "Skill {.val {skill_name}} not found.",
        "i" = "Available skills: {.val {skill_names}}"
      )
    )
  }

  fm <- frontmatter::read_front_matter(skill_info$path)
  skill_text <- fm$body %||% ""

  resources <- list_skill_resources(skill_info$base_dir)
  resources_listing <- format_resources_listing(resources, skill_info$base_dir)

  full_content <- paste0(skill_text, resources_listing)

  btw_tool_result(
    value = full_content,
    data = list(
      name = skill_name,
      path = skill_info$path,
      base_dir = skill_info$base_dir,
      metadata = fm$data,
      resources = resources
    ),
    display = list(
      title = sprintf("Skill: %s", skill_name),
      markdown = full_content
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_fetch_skill",
  group = "skills",
  tool = function() {
    ellmer::tool(
      btw_tool_fetch_skill_impl,
      name = "btw_tool_fetch_skill",
      description = paste(
        "Fetch a skill's instructions and list its bundled resources.",
        "Skills provide specialized guidance for specific tasks.",
        "After fetching, use file read tools to access references,",
        "or bash/code tools to run scripts."
      ),
      annotations = ellmer::tool_annotations(
        title = "Fetch Skill",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        btw_can_register = function() length(btw_list_skills()) > 0
      ),
      arguments = list(
        skill_name = ellmer::type_string(
          "The name of the skill to fetch"
        )
      )
    )
  }
)

# Skill Discovery ----------------------------------------------------------

btw_skill_directories <- function() {
  dirs <- character()

  # Package-bundled skills
  package_skills <- system.file("skills", package = "btw")
  if (nzchar(package_skills) && dir.exists(package_skills)) {
    dirs <- c(dirs, package_skills)
  }

  # User-level skills (global installation)
  user_skills_dir <- file.path(
    tools::R_user_dir("btw", "config"),
    "skills"
  )
  if (dir.exists(user_skills_dir)) {
    dirs <- c(dirs, user_skills_dir)
  }

  # Project-level skills from multiple conventions
  for (project_subdir in project_skill_subdirs()) {
    project_skills_dir <- file.path(getwd(), project_subdir)
    if (dir.exists(project_skills_dir)) {
      dirs <- c(dirs, project_skills_dir)
    }
  }

  dirs
}

project_skill_subdirs <- function() {
  c(
    file.path(".btw", "skills"),
    file.path(".agents", "skills")
  )
}

resolve_project_skill_dir <- function() {
  candidates <- file.path(getwd(), project_skill_subdirs())
  existing <- candidates[dir.exists(candidates)]

  if (length(existing) == 0) {
    # None exist yet, default to .btw/skills
    return(candidates[[1]])
  }

  if (length(existing) == 1) {
    return(existing[[1]])
  }

  # Multiple exist — if interactive, let the user choose
  if (!is_interactive()) {
    return(existing[[1]])
  }

  cli::cli_alert_success("Multiple skill directories found in project:")
  choice <- utils::menu(
    choices = existing,
    graphics = FALSE,
    title = "\u276F Which directory should be used?"
  )

  if (choice == 0) {
    cli::cli_abort("Aborted by user.")
  }

  existing[[choice]]
}

btw_list_skills <- function() {
  skill_dirs <- btw_skill_directories()
  all_skills <- list()

  for (dir in skill_dirs) {
    if (!dir.exists(dir)) {
      next
    }

    subdirs <- list.dirs(dir, full.names = TRUE, recursive = FALSE)

    for (subdir in subdirs) {
      skill_md_path <- file.path(subdir, "SKILL.md")
      if (!file.exists(skill_md_path)) {
        next
      }

      validation <- validate_skill(subdir)
      if (!validation$valid) {
        cli::cli_warn(c(
          "Skipping invalid skill in {.path {subdir}}.",
          set_names(validation$issues, rep("!", length(validation$issues)))
        ))
        next
      }

      metadata <- extract_skill_metadata(skill_md_path)
      skill_name <- basename(subdir)

      skill_entry <- list(
        name = skill_name,
        description = metadata$description %||% "No description available",
        path = skill_md_path
      )

      if (!is.null(metadata$compatibility)) {
        skill_entry$compatibility <- metadata$compatibility
      }
      if (!is.null(metadata[["allowed-tools"]])) {
        skill_entry$allowed_tools <- metadata[["allowed-tools"]]
      }

      all_skills[[skill_name]] <- skill_entry
    }
  }

  all_skills
}

find_skill <- function(skill_name) {
  skill_dirs <- btw_skill_directories()

  for (dir in skill_dirs) {
    skill_dir <- file.path(dir, skill_name)
    skill_md_path <- file.path(skill_dir, "SKILL.md")
    if (dir.exists(skill_dir) && file.exists(skill_md_path)) {
      validation <- validate_skill(skill_dir)
      if (!validation$valid) {
        return(NULL)
      }
      return(list(
        path = skill_md_path,
        base_dir = skill_dir
      ))
    }
  }

  NULL
}

extract_skill_metadata <- function(skill_path) {
  tryCatch(
    {
      fm <- frontmatter::read_front_matter(skill_path)
      fm$data %||% list()
    },
    error = function(e) {
      cli::cli_warn(
        "Failed to parse frontmatter in {.path {skill_path}}: {e$message}"
      )
      list()
    }
  )
}

# Skill Validation ---------------------------------------------------------

validate_skill_name <- function(name, dir_name = NULL) {
  issues <- character()

  if (is.null(name) || !is.character(name) || !nzchar(name)) {
    issues <- c(issues, "Missing or empty 'name' field in frontmatter.")
    return(issues)
  }

  if (nchar(name) > 64) {
    issues <- c(
      issues,
      sprintf("Name is too long (%d characters, max 64).", nchar(name))
    )
  }
  if (!grepl("^[a-z0-9][a-z0-9-]*[a-z0-9]$|^[a-z0-9]$", name)) {
    issues <- c(
      issues,
      sprintf(
        "Name '%s' must contain only lowercase letters, numbers, and hyphens, and must not start or end with a hyphen.",
        name
      )
    )
  }
  if (grepl("--", name)) {
    issues <- c(
      issues,
      sprintf("Name '%s' must not contain consecutive hyphens.", name)
    )
  }
  if (!is.null(dir_name) && name != dir_name) {
    issues <- c(
      issues,
      sprintf(
        "Name '%s' in frontmatter does not match directory name '%s'.",
        name,
        dir_name
      )
    )
  }

  issues
}

validate_skill <- function(skill_dir) {
  skill_dir <- normalizePath(skill_dir, mustWork = FALSE)
  issues <- character()

  skill_md_path <- file.path(skill_dir, "SKILL.md")
  if (!file.exists(skill_md_path)) {
    return(list(
      valid = FALSE,
      issues = "SKILL.md not found."
    ))
  }

  metadata <- tryCatch(
    {
      fm <- frontmatter::read_front_matter(skill_md_path)
      fm$data
    },
    error = function(e) {
      issues <<- c(
        issues,
        sprintf("Failed to parse frontmatter: %s", e$message)
      )
      NULL
    }
  )

  if (is.null(metadata)) {
    issues <- c(issues, "No YAML frontmatter found.")
    return(list(valid = FALSE, issues = issues))
  }

  if (!is.list(metadata)) {
    return(list(
      valid = FALSE,
      issues = "Frontmatter must be a YAML mapping."
    ))
  }

  # Check for unexpected properties
  allowed_fields <- c(
    "name",
    "description",
    "license",
    "compatibility",
    "metadata",
    "allowed-tools"
  )
  unexpected <- setdiff(names(metadata), allowed_fields)
  if (length(unexpected) > 0) {
    issues <- c(
      issues,
      sprintf(
        "Unexpected frontmatter field(s): %s. Allowed fields: %s.",
        paste(unexpected, collapse = ", "),
        paste(allowed_fields, collapse = ", ")
      )
    )
  }

  # Validate name
  issues <- c(issues, validate_skill_name(metadata$name, basename(skill_dir)))

  # Validate description
  description <- metadata$description
  if (
    is.null(description) || !is.character(description) || !nzchar(description)
  ) {
    issues <- c(issues, "Missing or empty 'description' field in frontmatter.")
  } else if (nchar(description) > 1024) {
    issues <- c(
      issues,
      sprintf(
        "Description is too long (%d characters, max 1024).",
        nchar(description)
      )
    )
  }

  # Validate optional fields
  if (!is.null(metadata$compatibility)) {
    if (!is.character(metadata$compatibility)) {
      issues <- c(
        issues,
        "The 'compatibility' field must be a character string."
      )
    } else if (nchar(metadata$compatibility) > 500) {
      issues <- c(
        issues,
        sprintf(
          "Compatibility field is too long (%d characters, max 500).",
          nchar(metadata$compatibility)
        )
      )
    }
  }

  if (!is.null(metadata$metadata) && !is.list(metadata$metadata)) {
    issues <- c(issues, "The 'metadata' field must be a key-value mapping.")
  }

  list(
    valid = length(issues) == 0,
    issues = issues
  )
}

# Skill Resources ----------------------------------------------------------

list_skill_resources <- function(skill_dir) {
  list(
    scripts = list_files_in_subdir(skill_dir, "scripts"),
    references = list_files_in_subdir(skill_dir, "references"),
    assets = list_files_in_subdir(skill_dir, "assets")
  )
}

list_files_in_subdir <- function(base_dir, subdir) {
  full_path <- file.path(base_dir, subdir)
  if (!dir.exists(full_path)) {
    return(character(0))
  }
  list.files(full_path, full.names = FALSE, recursive = TRUE)
}

has_skill_resources <- function(resources) {
  length(resources$scripts) > 0 ||
    length(resources$references) > 0 ||
    length(resources$assets) > 0
}

format_resources_listing <- function(resources, base_dir) {
  if (!has_skill_resources(resources)) {
    return("")
  }

  parts <- character()
  parts <- c(parts, "\n\n---\n\n## Bundled Resources\n")

  if (length(resources$scripts) > 0) {
    parts <- c(parts, "\n**Scripts:**\n")
    script_paths <- file.path(base_dir, "scripts", resources$scripts)
    parts <- c(parts, paste0("- ", script_paths, collapse = "\n"))
  }

  if (length(resources$references) > 0) {
    parts <- c(parts, "\n\n**References:**\n")
    ref_paths <- file.path(base_dir, "references", resources$references)
    parts <- c(parts, paste0("- ", ref_paths, collapse = "\n"))
  }

  if (length(resources$assets) > 0) {
    parts <- c(parts, "\n\n**Assets:**\n")
    asset_paths <- file.path(base_dir, "assets", resources$assets)
    parts <- c(parts, paste0("- ", asset_paths, collapse = "\n"))
  }

  paste(parts, collapse = "")
}

xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}

# System Prompt ------------------------------------------------------------

btw_skills_system_prompt <- function() {
  skills <- btw_list_skills()

  if (length(skills) == 0) {
    return("")
  }

  skills_prompt_path <- system.file("prompts", "skills.md", package = "btw")
  explanation <- if (file.exists(skills_prompt_path)) {
    paste(readLines(skills_prompt_path, warn = FALSE), collapse = "\n")
  } else {
    "## Skills\n\nYou have access to specialized skills that provide detailed guidance for specific tasks."
  }

  skill_items <- vapply(
    skills,
    function(skill) {
      parts <- sprintf(
        "<skill>\n<name>%s</name>\n<description>%s</description>\n<location>%s</location>",
        xml_escape(skill$name),
        xml_escape(skill$description),
        xml_escape(skill$path)
      )
      if (!is.null(skill$compatibility)) {
        parts <- paste0(
          parts,
          sprintf(
            "\n<compatibility>%s</compatibility>",
            xml_escape(skill$compatibility)
          )
        )
      }
      if (!is.null(skill$allowed_tools)) {
        parts <- paste0(
          parts,
          sprintf(
            "\n<allowed-tools>%s</allowed-tools>",
            xml_escape(skill$allowed_tools)
          )
        )
      }
      paste0(parts, "\n</skill>")
    },
    character(1)
  )

  paste0(
    explanation,
    "\n\n<available_skills>\n",
    paste(skill_items, collapse = "\n"),
    "\n</available_skills>"
  )
}

# Scope Resolution ---------------------------------------------------------

resolve_skill_scope <- function(scope, error_call = caller_env()) {
  if (inherits(scope, "AsIs")) {
    return(as.character(scope))
  }

  switch(
    scope,
    project = resolve_project_skill_dir(),
    user = file.path(tools::R_user_dir("btw", "config"), "skills"),
    scope
  )
}

# User-Facing Skill Management ---------------------------------------------

#' Create a new skill
#'
#' @description
#' Initialize a new skill directory following the
#' [Agent Skills specification](https://agentskills.io). Creates a `SKILL.md`
#' file with proper YAML frontmatter and optionally creates resource
#' directories (`scripts/`, `references/`, `assets/`).
#'
#' @param name The skill name. Must be a valid skill name: lowercase letters,
#'   numbers, and hyphens only, 1-64 characters, must not start or end with a
#'   hyphen, and must not contain consecutive hyphens.
#' @param description A description of what the skill does and when to use it.
#'   Maximum 1024 characters.
#' @param scope Where to create the skill. One of:
#'   - `"project"` (default): Creates in a project-level skills directory,
#'     chosen from `.btw/skills/` or `.agents/skills/`
#'     in that order. If one already exists, it is used; otherwise
#'     `.btw/skills/` is created.
#'   - `"user"`: Creates in the user-level skills directory
#'     (`tools::R_user_dir("btw", "config")/skills`).
#'   - A directory path: Creates the skill inside this path, e.g.
#'     `scope = ".openhands/skills"`. Use `I("project")` or `I("user")`
#'     if you need a literal directory with those names.
#' @param resources Logical. If `TRUE` (the default), creates empty
#'   `scripts/`, `references/`, and `assets/` subdirectories.
#'
#' @return The path to the created skill directory, invisibly.
#'
#' @family skills
#' @export
btw_skill_create <- function(
  name,
  description = "",
  scope = "project",
  resources = TRUE
) {
  check_name(name)
  check_string(description)
  check_string(scope)
  check_bool(resources)

  if (nchar(description) > 1024) {
    cli::cli_warn(
      "Skill description is long ({nchar(description)} characters, recommended max 1024)."
    )
  }

  # Validate name format
  name_issues <- validate_skill_name(name)
  if (length(name_issues) > 0) {
    cli::cli_abort(c(
      "Invalid skill name {.val {name}}:",
      set_names(name_issues, rep("!", length(name_issues)))
    ))
  }

  # Resolve target directory
  parent_dir <- resolve_skill_scope(scope)

  skill_dir <- file.path(parent_dir, name)

  if (dir.exists(skill_dir)) {
    cli::cli_abort("Skill directory already exists: {.path {skill_dir}}")
  }

  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)

  # Generate SKILL.md
  skill_title <- gsub("-", " ", name)
  skill_title <- paste0(
    toupper(substring(skill_title, 1, 1)),
    substring(skill_title, 2)
  )

  description_line <- if (nzchar(description)) {
    description
  } else {
    "TODO: Describe what this skill does and when to use it."
  }

  skill_md_content <- paste0(
    "---\n",
    "name: ",
    name,
    "\n",
    "description: ",
    description_line,
    "\n",
    "---\n",
    "\n",
    "# ",
    skill_title,
    "\n",
    "\n",
    "TODO: Add skill instructions here.\n"
  )

  write_lines(skill_md_content, file.path(skill_dir, "SKILL.md"))

  if (resources) {
    dir.create(file.path(skill_dir, "scripts"), showWarnings = FALSE)
    dir.create(file.path(skill_dir, "references"), showWarnings = FALSE)
    dir.create(file.path(skill_dir, "assets"), showWarnings = FALSE)
  }

  cli::cli_inform(c(
    "v" = "Created skill {.val {name}} at {.path {skill_dir}}",
    "i" = "Edit {.file {file.path(skill_dir, 'SKILL.md')}} to add instructions."
  ))

  invisible(skill_dir)
}

#' Validate a skill
#'
#' @description
#' Validate a skill directory against the
#' [Agent Skills specification](https://agentskills.io). Checks that the
#' `SKILL.md` file exists, has valid YAML frontmatter, and that required
#' fields follow the specification's naming and format rules.
#'
#' @param path Path to a skill directory (containing a `SKILL.md` file).
#'
#' @return A list with `valid` (logical) and `issues` (character vector of
#'   validation messages), invisibly. Issues are also printed to the console.
#'
#' @family skills
#' @export
btw_skill_validate <- function(path = ".") {
  check_string(path)

  path <- normalizePath(path, mustWork = FALSE)
  if (!dir.exists(path)) {
    cli::cli_abort("Directory does not exist: {.path {path}}")
  }

  result <- validate_skill(path)

  if (result$valid) {
    cli::cli_inform(c("v" = "Skill at {.path {path}} is valid."))
  } else {
    cli::cli_inform(c(
      "x" = "Skill at {.path {path}} has validation issues:",
      set_names(result$issues, rep("!", length(result$issues)))
    ))
  }

  invisible(result)
}

select_skill_dir <- function(
  skill_dirs,
  skill = NULL,
  source_label = "source"
) {
  if (length(skill_dirs) == 0) {
    cli::cli_abort("No skills found in {source_label}.")
  }

  if (!is.null(skill)) {
    check_string(skill)
    match_idx <- match(skill, basename(skill_dirs))
    if (is.na(match_idx)) {
      available <- basename(skill_dirs)
      cli::cli_abort(c(
        "Skill {.val {skill}} not found in {source_label}.",
        "i" = "Available skills: {.val {available}}"
      ))
    }
    return(skill_dirs[[match_idx]])
  }

  if (length(skill_dirs) == 1) {
    return(skill_dirs[[1]])
  }

  # Multiple skills, no name specified

  if (!is_interactive()) {
    available <- basename(skill_dirs)
    cli::cli_abort(c(
      "Multiple skills found in {source_label}.",
      "i" = "Available skills: {.val {available}}",
      "i" = "Use the {.arg skill} argument to select one."
    ))
  }

  cli::cli_alert_info("Multiple skills found in {source_label}:")
  choice <- utils::menu(
    choices = basename(skill_dirs),
    graphics = FALSE,
    title = "\u276F Which skill would you like to install?"
  )

  if (choice == 0) {
    cli::cli_abort("Aborted by user.")
  }

  skill_dirs[[choice]]
}

#' Install a skill from GitHub
#'
#' @description
#' Download and install a skill from a GitHub repository. The repository
#' should contain one or more skill directories, each with a `SKILL.md` file.
#'
#' @param repo GitHub repository in `"owner/repo"` format.
#' @param skill Optional skill name. If `NULL` and the repository contains
#'   multiple skills, an interactive picker is shown (or an error in
#'   non-interactive sessions).
#' @param ref Git reference (branch, tag, or SHA) to download. Defaults to
#'   `"HEAD"`.
#' @param scope Where to install the skill. One of:
#'   - `"project"` (default): Installs to a project-level skills directory,
#'     chosen from `.btw/skills/` or `.agents/skills/`
#'     in that order. If one already exists, it is used; otherwise
#'     `.btw/skills/` is created.
#'   - `"user"`: Installs to the user-level skills directory
#'     (`tools::R_user_dir("btw", "config")/skills`).
#'   - A directory path: Installs to a custom directory, e.g.
#'     `scope = ".openhands/skills"`. Use `I("project")` or `I("user")`
#'     if you need a literal directory with those names.
#' @param overwrite If `TRUE`, overwrite an existing skill with the same name.
#'   Defaults to `FALSE`, which errors if the skill already exists.
#'
#' @return The path to the installed skill directory, invisibly.
#'
#' @family skills
#' @export
btw_skill_install_github <- function(
  repo,
  skill = NULL,
  ref = "HEAD",
  scope = "project",
  overwrite = FALSE
) {
  check_string(repo)
  if (!is.null(skill)) {
    check_string(skill)
  }
  check_string(ref)
  check_string(scope)
  check_bool(overwrite)

  rlang::check_installed("gh", reason = "to install skills from GitHub.")

  # Validate repo format
  parts <- strsplit(repo, "/", fixed = TRUE)[[1]]
  if (length(parts) != 2 || !nzchar(parts[[1]]) || !nzchar(parts[[2]])) {
    cli::cli_abort(
      '{.arg repo} must be in {.val owner/repo} format, not {.val {repo}}.'
    )
  }
  owner <- parts[[1]]
  repo_name <- parts[[2]]

  # Download zipball
  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  tryCatch(
    gh::gh(
      "/repos/{owner}/{repo}/zipball/{ref}",
      owner = owner,
      repo = repo_name,
      ref = ref,
      .destfile = tmp_zip
    ),
    error = function(e) {
      cli::cli_abort(
        "Failed to download from GitHub repository {.val {repo}}: {e$message}",
        parent = e
      )
    }
  )

  # Extract to temp dir
  tmp_dir <- tempfile("btw_gh_skill_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  utils::unzip(tmp_zip, exdir = tmp_dir)

  # Find all SKILL.md files
  skill_files <- list.files(
    tmp_dir,
    pattern = "^SKILL\\.md$",
    recursive = TRUE,
    full.names = TRUE
  )

  if (length(skill_files) == 0) {
    cli::cli_abort("No skills found in GitHub repository {.val {repo}}.")
  }

  skill_dirs <- dirname(skill_files)

  selected <- select_skill_dir(
    skill_dirs,
    skill = skill,
    source_label = cli::format_inline("GitHub repository {.field {repo}}")
  )

  install_skill_from_dir(selected, scope = scope, overwrite = overwrite)
}

#' Install a skill from an R package
#'
#' @description
#' Install a skill bundled in an R package. Packages can bundle skills in
#' their `inst/skills/` directory, where each subdirectory containing a
#' `SKILL.md` file is a skill.
#'
#' @param package Name of an installed R package that bundles skills.
#' @param skill Optional skill name. If `NULL` and the package contains
#'   multiple skills, an interactive picker is shown (or an error in
#'   non-interactive sessions).
#' @param scope Where to install the skill. One of:
#'   - `"project"` (default): Installs to a project-level skills directory,
#'     chosen from `.btw/skills/` or `.agents/skills/`
#'     in that order. If one already exists, it is used; otherwise
#'     `.btw/skills/` is created.
#'   - `"user"`: Installs to the user-level skills directory
#'     (`tools::R_user_dir("btw", "config")/skills`).
#'   - A directory path: Installs to a custom directory, e.g.
#'     `scope = ".openhands/skills"`. Use `I("project")` or `I("user")`
#'     if you need a literal directory with those names.
#' @param overwrite If `TRUE`, overwrite an existing skill with the same name.
#'   Defaults to `FALSE`, which errors if the skill already exists.
#'
#' @return The path to the installed skill directory, invisibly.
#'
#' @family skills
#' @export
btw_skill_install_package <- function(
  package,
  skill = NULL,
  scope = "project",
  overwrite = FALSE
) {
  check_string(package)
  if (!is.null(skill)) {
    check_string(skill)
  }
  check_string(scope)
  check_bool(overwrite)

  rlang::check_installed(package, reason = "to install skills from it.")

  skills_dir <- system.file("skills", package = package)
  if (!nzchar(skills_dir) || !dir.exists(skills_dir)) {
    cli::cli_abort("Package {.pkg {package}} does not bundle any skills.")
  }

  # Find subdirectories that contain SKILL.md
  subdirs <- list.dirs(skills_dir, full.names = TRUE, recursive = FALSE)
  skill_dirs <- subdirs[file.exists(file.path(subdirs, "SKILL.md"))]

  if (length(skill_dirs) == 0) {
    cli::cli_abort("Package {.pkg {package}} does not bundle any skills.")
  }

  selected <- select_skill_dir(
    skill_dirs,
    skill = skill,
    source_label = cli::format_inline("package {.pkg {package}}")
  )

  install_skill_from_dir(selected, scope = scope, overwrite = overwrite)
}

install_skill_from_dir <- function(
  source_dir,
  scope = "project",
  overwrite = FALSE
) {
  check_string(source_dir)
  check_string(scope)
  check_bool(overwrite)

  source_dir <- normalizePath(source_dir, mustWork = FALSE)
  if (!dir.exists(source_dir)) {
    cli::cli_abort("Source directory not found: {.path {source_dir}}")
  }

  # Determine target directory
  target_parent <- resolve_skill_scope(scope)

  skill_name <- basename(source_dir)
  target_dir <- file.path(target_parent, skill_name)

  if (dir.exists(target_dir)) {
    if (overwrite) {
      unlink(target_dir, recursive = TRUE)
    } else {
      cli::cli_abort(
        "Skill {.val {skill_name}} already exists at {.path {target_dir}}."
      )
    }
  }

  # Validate before installing
  validation <- validate_skill(source_dir)
  if (!validation$valid) {
    cli::cli_abort(c(
      "Cannot install invalid skill:",
      set_names(validation$issues, rep("!", length(validation$issues)))
    ))
  }

  dir.create(target_parent, recursive = TRUE, showWarnings = FALSE)
  fs::dir_copy(source_dir, target_dir)

  cli::cli_inform(c(
    "v" = "Installed skill {.val {skill_name}} to {.path {target_dir}}"
  ))

  invisible(target_dir)
}
