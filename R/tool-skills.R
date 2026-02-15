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


  package_skills <- system.file("skills", package = "btw")
  if (nzchar(package_skills) && dir.exists(package_skills)) {
    dirs <- c(dirs, package_skills)
  }


  user_skills_dir <- file.path(
    tools::R_user_dir("btw", "config"),
    "skills"
  )
  if (dir.exists(user_skills_dir)) {
    dirs <- c(dirs, user_skills_dir)
  }

  project_skills_dir <- file.path(getwd(), ".btw", "skills")
  if (dir.exists(project_skills_dir)) {
    dirs <- c(dirs, project_skills_dir)
  }

  dirs
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
      if (file.exists(skill_md_path)) {
        metadata <- extract_skill_metadata(skill_md_path)
        skill_name <- basename(subdir)

        all_skills[[skill_name]] <- list(
          name = skill_name,
          description = metadata$description %||% "No description available",
          path = skill_md_path
        )
      }
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
    error = function(e) list()
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
  list.files(full_path, full.names = FALSE)
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
      sprintf(
        "<skill>\n<name>%s</name>\n<description>%s</description>\n</skill>",
        skill$name,
        skill$description
      )
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
