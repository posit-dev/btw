# Helper to create a temp skill directory with valid SKILL.md
create_temp_skill <- function(
  name = "test-skill",
  description = "A test skill for unit testing.",
  extra_frontmatter = list(),
  body = "\n# Test Skill\n\nInstructions here.\n",
  dir = NULL
) {
  if (is.null(dir)) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
  }

  skill_dir <- file.path(dir, name)
  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)

  skill <- list(
    data = list2(
      name = name,
      description = description,
      !!!extra_frontmatter
    ),
    body = body
  )

  frontmatter::write_front_matter(skill, file.path(skill_dir, "SKILL.md"))
  skill_dir
}

# Helper to mock skill directories for discovery
local_skill_dirs <- function(dirs, .env = parent.frame()) {
  local_mocked_bindings(
    btw_skill_directories = function() dirs,
    .env = .env
  )
}
