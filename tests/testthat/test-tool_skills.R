# Helper to create a temp skill directory with valid SKILL.md
create_temp_skill <- function(
  name = "test-skill",
  description = "A test skill for unit testing.",
  extra_frontmatter = "",
  body = "\n# Test Skill\n\nInstructions here.\n",
  dir = NULL
) {
  if (is.null(dir)) {
    dir <- withr::local_tempdir(.local_envir = parent.frame())
  }

  skill_dir <- file.path(dir, name)
  dir.create(skill_dir, recursive = TRUE, showWarnings = FALSE)

  frontmatter <- paste0(
    "---\n",
    "name: ", name, "\n",
    "description: ", description, "\n",
    extra_frontmatter,
    "---\n"
  )

  writeLines(paste0(frontmatter, body), file.path(skill_dir, "SKILL.md"))
  skill_dir
}

# Helper to mock skill directories for discovery
local_skill_dirs <- function(dirs, .env = parent.frame()) {
  local_mocked_bindings(
    btw_skill_directories = function() dirs,
    .env = .env
  )
}

# Validation ---------------------------------------------------------------

test_that("validate_skill() passes for valid skill", {
  skill_dir <- create_temp_skill()
  result <- validate_skill(skill_dir)
  expect_true(result$valid)
  expect_length(result$issues, 0)
})

test_that("validate_skill() fails when SKILL.md is missing", {
  dir <- withr::local_tempdir()
  result <- validate_skill(dir)
  expect_false(result$valid)
  expect_match(result$issues, "SKILL.md not found")
})

test_that("validate_skill() fails for missing name field", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "test-skill")
  dir.create(skill_dir)
  writeLines("---\ndescription: A skill.\n---\nBody.", file.path(skill_dir, "SKILL.md"))
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "Missing or empty 'name'", all = FALSE)
})

test_that("validate_skill() fails for missing description field", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "test-skill")
  dir.create(skill_dir)
  writeLines("---\nname: test-skill\n---\nBody.", file.path(skill_dir, "SKILL.md"))
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "Missing or empty 'description'", all = FALSE)
})

test_that("validate_skill() fails for name with uppercase", {
  skill_dir <- create_temp_skill(name = "test-skill")
  # Manually override the name in SKILL.md to have uppercase
  writeLines(
    "---\nname: Test-Skill\ndescription: A test.\n---\nBody.",
    file.path(skill_dir, "SKILL.md")
  )
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "lowercase letters", all = FALSE)
})

test_that("validate_skill() fails for name starting with hyphen", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "-bad-name")
  dir.create(skill_dir)
  writeLines(
    "---\nname: -bad-name\ndescription: A test.\n---\nBody.",
    file.path(skill_dir, "SKILL.md")
  )
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "must not start or end with a hyphen", all = FALSE)
})

test_that("validate_skill() fails for consecutive hyphens", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "bad--name")
  dir.create(skill_dir)
  writeLines(
    "---\nname: bad--name\ndescription: A test.\n---\nBody.",
    file.path(skill_dir, "SKILL.md")
  )
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "consecutive hyphens", all = FALSE)
})

test_that("validate_skill() fails for name exceeding 64 characters", {
  long_name <- paste(rep("a", 65), collapse = "")
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, long_name)
  dir.create(skill_dir)
  writeLines(
    paste0("---\nname: ", long_name, "\ndescription: A test.\n---\nBody."),
    file.path(skill_dir, "SKILL.md")
  )
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "too long", all = FALSE)
})

test_that("validate_skill() fails when name doesn't match directory", {
  skill_dir <- create_temp_skill(name = "test-skill")
  writeLines(
    "---\nname: other-name\ndescription: A test.\n---\nBody.",
    file.path(skill_dir, "SKILL.md")
  )
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "does not match directory name", all = FALSE)
})

test_that("validate_skill() fails for description exceeding 1024 characters", {
  long_desc <- paste(rep("a", 1025), collapse = "")
  skill_dir <- create_temp_skill(description = long_desc)
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "Description is too long", all = FALSE)
})

test_that("validate_skill() flags unexpected frontmatter fields", {
  skill_dir <- create_temp_skill(extra_frontmatter = "bogus: true\n")
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "Unexpected frontmatter", all = FALSE)
})

test_that("validate_skill() accepts optional fields", {
  skill_dir <- create_temp_skill(
    extra_frontmatter = "license: MIT\ncompatibility: Requires git\nallowed-tools: Read Bash\nmetadata:\n  author: test\n"
  )
  result <- validate_skill(skill_dir)
  expect_true(result$valid)
})

test_that("validate_skill() fails for compatibility exceeding 500 chars", {
  long_compat <- paste(rep("a", 501), collapse = "")
  skill_dir <- create_temp_skill(
    extra_frontmatter = paste0("compatibility: ", long_compat, "\n")
  )
  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "Compatibility field is too long", all = FALSE)
})

# Discovery ----------------------------------------------------------------

test_that("btw_list_skills() finds valid skills", {
  dir <- withr::local_tempdir()
  create_temp_skill(name = "my-skill", dir = dir)
  local_skill_dirs(dir)

  skills <- btw_list_skills()
  expect_length(skills, 1)
  expect_equal(skills[["my-skill"]]$name, "my-skill")
})

test_that("btw_list_skills() skips invalid skills with warning", {
  dir <- withr::local_tempdir()

  # Create a valid skill
  create_temp_skill(name = "good-skill", dir = dir)

  # Create an invalid skill (missing description)
  bad_dir <- file.path(dir, "bad-skill")
  dir.create(bad_dir)
  writeLines("---\nname: bad-skill\n---\nBody.", file.path(bad_dir, "SKILL.md"))

  local_skill_dirs(dir)

  expect_warning(
    skills <- btw_list_skills(),
    "Skipping invalid skill"
  )
  expect_length(skills, 1)
  expect_equal(skills[["good-skill"]]$name, "good-skill")
})

test_that("btw_list_skills() includes compatibility and allowed-tools", {
  dir <- withr::local_tempdir()
  create_temp_skill(
    name = "fancy-skill",
    dir = dir,
    extra_frontmatter = "compatibility: Requires Python 3\nallowed-tools: Read Bash\n"
  )
  local_skill_dirs(dir)

  skills <- btw_list_skills()
  expect_equal(skills[["fancy-skill"]]$compatibility, "Requires Python 3")
  expect_equal(skills[["fancy-skill"]]$allowed_tools, "Read Bash")
})

test_that("btw_skill_directories() discovers skills from multiple project dirs", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  project <- getwd()  # resolve symlinks (e.g. /private/var on macOS)

  # Create skills in .btw/skills and .claude/skills
  btw_dir <- file.path(project, ".btw", "skills")
  claude_dir <- file.path(project, ".claude", "skills")
  dir.create(btw_dir, recursive = TRUE)
  dir.create(claude_dir, recursive = TRUE)

  dirs <- btw_skill_directories()
  expect_true(btw_dir %in% dirs)
  expect_true(claude_dir %in% dirs)
})

test_that("btw_skill_directories() discovers .agents/skills", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  project <- getwd()

  agents_dir <- file.path(project, ".agents", "skills")
  dir.create(agents_dir, recursive = TRUE)

  dirs <- btw_skill_directories()
  expect_true(agents_dir %in% dirs)
})

test_that("resolve_project_skill_dir() defaults to .btw/skills when none exist", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  project <- getwd()

  result <- resolve_project_skill_dir()
  expect_equal(result, file.path(project, ".btw", "skills"))
})

test_that("resolve_project_skill_dir() returns the one that exists", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  project <- getwd()

  agents_dir <- file.path(project, ".agents", "skills")
  dir.create(agents_dir, recursive = TRUE)

  result <- resolve_project_skill_dir()
  expect_equal(result, agents_dir)
})

test_that("resolve_project_skill_dir() returns first existing when non-interactive", {
  project <- withr::local_tempdir()
  withr::local_dir(project)
  project <- getwd()

  btw_dir <- file.path(project, ".btw", "skills")
  agents_dir <- file.path(project, ".agents", "skills")
  dir.create(btw_dir, recursive = TRUE)
  dir.create(agents_dir, recursive = TRUE)

  local_mocked_bindings(is_interactive = function() FALSE)
  result <- resolve_project_skill_dir()
  expect_equal(result, btw_dir)
})

test_that("find_skill() returns NULL for nonexistent skill", {
  dir <- withr::local_tempdir()
  local_skill_dirs(dir)
  expect_null(find_skill("nonexistent"))
})

test_that("find_skill() finds a valid skill", {
  dir <- withr::local_tempdir()
  create_temp_skill(name = "found-skill", dir = dir)
  local_skill_dirs(dir)

  result <- find_skill("found-skill")
  expect_type(result, "list")
  expect_true(file.exists(result$path))
})

# extract_skill_metadata ---------------------------------------------------

test_that("extract_skill_metadata() returns parsed frontmatter", {
  skill_dir <- create_temp_skill(
    extra_frontmatter = "license: MIT\n"
  )
  metadata <- extract_skill_metadata(file.path(skill_dir, "SKILL.md"))
  expect_equal(metadata$name, "test-skill")
  expect_equal(metadata$license, "MIT")
})

test_that("extract_skill_metadata() returns empty list for bad files", {
  tmp <- withr::local_tempfile(lines = "No frontmatter here", fileext = ".md")
  metadata <- extract_skill_metadata(tmp)
  expect_equal(metadata, list())
})

# Resources ----------------------------------------------------------------

test_that("list_skill_resources() finds files recursively", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "test-skill")
  dir.create(file.path(skill_dir, "scripts"), recursive = TRUE)
  dir.create(file.path(skill_dir, "assets", "templates"), recursive = TRUE)
  writeLines("print('hi')", file.path(skill_dir, "scripts", "run.py"))
  writeLines("template", file.path(skill_dir, "assets", "templates", "base.html"))

  resources <- list_skill_resources(skill_dir)
  expect_equal(resources$scripts, "run.py")
  expect_true("templates/base.html" %in% resources$assets)
})

test_that("format_resources_listing() returns empty string for no resources", {
  resources <- list(scripts = character(0), references = character(0), assets = character(0))
  expect_equal(format_resources_listing(resources, "/tmp"), "")
})

# Fetch Skill Tool ---------------------------------------------------------

test_that("btw_tool_fetch_skill_impl() returns content and resources", {
  dir <- withr::local_tempdir()
  skill_dir <- create_temp_skill(name = "fetch-test", dir = dir)
  dir.create(file.path(skill_dir, "references"))
  writeLines("Reference doc.", file.path(skill_dir, "references", "guide.md"))

  local_skill_dirs(dir)

  result <- btw_tool_fetch_skill_impl("fetch-test")
  expect_s3_class(result, "ellmer::ContentToolResult")
  expect_match(result@value, "Test Skill")
  expect_match(result@value, "References:")
  expect_equal(result@extra$data$name, "fetch-test")
  expect_equal(result@extra$data$resources$references, "guide.md")
})

test_that("btw_tool_fetch_skill_impl() errors for missing skill", {
  dir <- withr::local_tempdir()
  local_skill_dirs(dir)
  expect_error(btw_tool_fetch_skill_impl("nonexistent"), "not found")
})

# System Prompt ------------------------------------------------------------

test_that("btw_skills_system_prompt() returns empty for no skills", {
  dir <- withr::local_tempdir()
  local_skill_dirs(dir)
  expect_equal(btw_skills_system_prompt(), "")
})

test_that("btw_skills_system_prompt() includes skill metadata", {
  dir <- withr::local_tempdir()
  create_temp_skill(
    name = "prompt-test",
    description = "A skill for testing prompts.",
    dir = dir,
    extra_frontmatter = "compatibility: Needs R 4.2\n"
  )
  local_skill_dirs(dir)

  prompt <- btw_skills_system_prompt()
  expect_match(prompt, "<name>prompt-test</name>")
  expect_match(prompt, "A skill for testing prompts.")
  expect_match(prompt, "<compatibility>Needs R 4.2</compatibility>")
})

# btw_skill_create ---------------------------------------------------------

test_that("btw_skill_create() creates valid skill directory", {
  dir <- withr::local_tempdir()
  path <- btw_skill_create(
    name = "my-new-skill",
    description = "A new skill.",
    scope = dir,
    resources = TRUE
  )

  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "SKILL.md")))
  expect_true(dir.exists(file.path(path, "scripts")))
  expect_true(dir.exists(file.path(path, "references")))
  expect_true(dir.exists(file.path(path, "assets")))

  # Validate the created skill
  result <- validate_skill(path)
  expect_true(result$valid)
})

test_that("btw_skill_create() without resources omits directories", {
  dir <- withr::local_tempdir()
  path <- btw_skill_create(
    name = "minimal-skill",
    description = "Minimal.",
    scope = dir,
    resources = FALSE
  )

  expect_true(file.exists(file.path(path, "SKILL.md")))
  expect_false(dir.exists(file.path(path, "scripts")))
})

test_that("btw_skill_create() errors for invalid name", {
  dir <- withr::local_tempdir()
  expect_error(btw_skill_create(name = "Bad-Name", scope = dir), "lowercase")
  expect_error(btw_skill_create(name = "-bad", scope = dir), "lowercase|hyphen")
  expect_error(btw_skill_create(name = "bad--name", scope = dir), "consecutive")
})

test_that("btw_skill_create() errors if skill already exists", {
  dir <- withr::local_tempdir()
  btw_skill_create(name = "existing", description = "First.", scope = dir)
  expect_error(
    btw_skill_create(name = "existing", description = "Second.", scope = dir),
    "already exists"
  )
})

test_that("btw_skill_create() treats I('project') as literal directory name", {
  dir <- withr::local_tempdir()
  path <- btw_skill_create(
    name = "literal-test",
    description = "Literal scope test.",
    scope = I(file.path(dir, "project"))
  )

  expect_true(dir.exists(path))
  expect_equal(dirname(path), file.path(dir, "project"))
})

# btw_skill_validate -------------------------------------------------------

test_that("btw_skill_validate() reports valid skill", {
  skill_dir <- create_temp_skill()
  expect_message(result <- btw_skill_validate(skill_dir), "valid")
  expect_true(result$valid)
})

test_that("btw_skill_validate() reports issues", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "bad-skill")
  dir.create(skill_dir)
  writeLines("---\nname: bad-skill\n---\nBody.", file.path(skill_dir, "SKILL.md"))
  expect_message(result <- btw_skill_validate(skill_dir), "validation issues")
  expect_false(result$valid)
})

test_that("btw_skill_validate() errors for nonexistent directory", {
  expect_error(btw_skill_validate("/nonexistent/path"), "does not exist")
})

# select_skill_dir ----------------------------------------------------------

test_that("select_skill_dir() returns single dir directly", {
  dir <- withr::local_tempdir()
  skill_dir <- create_temp_skill(name = "only-skill", dir = dir)
  result <- select_skill_dir(skill_dir)
  expect_equal(result, skill_dir)
})

test_that("select_skill_dir() matches named skill", {
  dir <- withr::local_tempdir()
  skill_a <- create_temp_skill(name = "skill-a", dir = dir)
  skill_b <- create_temp_skill(name = "skill-b", dir = dir)
  result <- select_skill_dir(c(skill_a, skill_b), skill = "skill-b")
  expect_equal(result, skill_b)
})

test_that("select_skill_dir() aborts when named skill not found", {
  dir <- withr::local_tempdir()
  skill_a <- create_temp_skill(name = "skill-a", dir = dir)
  expect_error(
    select_skill_dir(skill_a, skill = "nonexistent"),
    "not found"
  )
})

test_that("select_skill_dir() aborts for multiple dirs non-interactively", {
  dir <- withr::local_tempdir()
  skill_a <- create_temp_skill(name = "skill-a", dir = dir)
  skill_b <- create_temp_skill(name = "skill-b", dir = dir)
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_error(
    select_skill_dir(c(skill_a, skill_b)),
    "Multiple skills found"
  )
})

test_that("select_skill_dir() uses menu for multiple dirs interactively", {
  dir <- withr::local_tempdir()
  skill_a <- create_temp_skill(name = "skill-a", dir = dir)
  skill_b <- create_temp_skill(name = "skill-b", dir = dir)
  local_mocked_bindings(is_interactive = function() TRUE)
  local_mocked_bindings(menu = function(...) 2L, .package = "utils")
  expect_message(
    result <- select_skill_dir(c(skill_a, skill_b)),
    "Multiple skills found"
  )
  expect_equal(result, skill_b)
})

test_that("select_skill_dir() aborts when no dirs provided", {
  expect_error(select_skill_dir(character()), "No skills found")
})

# install_skill_from_dir ---------------------------------------------------

test_that("install_skill_from_dir() installs from directory", {
  source_dir <- withr::local_tempdir()
  create_temp_skill(name = "installable", dir = source_dir)

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)
  expect_message(
    path <- install_skill_from_dir(
      file.path(source_dir, "installable"),
      scope = "project"
    ),
    "Installed skill"
  )

  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "SKILL.md")))
})

test_that("install_skill_from_dir() refuses invalid skill", {
  source_dir <- withr::local_tempdir()
  bad_dir <- file.path(source_dir, "bad-skill")
  dir.create(bad_dir)
  writeLines("---\nname: bad-skill\n---\nBody.", file.path(bad_dir, "SKILL.md"))

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_error(install_skill_from_dir(bad_dir, scope = "project"), "Cannot install invalid")
})

test_that("install_skill_from_dir() errors for nonexistent source", {
  expect_error(install_skill_from_dir("/nonexistent/path"), "not found")
})

test_that("install_skill_from_dir() accepts custom path as scope", {
  source_dir <- withr::local_tempdir()
  create_temp_skill(name = "custom-install", dir = source_dir)

  target <- withr::local_tempdir()
  custom_dir <- file.path(target, ".openhands", "skills")

  expect_message(
    path <- install_skill_from_dir(
      file.path(source_dir, "custom-install"),
      scope = custom_dir
    ),
    "Installed skill"
  )
  expect_equal(dirname(path), custom_dir)
  expect_true(file.exists(file.path(path, "SKILL.md")))
})

test_that("install_skill_from_dir() treats I('project') as literal path", {
  source_dir <- withr::local_tempdir()
  create_temp_skill(name = "literal-test", dir = source_dir)

  target <- withr::local_tempdir()

  expect_message(
    path <- install_skill_from_dir(
      file.path(source_dir, "literal-test"),
      scope = I(file.path(target, "project"))
    ),
    "Installed skill"
  )
  expect_equal(dirname(path), file.path(target, "project"))
})

# btw_skill_install_github -------------------------------------------------

# Helper: create a zip mimicking GitHub's zipball format
create_github_zipball <- function(skills, zip_path = NULL) {
  tmp <- withr::local_tempdir(.local_envir = parent.frame())
  repo_dir <- file.path(tmp, "owner-repo-abc1234")
  dir.create(repo_dir)

  for (skill in skills) {
    skill_dir <- file.path(repo_dir, skill$name)
    dir.create(skill_dir, recursive = TRUE)
    writeLines(
      paste0(
        "---\nname: ", skill$name,
        "\ndescription: ", skill$description %||% "A test skill.",
        "\n---\n\n# ", skill$name, "\n\nInstructions.\n"
      ),
      file.path(skill_dir, "SKILL.md")
    )
  }

  if (is.null(zip_path)) {
    zip_path <- tempfile(fileext = ".zip", tmpdir = tmp)
  }
  withr::with_dir(tmp, {
    utils::zip(zip_path, basename(repo_dir), flags = "-r9Xq")
  })
  zip_path
}

test_that("btw_skill_install_github() errors for invalid repo format", {
  expect_error(btw_skill_install_github("badformat"), "owner/repo")
  expect_error(btw_skill_install_github("a/b/c"), "owner/repo")
  expect_error(btw_skill_install_github("/repo"), "owner/repo")
  expect_error(btw_skill_install_github("owner/"), "owner/repo")
})

test_that("btw_skill_install_github() installs single skill", {
  zip_path <- create_github_zipball(list(
    list(name = "gh-skill", description = "A GitHub skill.")
  ))

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  mock_gh <- function(..., .destfile = NULL) {
    file.copy(zip_path, .destfile)
  }

  local_mocked_bindings(gh = mock_gh, .package = "gh")

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_message(
    path <- btw_skill_install_github("owner/repo", scope = "project"),
    "Installed skill"
  )
  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "SKILL.md")))
  expect_equal(basename(path), "gh-skill")
})

test_that("btw_skill_install_github() selects named skill from multiple", {
  zip_path <- create_github_zipball(list(
    list(name = "skill-a", description = "Skill A."),
    list(name = "skill-b", description = "Skill B.")
  ))

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  mock_gh <- function(..., .destfile = NULL) {
    file.copy(zip_path, .destfile)
  }

  local_mocked_bindings(gh = mock_gh, .package = "gh")

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_message(
    path <- btw_skill_install_github("owner/repo", skill = "skill-b", scope = "project"),
    "Installed skill"
  )
  expect_equal(basename(path), "skill-b")
})

test_that("btw_skill_install_github() errors when named skill not found", {
  zip_path <- create_github_zipball(list(
    list(name = "skill-a", description = "Skill A.")
  ))

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  mock_gh <- function(..., .destfile = NULL) {
    file.copy(zip_path, .destfile)
  }

  local_mocked_bindings(gh = mock_gh, .package = "gh")

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_error(
    btw_skill_install_github("owner/repo", skill = "nonexistent"),
    "not found"
  )
})

test_that("btw_skill_install_github() errors when no skills in repo", {
  # Create a zip with no SKILL.md
  tmp <- withr::local_tempdir()
  repo_dir <- file.path(tmp, "owner-repo-abc1234")
  dir.create(repo_dir)
  writeLines("Just a README.", file.path(repo_dir, "README.md"))
  zip_path <- file.path(tmp, "empty.zip")
  withr::with_dir(tmp, {
    utils::zip(zip_path, basename(repo_dir), flags = "-r9Xq")
  })

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  mock_gh <- function(..., .destfile = NULL) {
    file.copy(zip_path, .destfile)
  }

  local_mocked_bindings(gh = mock_gh, .package = "gh")

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_error(
    btw_skill_install_github("owner/repo"),
    "No skills found"
  )
})

test_that("btw_skill_install_github() aborts non-interactively with multiple skills", {
  zip_path <- create_github_zipball(list(
    list(name = "skill-a", description = "Skill A."),
    list(name = "skill-b", description = "Skill B.")
  ))

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  mock_gh <- function(..., .destfile = NULL) {
    file.copy(zip_path, .destfile)
  }

  local_mocked_bindings(gh = mock_gh, .package = "gh")
  local_mocked_bindings(is_interactive = function() FALSE)

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_error(
    btw_skill_install_github("owner/repo"),
    "Multiple skills found"
  )
})

test_that("btw_skill_install_github() wraps download errors", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )

  mock_gh <- function(...) {
    stop("GitHub API error: Not Found")
  }

  local_mocked_bindings(gh = mock_gh, .package = "gh")

  expect_error(
    btw_skill_install_github("owner/nonexistent"),
    "Failed to download"
  )
})

# btw_skill_install_package ------------------------------------------------

test_that("btw_skill_install_package() installs single skill from package", {
  # Create a mock package skills directory
  pkg_skills <- withr::local_tempdir()
  skill_dir <- file.path(pkg_skills, "pkg-skill")
  dir.create(skill_dir)
  writeLines(
    "---\nname: pkg-skill\ndescription: A package skill.\n---\n\n# Pkg Skill\n",
    file.path(skill_dir, "SKILL.md")
  )

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    system.file = function(..., package = NULL) pkg_skills,
    .package = "base"
  )

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_message(
    path <- btw_skill_install_package("mypkg", scope = "project"),
    "Installed skill"
  )
  expect_true(dir.exists(path))
  expect_equal(basename(path), "pkg-skill")
})

test_that("btw_skill_install_package() selects named skill", {
  pkg_skills <- withr::local_tempdir()
  for (nm in c("alpha", "beta")) {
    d <- file.path(pkg_skills, nm)
    dir.create(d)
    writeLines(
      paste0("---\nname: ", nm, "\ndescription: Skill ", nm, ".\n---\n\n# ", nm, "\n"),
      file.path(d, "SKILL.md")
    )
  }

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    system.file = function(..., package = NULL) pkg_skills,
    .package = "base"
  )

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_message(
    path <- btw_skill_install_package("mypkg", skill = "beta", scope = "project"),
    "Installed skill"
  )
  expect_equal(basename(path), "beta")
})

test_that("btw_skill_install_package() errors when no skills dir", {
  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    system.file = function(..., package = NULL) "",
    .package = "base"
  )

  expect_error(
    btw_skill_install_package("emptypkg"),
    "does not bundle any skills"
  )
})

test_that("btw_skill_install_package() errors when no SKILL.md in subdirs", {
  pkg_skills <- withr::local_tempdir()
  dir.create(file.path(pkg_skills, "not-a-skill"))
  writeLines("just a file", file.path(pkg_skills, "not-a-skill", "README.md"))

  local_mocked_bindings(
    check_installed = function(...) invisible(),
    .package = "rlang"
  )
  local_mocked_bindings(
    system.file = function(..., package = NULL) pkg_skills,
    .package = "base"
  )

  expect_error(
    btw_skill_install_package("mypkg"),
    "does not bundle any skills"
  )
})

# Snapshot test for system prompt (existing) --------------------------------

test_that("btw_skills_system_prompt() works", {
  skip_if_not_snapshot_env()
  expect_snapshot(
    cat(btw_skills_system_prompt()),
    transform = function(x) {
      gsub("<location>.*?</location>", "<location>SKILL_PATH</location>", x)
    }
  )
})

# validate_skill_name() ----------------------------------------------------

test_that("validate_skill() accepts single-character name", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "a")
  dir.create(skill_dir)
  writeLines("---\nname: a\ndescription: A minimal skill.\n---\nBody.", file.path(skill_dir, "SKILL.md"))
  result <- validate_skill(skill_dir)
  expect_true(result$valid)
  expect_length(result$issues, 0)
})

test_that("validate_skill() flags non-character compatibility", {
  dir <- withr::local_tempdir()
  skill_dir <- file.path(dir, "test-skill")
  dir.create(skill_dir)
  writeLines(
    "---\nname: test-skill\ndescription: A test.\ncompatibility: true\n---\nBody.",
    file.path(skill_dir, "SKILL.md")
  )

  result <- validate_skill(skill_dir)
  expect_false(result$valid)
  expect_match(result$issues, "must be a character string", all = FALSE)
})

# find_skill() with invalid skill ------------------------------------------

test_that("find_skill() returns NULL for invalid skill on disk", {
  dir <- withr::local_tempdir()
  # Create a skill that exists on disk but fails validation (missing description)
  bad_dir <- file.path(dir, "bad-skill")
  dir.create(bad_dir)
  writeLines("---\nname: bad-skill\n---\nBody.", file.path(bad_dir, "SKILL.md"))

  local_skill_dirs(dir)
  expect_null(find_skill("bad-skill"))
})

# btw_skill_create() long description warning -------------------------------

test_that("btw_skill_create() warns on long description", {
  dir <- withr::local_tempdir()
  long_desc <- paste(rep("a", 1025), collapse = "")
  expect_warning(
    btw_skill_create(name = "long-desc", description = long_desc, scope = dir),
    "long"
  )
})

# install_skill_from_dir() overwrite ----------------------------------------

test_that("install_skill_from_dir() overwrites with overwrite = TRUE", {
  source_dir <- withr::local_tempdir()
  create_temp_skill(name = "overwrite-me", description = "Version 1.", dir = source_dir)

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  # First install
  expect_message(
    path <- install_skill_from_dir(
      file.path(source_dir, "overwrite-me"),
      scope = "project"
    ),
    "Installed skill"
  )

  # Update source
  writeLines(
    "---\nname: overwrite-me\ndescription: Version 2.\n---\nUpdated.",
    file.path(source_dir, "overwrite-me", "SKILL.md")
  )

  # Re-install with overwrite
  expect_message(
    path2 <- install_skill_from_dir(
      file.path(source_dir, "overwrite-me"),
      scope = "project",
      overwrite = TRUE
    ),
    "Installed skill"
  )

  expect_equal(path, path2)
  content <- readLines(file.path(path2, "SKILL.md"))
  expect_true(any(grepl("Version 2", content)))
})

# xml_escape() in system prompt ---------------------------------------------

test_that("btw_skills_system_prompt() escapes XML special characters", {
  dir <- withr::local_tempdir()
  create_temp_skill(
    name = "esc-test",
    description = "Uses <tags> & ampersands.",
    dir = dir
  )
  local_skill_dirs(dir)

  prompt <- btw_skills_system_prompt()
  expect_match(prompt, "&lt;tags&gt;", fixed = TRUE)
  expect_match(prompt, "&amp; ampersands", fixed = TRUE)
  expect_no_match(prompt, "<tags>", fixed = TRUE)
})

test_that("skills prompt is included in btw_client() system prompt", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  with_mocked_platform(ide = "rstudio", {
    chat <- btw_client(path_btw = FALSE)
  })

  system_prompt <- chat$get_system_prompt()

  expect_match(system_prompt, "## Skills", fixed = TRUE)
  expect_match(system_prompt, "<name>skill-creator</name>", fixed = TRUE)
})
