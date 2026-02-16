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

# btw_skill_install --------------------------------------------------------

test_that("btw_skill_install() installs from directory", {
  # Create source skill
  source_dir <- withr::local_tempdir()
  create_temp_skill(name = "installable", dir = source_dir)

  # Install to target
  target_base <- withr::local_tempdir()
  target_dir <- file.path(target_base, ".btw", "skills")

  withr::local_dir(target_base)
  path <- btw_skill_install(
    file.path(source_dir, "installable"),
    scope = "project"
  )

  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "SKILL.md")))
})

test_that("btw_skill_install() installs from .skill file", {
  # Create source skill
  source_dir <- withr::local_tempdir()
  create_temp_skill(name = "zipped", dir = source_dir)

  # Create .skill archive
  skill_file <- file.path(withr::local_tempdir(), "zipped.skill")
  zip_wd <- source_dir
  withr::with_dir(zip_wd, {
    utils::zip(skill_file, "zipped", flags = "-r9Xq")
  })

  # Install
  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)
  path <- btw_skill_install(skill_file, scope = "project")

  expect_true(dir.exists(path))
  expect_true(file.exists(file.path(path, "SKILL.md")))
})

test_that("btw_skill_install() refuses invalid skill", {
  source_dir <- withr::local_tempdir()
  bad_dir <- file.path(source_dir, "bad-skill")
  dir.create(bad_dir)
  writeLines("---\nname: bad-skill\n---\nBody.", file.path(bad_dir, "SKILL.md"))

  target_base <- withr::local_tempdir()
  withr::local_dir(target_base)

  expect_error(btw_skill_install(bad_dir, scope = "project"), "Cannot install invalid")
})

test_that("btw_skill_install() errors for nonexistent source", {
  expect_error(btw_skill_install("/nonexistent/path"), "not found")
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

test_that("skills prompt is included in btw_client() system prompt", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  with_mocked_platform(ide = "rstudio", {
    chat <- btw_client(path_btw = FALSE)
  })

  system_prompt <- chat$get_system_prompt()

  expect_match(system_prompt, "## Skills", fixed = TRUE)
  expect_match(system_prompt, "<name>skill-creator</name>", fixed = TRUE)
})
