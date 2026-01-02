# Helper to create a valid agent-*.md file
create_test_agent_file <- function(dir, name = "test_agent", content = NULL) {
  if (is.null(content)) {
    content <- sprintf(
      "---
name: %s
description: A test agent
title: Test Agent
tools:
  - files
---

This is the system prompt for the test agent.",
      name
    )
  }

  path <- file.path(dir, sprintf("agent-%s.md", name))
  writeLines(content, path)
  path
}

# Test file discovery ---------------------------------------------------------

test_that("find_project_agent_files() finds .btw/agent-*.md files", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create some agent files
  agent1 <- file.path(btw_dir, "agent-code-reviewer.md")
  agent2 <- file.path(btw_dir, "agent-docs-writer.md")
  writeLines("---\nname: code_reviewer\n---\nReview code.", agent1)
  writeLines("---\nname: docs_writer\n---\nWrite docs.", agent2)

  # Also create a non-matching file
  writeLines("Not an agent", file.path(btw_dir, "other.md"))

  files <- withr::with_dir(tmp_dir, find_project_agent_files())

  expect_length(files, 2)
  expect_true(any(grepl("agent-code-reviewer.md$", files)))
  expect_true(any(grepl("agent-docs-writer.md$", files)))
  expect_false(any(grepl("other.md$", files)))
})

test_that("find_project_agent_files() returns empty when no .btw directory", {
  tmp_dir <- withr::local_tempdir()

  files <- withr::with_dir(tmp_dir, find_project_agent_files())

  expect_length(files, 0)
})

test_that("find_project_agent_files() returns empty when .btw exists but no agents", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create a non-agent file
  writeLines("Not an agent", file.path(btw_dir, "other.md"))

  files <- withr::with_dir(tmp_dir, find_project_agent_files())

  expect_length(files, 0)
})

test_that("find_user_agent_files() returns empty in test mode", {
  # In testthat, TESTTHAT=true, so should return empty
  expect_equal(Sys.getenv("TESTTHAT"), "true")

  files <- find_user_agent_files()

  expect_length(files, 0)
})

test_that("discover_agent_md_files() combines project and user files", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  agent1 <- file.path(btw_dir, "agent-test1.md")
  writeLines("---\nname: test1\n---\nTest 1.", agent1)

  # Clear cache

  files <- withr::with_dir(tmp_dir, discover_agent_md_files())

  expect_length(files, 1)
  expect_true(any(grepl("agent-test1.md$", files)))
})

test_that("discover_agent_md_files() returns unique files", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  agent1 <- file.path(btw_dir, "agent-unique.md")
  writeLines("---\nname: unique\n---\nUnique agent.", agent1)

  # Clear cache

  files <- withr::with_dir(tmp_dir, discover_agent_md_files())

  # Should not have duplicates
  expect_equal(length(files), length(unique(files)))
})

# Test name validation --------------------------------------------------------

test_that("validate_agent_name() accepts valid names", {
  valid_names <- c(
    "my_agent",
    "CodeReviewer",
    "agent1",
    "test_agent_v2",
    "AgentName"
  )

  for (name in valid_names) {
    expect_true(validate_agent_name(name, "test.md"))
  }
})

test_that("validate_agent_name() rejects empty or NULL names", {
  expect_warning(
    result <- validate_agent_name(NULL, "test.md"),
    "has no name"
  )
  expect_false(result)

  expect_warning(
    result <- validate_agent_name("", "test.md"),
    "has no name"
  )
  expect_false(result)
})

test_that("validate_agent_name() rejects reserved names (built-in tool names)", {
  # Any name that matches a built-in tool name is reserved
  reserved_name <- names(.btw_tools)[1]

  expect_warning(
    result <- validate_agent_name(reserved_name, "test.md"),
    "reserved"
  )
  expect_false(result)
})

test_that("validate_agent_name() rejects invalid characters", {
  invalid_names <- c(
    "123invalid",
    "has-dash",
    "has space",
    "has.dot",
    "has@symbol"
  )

  for (name in invalid_names) {
    expect_warning(
      result <- validate_agent_name(name, "test.md"),
      "Invalid agent name"
    )
    expect_false(result)
  }
})

test_that("validate_agent_name() warning messages include path", {
  expect_warning(
    validate_agent_name(NULL, "/path/to/agent.md"),
    "/path/to/agent.md"
  )

  expect_warning(
    validate_agent_name(names(.btw_tools)[1], "/path/to/agent.md"),
    "/path/to/agent.md"
  )

  expect_warning(
    validate_agent_name("123invalid", "/path/to/agent.md"),
    "/path/to/agent.md"
  )
})

# Test file parsing -----------------------------------------------------------

test_that("read_agent_md_file() returns NULL for non-existent file", {
  result <- read_agent_md_file("/nonexistent/path/agent.md")

  expect_null(result)
})

test_that("read_agent_md_file() parses YAML frontmatter", {
  tmp_dir <- withr::local_tempdir()
  path <- create_test_agent_file(tmp_dir, "parser_test")

  config <- read_agent_md_file(path)

  expect_type(config, "list")
  expect_equal(config$name, "parser_test")
  expect_equal(config$description, "A test agent")
  expect_equal(config$title, "Test Agent")
  # YAML parses tools as character vector, not list
  expect_equal(config$tools, "files")
})

test_that("read_agent_md_file() extracts body as system_prompt", {
  tmp_dir <- withr::local_tempdir()
  path <- create_test_agent_file(tmp_dir, "body_test")

  config <- read_agent_md_file(path)

  expect_true("system_prompt" %in% names(config))
  expect_match(config$system_prompt, "This is the system prompt")
})

test_that("read_agent_md_file() handles files without YAML", {
  tmp_dir <- withr::local_tempdir()
  path <- file.path(tmp_dir, "agent-no-yaml.md")
  writeLines("Just body content, no frontmatter.", path)

  # This should still work but return minimal config
  config <- read_agent_md_file(path)

  expect_type(config, "list")
  expect_true("system_prompt" %in% names(config))
})

test_that("read_agent_md_file() handles various YAML fields", {
  tmp_dir <- withr::local_tempdir()
  content <- "---
name: complex_agent
description: A complex agent
title: Complex Agent
icon: robot
client: anthropic/claude-sonnet-4-20250514
tools:
  - files
  - docs
---

Complex system prompt with multiple lines.
And more content here."

  path <- file.path(tmp_dir, "agent-complex.md")
  writeLines(content, path)

  config <- read_agent_md_file(path)

  expect_equal(config$name, "complex_agent")
  expect_equal(config$description, "A complex agent")
  expect_equal(config$title, "Complex Agent")
  expect_equal(config$icon, "robot")
  expect_equal(config$client, "anthropic/claude-sonnet-4-20250514")
  expect_length(config$tools, 2)
  expect_match(config$system_prompt, "Complex system prompt")
})

# Test tool creation ----------------------------------------------------------
# Note: btw_agent_tool() returns raw tools that get wrapped with _intent
# argument later by as_ellmer_tools(). These tests check the unwrapped tools.

test_that("btw_agent_tool() returns NULL for invalid name", {
  tmp_dir <- withr::local_tempdir()
  agent_file <- file.path(tmp_dir, "agent-invalid.md")
  writeLines(
    "---\nname: 123invalid\ndescription: Test\n---\nPrompt.",
    agent_file
  )

  expect_warning(
    result <- btw_agent_tool(agent_file),
    "Invalid agent name"
  )
  expect_null(result)
})

test_that("btw_agent_tool() returns NULL for reserved name", {
  tmp_dir <- withr::local_tempdir()
  reserved_name <- names(.btw_tools)[1]
  agent_file <- file.path(tmp_dir, sprintf("agent-%s.md", reserved_name))
  writeLines(
    sprintf("---\nname: %s\ndescription: Test\n---\nPrompt.", reserved_name),
    agent_file
  )

  expect_warning(
    result <- btw_agent_tool(agent_file),
    "reserved"
  )
  expect_null(result)
})

test_that("btw_agent_tool() errors for non-existent file", {
  expect_error(
    btw_agent_tool("/nonexistent/path/agent-test.md"),
    "Agent file not found"
  )
})

# Test integration with btw_tools() -------------------------------------------
# These tests check the full integration through btw_tools() which applies
# all the necessary wrapping including _intent argument.

test_that("custom agents can be discovered and loaded", {
  skip_if_not_installed("ellmer")

  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  create_test_agent_file(btw_dir, "integration_test")

  # Clear cache

  # Get tools from that directory
  tools <- withr::with_dir(tmp_dir, get_custom_agent_tools())

  expect_type(tools, "list")
  expect_true("btw_tool_agent_integration_test" %in% names(tools))

  tool_def <- tools[["btw_tool_agent_integration_test"]]
  expect_equal(tool_def$name, "btw_tool_agent_integration_test")
  expect_equal(tool_def$group, "agent")
  expect_type(tool_def$tool, "closure")

  # Calling tool() should return a tool object (before wrapping)
  tool <- tool_def$tool()
  expect_equal(tool@name, "btw_tool_agent_integration_test")
  expect_equal(tool@description, "A test agent")
})

test_that("get_custom_agent_tools() returns empty list when no agents", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Clear cache

  tools <- withr::with_dir(tmp_dir, get_custom_agent_tools())

  expect_length(tools, 0)
})

test_that("get_custom_agent_tools() skips files with invalid names", {
  skip_if_not_installed("ellmer")

  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create valid agent
  create_test_agent_file(btw_dir, "valid_agent")

  # Create agent with invalid name
  content_invalid <- "---
name: 123invalid
description: Invalid
---
Invalid agent."
  writeLines(content_invalid, file.path(btw_dir, "agent-invalid.md"))

  # Clear cache

  expect_warning(
    tools <- withr::with_dir(tmp_dir, get_custom_agent_tools()),
    "Invalid agent name"
  )

  # Should only have the valid agent
  expect_length(tools, 1)
  expect_true("btw_tool_agent_valid_agent" %in% names(tools))
})

test_that("get_custom_agent_tools() skips files with missing name", {
  skip_if_not_installed("ellmer")

  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create agent without name
  content_no_name <- "---
description: No name
---
Agent without name."
  writeLines(content_no_name, file.path(btw_dir, "agent-noname.md"))

  # Clear cache

  tools <- withr::with_dir(tmp_dir, get_custom_agent_tools())

  expect_length(tools, 0)
})

test_that("get_custom_agent_tools() warns on error loading agent", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create file with malformed YAML
  content_bad_yaml <- "---
name: bad_yaml
description: [invalid yaml structure
---
Bad YAML."
  writeLines(content_bad_yaml, file.path(btw_dir, "agent-bad.md"))

  # Clear cache

  expect_warning(
    tools <- withr::with_dir(tmp_dir, get_custom_agent_tools()),
    "Error loading custom agent"
  )
})

test_that("get_custom_agent_tools() handles multiple agents", {
  skip_if_not_installed("ellmer")

  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  create_test_agent_file(btw_dir, "agent_one")
  create_test_agent_file(btw_dir, "agent_two")
  create_test_agent_file(btw_dir, "agent_three")

  # Clear cache

  tools <- withr::with_dir(tmp_dir, get_custom_agent_tools())

  expect_length(tools, 3)
  expect_true("btw_tool_agent_agent_one" %in% names(tools))
  expect_true("btw_tool_agent_agent_two" %in% names(tools))
  expect_true("btw_tool_agent_agent_three" %in% names(tools))
})

# Test error handling ---------------------------------------------------------

test_that("validate_agent_name() includes helpful messages", {
  expect_warning(
    validate_agent_name(NULL, "test.md"),
    "Add.*name: agent_name"
  )

  expect_warning(
    validate_agent_name(names(.btw_tools)[1], "test.md"),
    "reserved"
  )

  expect_warning(
    validate_agent_name("123bad", "test.md"),
    "must be valid R identifiers"
  )

  expect_warning(
    validate_agent_name("has-dash", "test.md"),
    "must start with a letter"
  )
})

test_that("btw_agent_tool() returns valid tool for valid config", {
  tmp_dir <- withr::local_tempdir()
  agent_file <- file.path(tmp_dir, "agent-config_test.md")
  writeLines(
    c(
      "---",
      "name: config_test",
      "description: A config test agent",
      "tools:",
      "  - files",
      "  - docs",
      "---",
      "Test prompt"
    ),
    agent_file
  )

  tool <- btw_agent_tool(agent_file)

  # The tool should be created (not NULL)
  expect_false(is.null(tool))
  # Check basic properties
  expect_equal(tool@name, "btw_tool_agent_config_test")
  expect_equal(tool@description, "A config test agent")
})

# Test agent name variations --------------------------------------------------

test_that("validate_agent_name() handles various valid patterns", {
  valid_patterns <- c(
    "a", # Single letter
    "A", # Capital letter
    "agent_123", # With numbers
    "AgentName", # CamelCase
    "agent_name_v2", # Multiple underscores
    "MyAgent123" # Mixed
  )

  for (name in valid_patterns) {
    expect_true(validate_agent_name(name, "test.md"), info = name)
  }
})

test_that("validate_agent_name() rejects edge cases", {
  invalid_cases <- c(
    "_agent", # Starts with underscore
    "1agent", # Starts with number
    "agent-name", # Contains dash
    "agent name", # Contains space
    "agent.name", # Contains dot
    "agent$name", # Contains special char
    "" # Empty string
  )

  for (name in invalid_cases) {
    expect_warning(
      result <- validate_agent_name(name, "test.md"),
      info = name
    )
    expect_false(result, info = name)
  }
})

# Test tool closure and configuration -----------------------------------------

test_that("btw_tool_agent_custom_config() creates proper closure", {
  agent_config <- list(
    name = "closure_test",
    client = NULL,
    tools = c("files"),
    system_prompt = "Test",
    tools_default = NULL,
    tools_allowed = NULL
  )

  tool_fn <- btw_tool_agent_custom_config(agent_config)

  expect_type(tool_fn, "closure")

  # Function should have correct parameters (without _intent, which is added by wrapper)
  fn_args <- names(formals(tool_fn))
  expect_true("prompt" %in% fn_args)
  expect_true("session_id" %in% fn_args)
  # _intent is NOT in the closure - it's added later by wrap_with_intent()
  expect_false("_intent" %in% fn_args)
})

# Test register_custom_agent_tools() ------------------------------------------

test_that("register_custom_agent_tools() can be called without error", {
  # This is mainly a smoke test - the function modifies .btw_tools
  # which is a global state

  # Clear cache first

  expect_no_error(register_custom_agent_tools())
})
