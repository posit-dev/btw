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

  local_test_agent_file(btw_dir, "integration_test")

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

  tools <- withr::with_dir(tmp_dir, get_custom_agent_tools())

  expect_length(tools, 0)
})

test_that("get_custom_agent_tools() skips files with invalid names", {
  skip_if_not_installed("ellmer")

  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create valid agent
  local_test_agent_file(btw_dir, "valid_agent")

  # Create agent with invalid name
  content_invalid <- "---
name: 123invalid
description: Invalid
---
Invalid agent."
  writeLines(content_invalid, file.path(btw_dir, "agent-invalid.md"))

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

  # Should warn about missing name
  expect_warning(
    tools <- withr::with_dir(tmp_dir, get_custom_agent_tools()),
    "Agent file has no name"
  )

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

  local_test_agent_file(btw_dir, "agent_one")
  local_test_agent_file(btw_dir, "agent_two")
  local_test_agent_file(btw_dir, "agent_three")

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

# Internal closure structure is an implementation detail.
# Tool behavior is tested through integration tests below.

# Test register_custom_agent_tools() ------------------------------------------

test_that("register_custom_agent_tools() can be called without error", {
  # This is mainly a smoke test - the function modifies .btw_tools
  # which is a global state

  # Clear cache first

  expect_no_error(register_custom_agent_tools())
})

# ---- Custom Agent Configuration (Behavioral) --------------------------------

test_that("btw_custom_agent_client_config creates chat with custom system prompt", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create agent file
  writeLines(
    c(
      "---",
      "name: code_reviewer",
      "description: Expert code reviewer",
      "tools:",
      "  - files",
      "---",
      "",
      "You are an expert code reviewer. Focus on:",
      "- Code quality and best practices",
      "- Performance issues",
      "- Security vulnerabilities"
    ),
    file.path(btw_dir, "agent-code-reviewer.md")
  )

  # Load config and create chat
  agent_config <- withr::with_dir(tmp_dir, {
    read_agent_md_file(file.path(btw_dir, "agent-code-reviewer.md"))
  })

  chat <- btw_custom_agent_client_config(agent_config)

  expect_true(inherits(chat, "Chat"))

  # Verify system prompt
  system_prompt <- chat$get_system_prompt()
  expect_match(system_prompt, "expert code reviewer", ignore.case = TRUE)
  expect_match(system_prompt, "Code quality", ignore.case = TRUE)

  # Verify tools
  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_files_", tool_names)))
  expect_false(any(grepl("^btw_tool_docs_", tool_names)))
})

test_that("btw_custom_agent_client_config respects tool restrictions", {
  agent_config <- list(
    name = "docs_agent",
    description = "Documentation expert",
    tools = "docs",
    system_prompt = "You help with documentation.",
    tools_default = NULL,
    tools_allowed = NULL,
    client = NULL
  )

  chat <- btw_custom_agent_client_config(agent_config)

  tool_names <- map_chr(chat$get_tools(), function(t) t@name)
  expect_true(all(grepl("^btw_tool_docs_", tool_names)))
  expect_false(any(grepl("^btw_tool_files_", tool_names)))
})

test_that("btw_custom_agent_client_config concatenates system prompts", {
  agent_config <- list(
    name = "test",
    client = NULL,
    tools = "files",
    system_prompt = "Custom instructions for this agent.",
    tools_default = NULL,
    tools_allowed = NULL
  )

  chat <- btw_custom_agent_client_config(agent_config)
  system_prompt <- chat$get_system_prompt()

  # Should include base prompt
  expect_match(system_prompt, "Task Execution", ignore.case = TRUE)
  # Should include custom prompt
  expect_match(system_prompt, "Custom instructions", ignore.case = TRUE)
  # Should have separator
  expect_match(system_prompt, "---")
})

test_that("btw_custom_agent_client_config uses btw_agent_resolve_client", {
  # Test explicit client
  custom_client <- ellmer::chat_anthropic(model = "claude-opus-4-20241120")
  agent_config <- list(
    name = "test",
    client = custom_client,
    tools = "files",
    system_prompt = "Test"
  )

  chat <- btw_custom_agent_client_config(agent_config)
  expect_identical(chat, custom_client)

  # Test option fallback
  withr::local_options(
    btw.subagent.client = "anthropic/claude-sonnet-4-20250514"
  )
  agent_config$client <- NULL

  chat2 <- btw_custom_agent_client_config(agent_config)
  expect_equal(chat2$get_model(), "claude-sonnet-4-20250514")
})

# ---- Multiple Custom Agents -------------------------------------------------

test_that("multiple custom agents can be discovered and registered", {
  tmp_dir <- withr::local_tempdir()
  btw_dir <- file.path(tmp_dir, ".btw")
  dir.create(btw_dir)

  # Create two agents
  local_test_agent_file(btw_dir, "agent_one")
  local_test_agent_file(btw_dir, "agent_two")

  # Use get_custom_agent_tools() to get internal btw tool structure
  tools <- withr::with_dir(tmp_dir, get_custom_agent_tools())

  expect_type(tools, "list")
  expect_true("btw_tool_agent_agent_one" %in% names(tools))
  expect_true("btw_tool_agent_agent_two" %in% names(tools))

  # Verify they have correct structure
  agent_one_def <- tools[["btw_tool_agent_agent_one"]]
  agent_two_def <- tools[["btw_tool_agent_agent_two"]]

  expect_equal(agent_one_def$name, "btw_tool_agent_agent_one")
  expect_equal(agent_one_def$group, "agent")
  expect_type(agent_one_def$tool, "closure")

  expect_equal(agent_two_def$name, "btw_tool_agent_agent_two")
  expect_equal(agent_two_def$group, "agent")
  expect_type(agent_two_def$tool, "closure")
})
