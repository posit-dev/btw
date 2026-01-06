#' Create a test agent file with automatic cleanup
#'
#' Creates a valid agent-*.md file in the specified directory. The file is
#' automatically cleaned up when the calling test completes.
#'
#' @param dir Directory to create the agent file in (typically a .btw directory)
#' @param name Name of the agent (without 'agent-' prefix or '.md' suffix)
#' @param content Optional custom content. If NULL, creates a standard test agent.
#' @param .envir Environment to use for cleanup (typically parent.frame())
#'
#' @return Path to the created agent file
#' @noRd
local_test_agent_file <- function(
  dir = ".",
  name = "test_agent",
  content = NULL,
  .envir = parent.frame()
) {
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

  path <- fs::path_norm(file.path(dir, sprintf("agent-%s.md", name)))
  writeLines(content, path)

  withr::defer(
    if (file.exists(path)) unlink(path),
    envir = .envir
  )

  path
}
