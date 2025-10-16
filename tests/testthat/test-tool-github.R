# Helper to scrub dynamic GitHub content for snapshots
scrub_github_content <- function(x) {
  # Scrub timestamps
  x <- gsub("\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}Z", "TIMESTAMP", x)
  # Scrub GitHub URLs with actual domains
  x <- gsub("https://github\\.com/[^\\s]+", "GITHUB_URL", x)
  # Scrub GitHub API URLs
  x <- gsub("https://api\\.github\\.com/[^\\s]+", "API_URL", x)
  x
}

# Tests for posit-dev/btw repository -------------------------------------------
# Using issue #1 and PR #1 as stable test cases

test_that("btw_tool_github_issue_get() works", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_issue_get(
    issue_number = 1,
    owner = "posit-dev",
    repo = "btw"
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Issue #1")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_issue_thread() works", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_issue_thread(
    issue_number = 1,
    owner = "posit-dev",
    repo = "btw"
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Issue #1")
  # Issue #1 should have comments
  expect_match(result@value, "Comments:")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_pull_request_get() works", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_pull_request_get(
    pull_number = 1,
    owner = "posit-dev",
    repo = "btw"
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Pull Request #1")
  expect_match(result@value, "Base:")
  expect_match(result@value, "Head:")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_pull_request_diff() works", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_pull_request_diff(
    pull_number = 1,
    owner = "posit-dev",
    repo = "btw"
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Pull Request #1 File Changes")
  expect_match(result@value, "Total Files Changed:")
  # Should contain diff markers
  expect_true(grepl("```diff", result@value, fixed = TRUE))
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_issues_list() works", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_issues_list(
    owner = "posit-dev",
    repo = "btw",
    state = "closed",
    max = 5
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Issues in posit-dev/btw")
  expect_match(result@value, "Total:")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_issues_list() handles no results", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_issues_list(
    owner = "posit-dev",
    repo = "btw",
    state = "open",
    labels = c("nonexistent-label-xyz123")
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "No issues found")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_pull_requests_list() works", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_pull_requests_list(
    owner = "posit-dev",
    repo = "btw",
    state = "closed",
    max = 5
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Pull Requests in posit-dev/btw")
  expect_match(result@value, "Total:")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

test_that("btw_tool_github_pull_requests_list() handles no results", {
  skip_if_offline()
  skip_if_not_installed("gh")

  result <- btw_tool_github_pull_requests_list(
    owner = "posit-dev",
    repo = "btw",
    state = "open",
    author = "nonexistent-user-xyz123"
  )

  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "No pull requests found")
  expect_snapshot(
    cli::cat_line(result@value),
    transform = scrub_github_content
  )
})

# Error handling tests ---------------------------------------------------------

test_that("get_github_repo() errors when no repo detected", {
  skip_if_not_installed("gh")

  # Mock gh_tree_remote to return NULL
  local_mocked_bindings(
    gh_tree_remote = function() stop("Not a git repo"),
    .package = "gh"
  )

  expect_snapshot(error = TRUE, {
    get_github_repo()
  })
})

test_that("btw_tool_github_issue_get() errors with invalid issue number", {
  skip_if_not_installed("gh")

  expect_error(
    btw_tool_github_issue_get(
      issue_number = -1,
      owner = "posit-dev",
      repo = "btw"
    ),
    "min"
  )

  expect_error(
    btw_tool_github_issue_get(
      issue_number = 1.5,
      owner = "posit-dev",
      repo = "btw"
    ),
    "whole"
  )
})

test_that("btw_tool_github_issue_get() requires gh package", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    is_installed = function(pkg) pkg != "gh"
  )

  expect_error(
    btw_tool_github_issue_get(
      issue_number = 1,
      owner = "posit-dev",
      repo = "btw"
    ),
    "gh"
  )
})

test_that("btw_tool_github_pull_request_get() requires gh package", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    is_installed = function(pkg) pkg != "gh"
  )

  expect_error(
    btw_tool_github_pull_request_get(
      pull_number = 1,
      owner = "posit-dev",
      repo = "btw"
    ),
    "gh"
  )
})

test_that("github list tools validate arguments", {
  skip_if_not_installed("gh")

  # Invalid state
  expect_error(
    btw_tool_github_issues_list(
      owner = "posit-dev",
      repo = "btw",
      state = "invalid"
    )
  )

  expect_error(
    btw_tool_github_pull_requests_list(
      owner = "posit-dev",
      repo = "btw",
      state = "invalid"
    )
  )

  # Invalid max
  expect_error(
    btw_tool_github_issues_list(
      owner = "posit-dev",
      repo = "btw",
      max = 0
    ),
    "min"
  )

  expect_error(
    btw_tool_github_issues_list(
      owner = "posit-dev",
      repo = "btw",
      max = 1.5
    ),
    "whole"
  )
})

test_that("btw_tool_github_issue_create() validates arguments", {
  skip_if_not_installed("gh")

  expect_error(
    btw_tool_github_issue_create(
      title = 123,
      body = "test",
      owner = "posit-dev",
      repo = "btw"
    ),
    "string"
  )

  expect_error(
    btw_tool_github_issue_create(
      title = "test",
      body = 123,
      owner = "posit-dev",
      repo = "btw"
    ),
    "string"
  )

  expect_error(
    btw_tool_github_issue_create(
      title = "test",
      body = "test",
      owner = "posit-dev",
      repo = "btw",
      labels = 123
    ),
    "character"
  )
})

test_that("btw_tool_github_pull_request_create() validates arguments", {
  skip_if_not_installed("gh")

  expect_error(
    btw_tool_github_pull_request_create(
      title = 123,
      body = "test",
      head = "feat",
      base = "main",
      owner = "posit-dev",
      repo = "btw"
    ),
    "string"
  )

  expect_error(
    btw_tool_github_pull_request_create(
      title = "test",
      body = "test",
      head = 123,
      base = "main",
      owner = "posit-dev",
      repo = "btw"
    ),
    "string"
  )

  expect_error(
    btw_tool_github_pull_request_create(
      title = "test",
      body = "test",
      head = "feat",
      base = 123,
      owner = "posit-dev",
      repo = "btw"
    ),
    "string"
  )
})

# Tool registration tests ------------------------------------------------------

test_that("github tools register correctly", {
  skip_if_not_installed("gh")

  tools <- btw_tools("github")

  expect_true(length(tools) > 0)

  tool_names <- vapply(tools, function(t) t@name, character(1))

  expect_in("btw_tool_github_issue_get", tool_names)
  expect_in("btw_tool_github_issue_thread", tool_names)
  expect_in("btw_tool_github_pull_request_get", tool_names)
  expect_in("btw_tool_github_pull_request_diff", tool_names)
  expect_in("btw_tool_github_issue_create", tool_names)
  expect_in("btw_tool_github_pull_request_create", tool_names)
  expect_in("btw_tool_github_issues_list", tool_names)
  expect_in("btw_tool_github_pull_requests_list", tool_names)
})

test_that("github tools require gh package for registration", {
  local_mocked_bindings(
    is_installed = function(pkg) pkg != "gh"
  )

  tools <- btw_tools("github")

  # Should return empty list when gh is not installed
  expect_equal(length(tools), 0)
})

test_that("github tools have correct annotations", {
  skip_if_not_installed("gh")

  tools <- btw_tools("github")

  # Check read-only tools
  read_only_tools <- c(
    "btw_tool_github_issue_get",
    "btw_tool_github_issue_thread",
    "btw_tool_github_pull_request_get",
    "btw_tool_github_pull_request_diff",
    "btw_tool_github_issues_list",
    "btw_tool_github_pull_requests_list"
  )

  for (tool_name in read_only_tools) {
    tool <- tools[[which(vapply(
      tools,
      function(t) t@name == tool_name,
      logical(1)
    ))]]
    expect_true(
      tool@annotations$read_only_hint,
      info = sprintf("%s should be read-only", tool_name)
    )
    expect_true(
      tool@annotations$open_world_hint,
      info = sprintf("%s should be open-world", tool_name)
    )
  }

  # Check write tools
  write_tools <- c(
    "btw_tool_github_issue_create",
    "btw_tool_github_pull_request_create"
  )

  for (tool_name in write_tools) {
    tool <- tools[[which(vapply(
      tools,
      function(t) t@name == tool_name,
      logical(1)
    ))]]
    expect_false(
      tool@annotations$read_only_hint,
      info = sprintf("%s should not be read-only", tool_name)
    )
    expect_true(
      tool@annotations$open_world_hint,
      info = sprintf("%s should be open-world", tool_name)
    )
  }
})
