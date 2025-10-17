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

# Tests for btw_gh() endpoint validation ---------------------------------------

test_that("btw_github_parse_endpoint() parses endpoints correctly", {
  # With verb
  expect_equal(
    btw_github_parse_endpoint("GET /repos/owner/repo"),
    list(verb = "GET", path = "/repos/owner/repo")
  )

  expect_equal(
    btw_github_parse_endpoint("POST /repos/owner/repo/issues"),
    list(verb = "POST", path = "/repos/owner/repo/issues")
  )

  # Without verb (defaults to GET)
  expect_equal(
    btw_github_parse_endpoint("/repos/owner/repo"),
    list(verb = "GET", path = "/repos/owner/repo")
  )

  # Without leading slash
  expect_equal(
    btw_github_parse_endpoint("repos/owner/repo"),
    list(verb = "GET", path = "/repos/owner/repo")
  )

  # Case insensitive verb
  expect_equal(
    btw_github_parse_endpoint("get /repos/owner/repo"),
    list(verb = "GET", path = "/repos/owner/repo")
  )

  # With template variables
  expect_equal(
    btw_github_parse_endpoint(
      "GET /repos/{owner}/{repo}/issues/{issue_number}"
    ),
    list(verb = "GET", path = "/repos/{owner}/{repo}/issues/{issue_number}")
  )
})

test_that("btw_github_match_pattern() matches patterns correctly", {
  # Exact matches
  expect_true(btw_github_match_pattern(
    "GET /repos/owner/repo",
    "GET /repos/*/*"
  ))

  # Template variables
  expect_true(btw_github_match_pattern(
    "GET /repos/{owner}/{repo}/issues/{issue_number}",
    "GET /repos/*/*/issues/*"
  ))

  # Literal values
  expect_true(btw_github_match_pattern(
    "GET /repos/posit-dev/btw/issues/123",
    "GET /repos/*/*/issues/*"
  ))

  # Mixed template and literal
  expect_true(btw_github_match_pattern(
    "GET /repos/posit-dev/btw/issues/{issue_number}",
    "GET /repos/*/*/issues/*"
  ))

  # Double star matches zero segments
  expect_true(btw_github_match_pattern(
    "GET /repos/owner/repo/commits",
    "GET /repos/*/*/commits/**"
  ))

  # Double star matches one segment
  expect_true(btw_github_match_pattern(
    "GET /repos/owner/repo/commits/abc123",
    "GET /repos/*/*/commits/**"
  ))

  # Double star matches multiple segments
  expect_true(btw_github_match_pattern(
    "GET /repos/owner/repo/commits/abc123/status",
    "GET /repos/*/*/commits/**"
  ))

  # Verb mismatch
  expect_false(btw_github_match_pattern(
    "POST /repos/owner/repo",
    "GET /repos/*/*"
  ))

  # Path segment count mismatch (without **)
  expect_false(btw_github_match_pattern(
    "GET /repos/owner/repo/issues",
    "GET /repos/*/*"
  ))

  # Path segment mismatch
  expect_false(btw_github_match_pattern(
    "GET /repos/owner/repo/pulls",
    "GET /repos/*/*/issues"
  ))

  # Double star doesn't match if prefix doesn't match
  expect_false(btw_github_match_pattern(
    "GET /repos/owner/repo/issues/123",
    "GET /repos/*/*/commits/**"
  ))
})

test_that("btw_gh() allows safe read operations on issues and PRs", {
  local_mocked_bindings(
    gh = function(...) list(status = "ok"),
    .package = "gh"
  )

  # Reading issues and PRs should be safe
  expect_no_error(btw_gh("GET /repos/owner/repo/issues/123"))
  expect_no_error(btw_gh("/repos/owner/repo/issues/123"))
  expect_no_error(btw_gh("GET /repos/owner/repo/issues/123/comments"))
  expect_no_error(btw_gh("GET /repos/owner/repo/pulls/456"))
  expect_no_error(btw_gh("GET /repos/owner/repo/pulls/456/files"))
})

test_that("btw_gh() allows creating issues, PRs, and comments", {
  local_mocked_bindings(
    gh = function(...) list(status = "ok"),
    .package = "gh"
  )

  # Creating issues and PRs should be allowed
  expect_no_error(btw_gh("POST /repos/owner/repo/issues"))
  expect_no_error(btw_gh("POST /repos/owner/repo/pulls"))
  expect_no_error(btw_gh("POST /repos/owner/repo/issues/123/comments"))
})

test_that("btw_gh() blocks dangerous operations", {
  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )

  # These operations should always be blocked as too dangerous
  expect_error(
    btw_gh("PUT /repos/owner/repo/pulls/123/merge"),
    "blocked"
  )

  expect_error(
    btw_gh("DELETE /repos/owner/repo"),
    "blocked"
  )

  expect_error(
    btw_gh("POST /repos/owner/repo/hooks"),
    "blocked"
  )

  expect_error(
    btw_gh("PUT /repos/owner/repo/actions/secrets/MY_SECRET"),
    "blocked"
  )
})

test_that("btw_gh() uses allowlist - unknown endpoints rejected by default", {
  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )

  # Endpoints not explicitly allowed should be rejected
  # Using clearly fictional/unlikely endpoints to avoid coupling to specific rules
  expect_error(
    btw_gh("GET /repos/owner/repo/xyz-nonexistent-endpoint"),
    "not allowed"
  )

  expect_error(
    btw_gh("POST /repos/owner/repo/xyz-made-up-action"),
    "not allowed"
  )
})

test_that("btw_gh() respects user allow rules", {
  local_mocked_bindings(
    gh = function(...) list(status = "ok"),
    .package = "gh"
  )

  # Use a fictional endpoint that definitely isn't in default rules
  fictional_endpoint <- "GET /repos/owner/repo/xyz-custom-endpoint"

  # Without user allow rule, should fail
  expect_error(
    btw_gh(fictional_endpoint),
    "not allowed"
  )

  # With user allow rule, should pass
  withr::local_options(btw.github_endpoint.allow = fictional_endpoint)
  expect_no_error(btw_gh(fictional_endpoint))
})

test_that("btw_gh() respects user block rules", {
  local_mocked_bindings(
    gh = function(...) list(status = "ok"),
    .package = "gh"
  )

  # Use a fictional endpoint to test user blocking
  fictional_endpoint <- "GET /repos/owner/repo/xyz-test-endpoint"

  # First allow it via user rules
  withr::local_options(btw.github_endpoint.allow = fictional_endpoint)
  expect_no_error(btw_gh(fictional_endpoint))

  # Now block it - user block should take precedence over user allow
  withr::local_options(
    btw.github_endpoint.allow = fictional_endpoint,
    btw.github_endpoint.block = fictional_endpoint
  )
  expect_error(
    btw_gh(fictional_endpoint),
    "blocked"
  )
})

test_that("btw_gh() user block rules take precedence over user allow rules", {
  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )

  fictional_endpoint <- "GET /repos/owner/repo/xyz-conflict-test"

  # Add both allow and block rules for the same endpoint
  withr::local_options(
    btw.github_endpoint.allow = fictional_endpoint,
    btw.github_endpoint.block = fictional_endpoint
  )

  # User block should take precedence over user allow
  expect_error(
    btw_gh(fictional_endpoint),
    "blocked"
  )
})

test_that("btw_gh() user allow rules can override built-in block rules", {
  local_mocked_bindings(
    gh = function(...) list(status = "ok"),
    .package = "gh"
  )

  # Merge is blocked by default
  merge_endpoint <- "PUT /repos/owner/repo/pulls/123/merge"

  # Should be blocked by default
  expect_error(
    btw_gh(merge_endpoint),
    "blocked"
  )

  # User allow should override built-in block
  withr::local_options(btw.github_endpoint.allow = merge_endpoint)
  expect_no_error(btw_gh(merge_endpoint))
})

test_that("btw_gh() user block rules can override built-in allow rules", {
  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )

  # Reading issues is allowed by default
  issue_endpoint <- "GET /repos/owner/repo/issues/123"

  # Should be allowed by default
  local_mocked_bindings(
    gh = function(...) list(status = "ok"),
    .package = "gh"
  )
  expect_no_error(btw_gh(issue_endpoint))

  # User block should override built-in allow
  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )
  withr::local_options(btw.github_endpoint.block = issue_endpoint)
  expect_error(
    btw_gh(issue_endpoint),
    "blocked"
  )
})

test_that("btw_gh() passes arguments through to gh::gh()", {
  local_mocked_bindings(
    gh = function(endpoint, ..., owner, repo) {
      list(
        endpoint = endpoint,
        owner = owner,
        repo = repo,
        dots = list(...)
      )
    },
    .package = "gh"
  )

  result <- btw_gh(
    "/repos/{owner}/{repo}/issues",
    owner = "posit-dev",
    repo = "btw",
    state = "open"
  )

  expect_equal(result$endpoint, "/repos/{owner}/{repo}/issues")
  expect_equal(result$owner, "posit-dev")
  expect_equal(result$repo, "btw")
  expect_equal(result$dots$state, "open")
})

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

  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )

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

test_that("btw_tool_github_comment_add() validates arguments", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    gh = function(...) stop("Should not reach API"),
    .package = "gh"
  )

  expect_error(
    btw_tool_github_comment_add(
      number = -1,
      body = "test",
      owner = "posit-dev",
      repo = "btw"
    ),
    "min"
  )

  expect_error(
    btw_tool_github_comment_add(
      number = 1.5,
      body = "test",
      owner = "posit-dev",
      repo = "btw"
    ),
    "whole"
  )

  expect_error(
    btw_tool_github_comment_add(
      number = 1,
      body = 123,
      owner = "posit-dev",
      repo = "btw"
    ),
    "string"
  )
})

# Tool registration tests ------------------------------------------------------

test_that("github tools register correctly", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    btw_can_register_gh_tool = function() TRUE
  )

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
  expect_in("btw_tool_github_comment_add", tool_names)
})

test_that("github tools require gh package for registration", {
  local_mocked_bindings(
    btw_can_register_gh_tool = function() FALSE
  )

  tools <- btw_tools("github")

  # Should return empty list when gh is not installed or not authenticated
  expect_equal(length(tools), 0)
})

test_that("github tools have correct annotations", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    btw_can_register_gh_tool = function() TRUE
  )

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
    "btw_tool_github_pull_request_create",
    "btw_tool_github_comment_add"
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
