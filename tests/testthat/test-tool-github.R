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
  withr::local_options(btw.github.allow = fictional_endpoint)
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
  withr::local_options(btw.github.allow = fictional_endpoint)
  expect_no_error(btw_gh(fictional_endpoint))

  # Now block it - user block should take precedence over user allow
  withr::local_options(
    btw.github.allow = fictional_endpoint,
    btw.github.block = fictional_endpoint
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
    btw.github.allow = fictional_endpoint,
    btw.github.block = fictional_endpoint
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
  withr::local_options(btw.github.allow = merge_endpoint)
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
  withr::local_options(btw.github.block = issue_endpoint)
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

# Tests for btw_tool_github() --------------------------------------------------

test_that("btw_tool_github() can get an issue", {
  skip_if_not_installed("gh")

  local_posit_dev_btw_repo()
  local_mocked_gh(mock_btw_issue_37)

  result <- btw_tool_github_impl(
    'gh("/repos/{owner}/{repo}/issues/37", owner = owner, repo = repo)'
  )

  expect_s7_class(result, BtwToolResult)
  expect_type(result@value, "list")

  expect_equal(result@value$number, 37)
  expect_equal(
    result@value$html_url,
    "https://github.com/posit-dev/btw/issues/37"
  )
})

test_that("btw_tool_github() can list issues", {
  skip_if_not_installed("gh")

  local_posit_dev_btw_repo()
  local_mocked_gh(mock_btw_issues_open)

  result <- btw_tool_github_impl(
    'gh("/repos/{owner}/{repo}/issues", state = "open", owner = owner, repo = repo)'
  )

  expect_s7_class(result, BtwToolResult)
  expect_type(result@value, "list")
})

test_that("btw_tool_github() can create an issue", {
  skip_if_not_installed("gh")

  local_posit_dev_btw_repo()
  local_mocked_gh()

  result <- btw_tool_github_impl(
    '
gh("POST /repos/{owner}/{repo}/issues",
  title = "Test issue",
  body = "Test body",
  owner = owner,
  repo = repo
)
  '
  )

  expect_s7_class(result, BtwToolResult)
  expect_type(result@value, "list")
  expect_equal(result@value$endpoint, "POST /repos/{owner}/{repo}/issues")
  expect_equal(result@value$payload$title, "Test issue")
  expect_equal(result@value$payload$body, "Test body")
  expect_equal(result@value$payload$owner, "posit-dev")
  expect_equal(result@value$payload$repo, "btw")
})

test_that("btw_tool_github() can get a pull request", {
  skip_if_not_installed("gh")

  local_posit_dev_btw_repo()
  local_mocked_gh()

  result <- btw_tool_github_impl(
    'gh("/repos/{owner}/{repo}/pulls/456", owner = owner, repo = repo)'
  )

  expect_s7_class(result, BtwToolResult)
  expect_type(result@value, "list")
  expect_equal(result@value$endpoint, "/repos/{owner}/{repo}/pulls/456")
  expect_equal(result@value$payload$owner, "posit-dev")
  expect_equal(result@value$payload$repo, "btw")
})

test_that("btw_tool_github() can get pull request files", {
  skip_if_not_installed("gh")

  local_posit_dev_btw_repo()
  local_mocked_gh()

  result <- btw_tool_github_impl(
    'gh("/repos/{owner}/{repo}/pulls/456/files", owner = owner, repo = repo)'
  )

  expect_s7_class(result, BtwToolResult)
  expect_type(result@value, "list")
  expect_equal(result@value$endpoint, "/repos/{owner}/{repo}/pulls/456/files")
  expect_equal(result@value$payload$owner, "posit-dev")
  expect_equal(result@value$payload$repo, "btw")
})

test_that("btw_tool_github() can call gh_whoami", {
  skip_if_not_installed("gh")

  local_posit_dev_btw_repo()
  local_mocked_bindings(
    gh_whoami = function() {
      structure(
        list(
          name = "Garrick Aden-Buie",
          login = "gadenbuie",
          html_url = "https://github.com/gadenbuie",
          scopes = "admin:org, gist, notifications, project, read:discussion, repo, user, workflow",
          token = "ghp_TOKEN_VALUE"
        ),
        class = c("gh_response", "list")
      )
    },
    .package = "gh"
  )

  result <- btw_tool_github_impl('gh_whoami()')

  expect_s7_class(result, BtwToolResult)
  expect_type(result@value, "list")
  expect_equal(result@value$login, "gadenbuie")
})

# Tool registration tests ------------------------------------------------------

test_that("github tool registers correctly", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    btw_can_register_gh_tool = function() TRUE
  )

  tools <- btw_tools("github")

  expect_true(length(tools) > 0)

  tool_names <- vapply(tools, function(t) t@name, character(1))

  expect_in("btw_tool_github", tool_names)
})

test_that("github tool requires gh package for registration", {
  local_mocked_bindings(
    btw_can_register_gh_tool = function() FALSE
  )

  tools <- btw_tools("github")

  # Should return empty list when gh is not installed or not authenticated
  expect_equal(length(tools), 0)
})

test_that("github tool has correct annotations", {
  skip_if_not_installed("gh")

  local_mocked_bindings(
    btw_can_register_gh_tool = function() TRUE
  )

  tools <- btw_tools("github")

  tool <- tools[[which(vapply(
    tools,
    function(t) t@name == "btw_tool_github",
    logical(1)
  ))]]

  expect_false(tool@annotations$read_only_hint)
  expect_true(tool@annotations$open_world_hint)
})
