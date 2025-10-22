#' @include tool-result.R
NULL

# GitHub API Wrapper with Endpoint Validation ---------------------------------

btw_gh <- function(endpoint, ...) {
  endpoint <- btw_github_check_endpoint(endpoint)
  gh::gh(endpoint, ...)
}

btw_eval_gh_code <- function(code, fields = btw_gh_fields()) {
  check_installed("gh")

  repo_info <- get_github_repo(NULL, NULL)

  res <- eval_limited_r_code(
    code,
    gh = btw_gh,
    gh_whoami = gh::gh_whoami,
    .parent_env = repo_info,
    .error_extra = "Only unprefixed {.fn gh} and {.fn gh_whoami} from the {.pkg gh} package are allowed.",
    .error_eval = "Error evaluating GitHub code."
  )

  if (!is.null(fields)) {
    res <- btw_gh_filter_fields(res, fields = fields)
  }

  res
}

# Helper: Check if GitHub tools can register ----------------------------------

btw_can_register_gh_tool <- local({
  gh_auth_result <- NULL

  function() {
    if (!is_installed("gh")) {
      warn(
        "Install the {gh} package to enable GitHub tools.",
        .frequency = "once",
        .frequency_id = "btw_github_tools_missing_gh"
      )
      return(FALSE)
    }

    if (!is.null(gh_auth_result)) {
      return(gh_auth_result)
    }

    gh_auth_result <<- tryCatch(
      {
        whoami <- gh::gh_whoami()
        !is.null(whoami)
      },
      error = function(e) {
        FALSE
      }
    )

    if (!gh_auth_result) {
      warn(
        c(
          "GitHub tools are not available because you are not authenticated with the gh package.",
          i = "Run `gh::gh_whoami()` to check your authentication status.",
          i = "Run `gitcreds::gitcreds_set()` or set the GITHUB_PAT environment variable to authenticate."
        ),
        .frequency = "once",
        .frequency_id = "btw_github_tools_not_authenticated"
      )
    }

    gh_auth_result
  }
})

# Helper: Get GitHub repo info ------------------------------------------------

get_github_repo <- function(owner = NULL, repo = NULL) {
  if (!is.null(owner) && !is.null(repo)) {
    return(list(owner = owner, repo = repo))
  }

  check_installed("gh")

  remote_info <- new_environment()
  gh_tr <- tryCatch(
    gh::gh_tree_remote(),
    error = function(e) NULL
  )

  abort_if_missing_gh_tr <- function() {
    if (is.null(gh_tr)) {
      abort(
        c(
          "Could not detect GitHub repository.",
          i = "Provide `owner` and `repo` parameters, or run from within a git repository with a GitHub remote."
        ),
        .frame = caller_env(3)
      )
    }
  }

  delayedAssign("owner", assign.env = remote_info, {
    abort_if_missing_gh_tr()
    gh_tr$username
  })

  delayedAssign("repo", assign.env = remote_info, {
    abort_if_missing_gh_tr()
    gh_tr$repo
  })

  # Returns an environment with `owner` and `repo` bindings that are evaluated
  # lazily and throw when accessed if owner and repo aren't available.
  remote_info
}

# GitHub Tool -----------------------------------------------------------------

#' Tool: GitHub
#'
#' @description
#' Execute R code that calls the GitHub API using [gh::gh()].
#'
#' This tool is
#' designed such that models can write very limited R code to call [gh::gh()]
#' and protections are inserted to prevent the model from calling unsafe or
#' destructive actions via the API. The **Endpoint Validation** section below
#' describes how API endpoints are validated to ensure safety.
#'
#' While this tool *can* execute R code, the code is evaluated in an environment
#' where only a limited set of functions and variables are available. In
#' particular, only the `gh()` and `gh_whoami()` functions from the `gh` package
#' are available, along with `owner` and `repo` variables that are pre-defined
#' to point to the current repository (if detected). This allows models to focus
#' on writing GitHub API calls without needing to load packages or manage
#' authentication.
#'
#' ## Endpoint Validation
#'
#' This tool uses endpoint validation to ensure only safe GitHub API operations
#' are performed. By default, most read operations and low-risk write operations
#' (like creating issues or PRs) are allowed, while dangerous operations (like
#' merging PRs or deleting repositories) are blocked.
#'
#' To customize which endpoints are allowed or blocked, use the
#' `btw.github.allow` and `btw.github.block` options:
#'
#' ```r
#' # Allow a specific endpoint
#' options(btw.github.allow = c(
#'   getOption("btw.github.allow"),
#'   "GET /repos/*/*/topics"
#' ))
#'
#' # Block a specific endpoint
#' options(btw.github.block = c(
#'   getOption("btw.github.block"),
#'   "GET /repos/*/*/branches"
#' ))
#' ```
#'
#' You can also set these options in your [btw.md][use_btw_md()] file under the
#' `options` field:
#'
#' ```yaml
#' tools: github
#' options:
#'   github:
#'     allow:
#'       - "PATCH /repos/*/*/pulls/*" # Allow converting PRs to/from draft
#'       - "POST /repos/*/*/git/refs" # Allow creating branches
#'     block:
#'       - "DELETE /repos/**" # Block any delete action under /repos
#' ````
#'
#' The precedence order for rules is:
#' 1. User block rules (checked first, highest priority)
#' 2. User allow rules
#' 3. Built-in block rules
#' 4. Built-in allow rules
#' 5. Default: reject (if no rules match)
#'
#' @examples
#' \dontrun{
#' # Get an issue
#' btw_tool_github(
#'   code = 'gh("/repos/{owner}/{repo}/issues/123", owner = owner, repo = repo)'
#' )
#'
#' # Create an issue
#' btw_tool_github(code = r"(
#'   gh(
#'     "POST /repos/{owner}/{repo}/issues",
#'     title = \"Bug report\",
#'     body = \"Description of bug\",
#'     owner = owner,
#'     repo = repo
#'   )
#' )")
#'
#' # Target a different repository
#' btw_tool_github(code = 'gh("/repos/tidyverse/dplyr/issues/123")')
#' }
#'
#' @param code R code that calls `gh()` or `gh_whoami()`. The code will be
#'   evaluated in an environment where `owner` and `repo` variables are
#'   predefined (defaulting to the current repository if detected). The `gh()`
#'   function is available without needing to load the gh package.
#' @param fields Optional character vector of GitHub API response fields to
#'   retain. If provided, only these fields will be included in the result.
#'   Defaults to a curated set of commonly used fields.
#' @inheritParams btw_tool_docs_package_news
#' @return A `btw_tool_result` containing the result of the GitHub API call.
#'
#' @family github tools
#' @export
btw_tool_github <- function(code, fields, `_intent`) {}

btw_tool_github_impl <- function(code, fields = "default") {
  check_string(code)
  check_character(fields, allow_null = TRUE)

  if (identical(fields, "default")) {
    fields <- btw_gh_fields()
  } else if (identical(fields, "all")) {
    fields <- NULL
  }

  result <- btw_eval_gh_code(code, fields)

  btw_tool_result(result)
}

.btw_add_to_tools(
  name = "btw_tool_github",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_impl,
      name = "btw_tool_github",
      description = r"---(Execute R code that calls the GitHub API using gh().

WHEN TO USE:
* Use this tool to interact with GitHub repositories, issues, pull requests, and more.
* Write R code that calls gh() - you don't need to load the gh package.
* The code runs in an environment with `owner` and `repo` variables already defined.
* ALWAYS show the user a preview or description before performing write operations and allow the user to provide feedback.
* ALWAYS ask the user for confirmation before performing write or delete operations.

CODE ENVIRONMENT:
* `owner` and `repo` variables are pre-defined for the current repository
* `gh()` function is available to call any GitHub API endpoint
* `gh_whoami()` is available to get current user information
* You can provide the endpoint with `owner` or `repo` values filled in to target another repo
* The last value of the code block is returned as the result
* You will not see any output from print statements or other side effects

ENDPOINT VALIDATION:
* Most read operations (GET) are allowed by default
* Low-risk write operations (creating issues/PRs, adding comments) are allowed
* Dangerous operations (merging PRs, deleting repos, managing webhooks/secrets) are blocked
* If an endpoint is blocked, the error message will explain how the user can allow it

EXAMPLES:
```r
# Get an issue from current repo
gh("/repos/{owner}/{repo}/issues/123", owner = owner, repo = repo)

# Get an issue from a fixed repo (tidyverse/dplyr)
gh("/repos/tidyverse/dplyr/issues/123")

# List open issues in current repository
gh("/repos/{owner}/{repo}/issues", state = "open", owner = owner, repo = repo, .limit = 10)

# Create an issue in the current repo
gh(
  "POST /repos/{owner}/{repo}/issues",
  title = "Bug report",
  body = "Description",
  owner = owner,
  repo = repo
)

# Get PR diff files in a specific repo (posit-dev/btw)
gh("/repos/posit-dev/btw/pulls/6/files")

# Target a different repo (option 1)
gh("/repos/{owner}/{repo}/issues/123", owner = "tidyverse", repo = "dplyr")
# Target a different repo (option 2)
gh("/repos/tidyverse/dplyr/issues/123")
```

RETURNS: The result from the GitHub API call, formatted as JSON.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub API",
        read_only_hint = FALSE, # Can perform writes
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        btw_can_register = btw_can_register_gh_tool
      ),
      arguments = list(
        code = ellmer::type_string(
          "R code that calls gh() or gh_whoami(). The code will be evaluated in an environment with owner and repo variables predefined."
        ),
        fields = ellmer::type_array(
          ellmer::type_string(),
          paste(
            "Optional character vector of GitHub API response fields to retain.",
            "If provided, only these fields will be included in the result.",
            "Use `'all'` to retain all fields.",
            "The field filter only applies to the top-level response object, or the top-level items in the response if an array is returned."
          ),
          required = FALSE
        )
      )
    )
  }
)

# Helper: GitHub Endpoint Validation ------------------------------------------

btw_github_default_allow_rules <- function() {
  c(
    # Basic Information Gathering Endpoints
    "GET /repos/*/*/issues/*",
    "GET /repos/*/*/issues/*/comments/**",
    "GET /repos/*/*/pulls/*",
    "GET /repos/*/*/pulls/*/files/**",
    "GET /repos/*/*/pulls/*/comments/**",
    "GET /repos/*/*/issues",
    "GET /repos/*/*/pulls",
    "POST /repos/*/*/issues",
    "POST /repos/*/*/pulls",
    "POST /repos/*/*/issues/*/comments",
    "POST /repos/*/*/pulls/*/comments",

    # Read-only Repository Information
    "GET /repos/*/*",
    "GET /repos/*/*/branches",
    "GET /repos/*/*/branches/*",
    "GET /repos/*/*/contents/**",
    "GET /repos/*/*/commits",
    "GET /repos/*/*/commits/*",
    "GET /repos/*/*/compare/*",

    # Read-only PR/Issue Information
    "GET /repos/*/*/pulls/*/reviews",
    "GET /repos/*/*/pulls/*/commits",
    "GET /repos/*/*/issues/*/events",
    "GET /repos/*/*/issues/events",

    # Read-only Labels and Milestones
    "GET /repos/*/*/labels",
    "GET /repos/*/*/milestones",
    "GET /repos/*/*/milestones/*",

    # Read-only Checks and Status
    "GET /repos/*/*/commits/*/status",
    "GET /repos/*/*/commits/*/check-runs",

    # Read-only Release Information
    "GET /repos/*/*/releases",
    "GET /repos/*/*/releases/*",

    # Read-only Workflow Information
    "GET /repos/*/*/actions/workflows",
    "GET /repos/*/*/actions/workflows/*",
    "GET /repos/*/*/actions/runs",
    "GET /repos/*/*/actions/runs/*",
    "GET /repos/*/*/actions/runs/*/jobs",
    "GET /repos/*/*/actions/runs/*/jobs/*/logs",

    # Search GitHub
    "GET /search/*",

    # Low-risk Write Operations
    "POST /repos/*/*/issues/*/labels",
    "DELETE /repos/*/*/issues/*/labels/*",
    "PATCH /repos/*/*/issues/*",
    "PATCH /repos/*/*/pulls/*"
  )
}

btw_github_default_block_rules <- function() {
  c(
    # Merge operations
    "PUT /repos/*/*/pulls/*/merge",
    "POST /repos/*/*/merges",

    # Repository deletion and dangerous operations
    "DELETE /repos/*/*",

    # Webhooks
    "GET /repos/*/*/hooks",
    "GET /repos/*/*/hooks/*",
    "POST /repos/*/*/hooks",
    "PATCH /repos/*/*/hooks/*",
    "DELETE /repos/*/*/hooks/*",

    # Secrets and keys
    "GET /repos/*/*/actions/secrets",
    "GET /repos/*/*/actions/secrets/*",
    "PUT /repos/*/*/actions/secrets/*",
    "DELETE /repos/*/*/actions/secrets/*",
    "GET /repos/*/*/keys",
    "GET /repos/*/*/keys/*",
    "POST /repos/*/*/keys",
    "DELETE /repos/*/*/keys/*",

    # Collaborators
    "GET /repos/*/*/collaborators",
    "GET /repos/*/*/collaborators/*",
    "PUT /repos/*/*/collaborators/*",
    "DELETE /repos/*/*/collaborators/*",

    # Repository settings
    "PATCH /repos/*/*",

    # Force operations
    "POST /repos/*/*/git/refs/*/force"
  )
}

btw_github_parse_endpoint <- function(endpoint) {
  check_string(endpoint)
  endpoint <- trimws(endpoint)

  # Check if endpoint starts with a verb
  verb_path <- strsplit(endpoint, "\\s+")[[1]]

  if (!length(verb_path) %in% 1:2) {
    cli::cli_abort(c(
      "Invalid GitHub endpoint format: {.val {endpoint}}",
      i = "Must be in the format {.code VERB /path/to/endpoint} or {.code /path/to/endpoint}"
    ))
  }

  if (length(verb_path) == 2) {
    verb <- toupper(verb_path[1])
    path <- verb_path[2]
  } else {
    verb <- "GET"
    path <- verb_path[1]
  }

  # Ensure path starts with /
  if (!grepl("^/", path)) {
    path <- paste0("/", path)
  }

  # Remove trailing /
  path <- gsub("/+", "/", path)

  list(verb = verb, path = path)
}

btw_github_match_pattern <- function(endpoint, pattern) {
  check_string(endpoint)
  check_string(pattern)

  endpoint_parsed <- btw_github_parse_endpoint(endpoint)
  pattern_parsed <- btw_github_parse_endpoint(pattern)

  # Verb must match exactly
  if (endpoint_parsed$verb != pattern_parsed$verb) {
    return(FALSE)
  }

  # Split paths into segments
  endpoint_segments <- strsplit(endpoint_parsed$path, "/")[[1]]
  pattern_segments <- strsplit(pattern_parsed$path, "/")[[1]]

  # Remove empty segments (from leading /)
  endpoint_segments <- endpoint_segments[endpoint_segments != ""]
  pattern_segments <- pattern_segments[pattern_segments != ""]

  # Check if pattern ends with **
  has_double_star <- length(pattern_segments) > 0 &&
    pattern_segments[length(pattern_segments)] == "**"

  if (has_double_star) {
    # Remove ** from pattern segments
    pattern_segments <- pattern_segments[-length(pattern_segments)]

    # Endpoint must have at least as many segments as pattern (minus **)
    if (length(endpoint_segments) < length(pattern_segments)) {
      return(FALSE)
    }

    # Match only the segments before **
    for (i in seq_along(pattern_segments)) {
      if (
        pattern_segments[i] != "*" &&
          pattern_segments[i] != endpoint_segments[i]
      ) {
        return(FALSE)
      }
    }

    return(TRUE)
  } else {
    # Without **, must have exact same number of segments
    if (length(endpoint_segments) != length(pattern_segments)) {
      return(FALSE)
    }

    # Match each segment
    for (i in seq_along(pattern_segments)) {
      if (
        pattern_segments[i] != "*" &&
          pattern_segments[i] != endpoint_segments[i]
      ) {
        return(FALSE)
      }
    }

    return(TRUE)
  }
}

btw_github_check_endpoint <- function(endpoint) {
  check_string(endpoint)

  # Get user-defined rules
  user_block <- getOption("btw.github.block", character())
  user_allow <- getOption("btw.github.allow", character())

  # 1. Check user block rules first
  for (rule in user_block) {
    if (btw_github_match_pattern(endpoint, rule)) {
      cli::cli_abort(c(
        "GitHub API endpoint is blocked: {.val {endpoint}}",
        x = "Matched user block rule: {.val {rule}}"
      ))
    }
  }

  # 2. Check user allow rules second
  for (rule in user_allow) {
    if (btw_github_match_pattern(endpoint, rule)) {
      return(invisible(endpoint))
    }
  }

  cmd_allow <- sprintf(
    "btw.github.allow = c(getOption('btw.github.allow'), '%s')",
    endpoint
  )

  # 3. Check built-in block rules third
  for (rule in btw_github_default_block_rules()) {
    if (btw_github_match_pattern(endpoint, rule)) {
      cli::cli_abort(c(
        "GitHub API endpoint is blocked: {.val {endpoint}}",
        x = "Matched btw block rule: {.val {rule}}",
        i = "To allow this endpoint anyway, add it to the {.code btw.github.allow} option.",
        i = "Ex: {.run {cmd_allow}}"
      ))
    }
  }

  # 4. Check built-in allow rules fourth
  for (rule in btw_github_default_allow_rules()) {
    if (btw_github_match_pattern(endpoint, rule)) {
      return(invisible(endpoint))
    }
  }

  # 5. No matching allow rule found - reject
  cli::cli_abort(c(
    "GitHub API endpoint not allowed: {.val {endpoint}}",
    x = "This endpoint has not been approved for use.",
    i = "To allow this endpoint, add it to the {.code btw.github.allow} option.",
    i = "Example: {.run {cmd_allow}}"
  ))
}

# Helper: GitHub Fields -------------------------------------------------------
btw_gh_filter_fields <- function(x, fields = btw_gh_fields()) {
  if (!any(nzchar(names2(x)))) {
    return(lapply(x, btw_gh_filter_fields, fields = fields))
  } else {
    x[intersect(fields, names(x))]
  }
}

btw_gh_fields <- function() {
  c(
    # Core denormalized identifiers
    "number", # issue/PR/milestone number
    "name", # generic name (branch/workflow/label)
    "title", # issue/PR/release title
    "sha", # commit SHA (commits, statuses)
    "ref", # branch/tag ref (branches, compare)
    "path", # contents path (files/dirs)

    # Identity and canonical URLs
    "id", # numeric/id-like key (where present)
    "node_id", # global GraphQL node id (often present)
    "type", # normalize record type if present (issue, pull_request, commit, etc.)
    "url", # API self URL
    "html_url", # human-readable page
    "repository", # prefer "owner/repo" when present
    "owner", # "owner" or org login if top-level exists

    # Authorship and associations
    "author", # primary author login (when available)
    "committer", # committer login (commits)
    "author_association", # OWNER, COLLABORATOR, CONTRIBUTOR, etc.

    # Lifecycle and status flags
    "state", # open/closed, etc.
    "status", # queued/in_progress/completed (workflows/jobs)
    "conclusion", # success/failure/cancelled (CI)
    "merged", # PR merged flag
    "draft", # PR draft flag
    "locked", # issue/PR locked flag
    "protected", # branch protection (branches)
    "prerelease", # release prerelease flag

    # Timestamps (ISO 8601)
    "created_at",
    "updated_at",
    "closed_at",
    "merged_at",
    "published_at",
    "started_at",
    "completed_at",

    # Primary text
    "body", # main markdown content (issue/PR/review/release)
    "message", # commit message (commits)

    # Lightweight impact/size signals
    "comment_count", # issue/PR comment count (or "comments" if that’s the actual field)
    "review_count", # PR review count
    "commit_count", # PR commits count
    "changed_files", # PR changed files
    "additions", # PR/commit additions
    "deletions", # PR/commit deletions
    "size", # repo size / content size (context-dependent)

    # Classification and routing
    "labels", # array of label objects or names
    "assignees", # array of user objects or logins
    "requested_reviewers", # PR reviewers
    "milestone", # milestone reference/name/number

    # PR/compare core refs
    "base_ref",
    "base_sha",
    "head_ref",
    "head_sha",
    "compare_url",

    # CI/workflows essentials
    "workflow_name", # workflow
    "run_id", # workflow run id
    "job_name", # workflow job name
    "logs_url", # logs link (runs/jobs)

    # Releases
    "release_tag", # tag_name for releases
    "release_name", # name/title of release
    "assets", # array of asset summaries (name/url/size)

    # Repo metadata essentials
    "default_branch",
    "license", # license spdx_id/name
    "language", # primary language
    "topics", # repo topics
    "is_private", # repo privacy
    "is_fork", # repo fork flag
    "archived", # repo archived flag

    # Social/engagement counters (compact signal)
    "stargazers_count",
    "forks_count",
    "watchers_count",
    "open_issues_count",

    # Search
    "search_score" # GitHub search score (when present)
  )
}
