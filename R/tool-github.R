#' @include tool-result.R
NULL

# GitHub API Wrapper with Endpoint Validation ---------------------------------

btw_github_default_allow_rules <- function() {
  c(
    # Basic Information Gathering Endpoints
    "GET /repos/*/*/issues/*",
    "GET /repos/*/*/issues/*/comments",
    "GET /repos/*/*/pulls/*",
    "GET /repos/*/*/pulls/*/files",
    "GET /repos/*/*/issues",
    "GET /repos/*/*/pulls",
    "POST /repos/*/*/issues",
    "POST /repos/*/*/pulls",
    "POST /repos/*/*/issues/*/comments",

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
  user_block <- getOption("btw.github_endpoint.block", character())
  user_allow <- getOption("btw.github_endpoint.allow", character())

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
    "btw.github_endpoint.allow = c(getOption('btw.github_endpoint.allow'), '%s')",
    endpoint
  )

  # 3. Check built-in block rules third
  for (rule in btw_github_default_block_rules()) {
    if (btw_github_match_pattern(endpoint, rule)) {
      cli::cli_abort(c(
        "GitHub API endpoint is blocked: {.val {endpoint}}",
        x = "Matched btw block rule: {.val {rule}}",
        i = "To allow this endpoint anyway, add it to the {.code btw.github_endpoint.allow} option.",
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
    i = "To allow this endpoint, add it to the {.code btw.github_endpoint.allow} option.",
    i = "Example: {.run {cmd_allow}}"
  ))
}

btw_gh <- function(endpoint, ...) {
  endpoint <- btw_github_check_endpoint(endpoint)
  gh::gh(endpoint, ...)
}

btw_eval_gh_code <- function(code) {
  check_installed("gh")

  repo_info <- get_github_repo(NULL, NULL)

  gh_namespace <- asNamespace("gh")

  env <- new_environment(list(
    owner = repo_info$owner,
    repo = repo_info$repo,
    gh = btw_gh,
    gh_whoami = gh::gh_whoami,
    `[` = base::`[`,
    `[[` = base::`[[`,
    `$` = base::`$`,
    c = base::c,
    list = base::list,
    lapply = base::lapply,
    vapply = base::vapply
  ))

  tryCatch(
    eval(parse(text = code), envir = env),
    error = function(e) {
      cld_not_find <- gettext("could not find function", domain = "R")
      e_msg <- conditionMessage(e)
      if (grepl(cld_not_find, e_msg, fixed = TRUE)) {
        e_msg <- sub(cld_not_find, "", e_msg, fixed = TRUE)
        e_msg <- trimws(e_msg)
        cli::cli_abort(c(
          "Function not allowed or not found: {e_msg}",
          i = "Only unprefixed `gh()` and `gh_whoami()` from the gh package are allowed."
        ))
      } else {
        cli::cli_abort("Error evaluating GitHub code.", parent = e)
      }
    }
  )
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

  # Try to detect from current git repo
  remote_info <- tryCatch(
    gh::gh_tree_remote(),
    error = function(e) NULL
  )

  if (is.null(remote_info)) {
    abort(c(
      "Could not detect GitHub repository.",
      i = "Provide `owner` and `repo` parameters, or run from within a git repository with a GitHub remote."
    ))
  }

  list(
    owner = owner %||% remote_info$username,
    repo = repo %||% remote_info$repo
  )
}

# GitHub Tool -----------------------------------------------------------------

#' Tool: GitHub
#'
#' Execute R code that calls the GitHub API using `gh()` function from the gh
#' package.
#'
#' @details
#' ## Endpoint Validation
#'
#' This tool uses endpoint validation to ensure only safe GitHub API operations
#' are performed. By default, most read operations and low-risk write operations
#' (like creating issues or PRs) are allowed, while dangerous operations (like
#' merging PRs or deleting repositories) are blocked.
#'
#' To customize which endpoints are allowed or blocked, use the
#' `btw.github_endpoint.allow` and `btw.github_endpoint.block` options:
#'
#' ```r
#' # Allow a specific endpoint
#' options(btw.github_endpoint.allow = c(
#'   getOption("btw.github_endpoint.allow"),
#'   "GET /repos/*/*/topics"
#' ))
#'
#' # Block a specific endpoint
#' options(btw.github_endpoint.block = c(
#'   getOption("btw.github_endpoint.block"),
#'   "GET /repos/*/*/branches"
#' ))
#' ```
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
#' btw_tool_github(code = 'gh("/repos/{owner}/{repo}/issues/123")')
#'
#' # List open issues
#' btw_tool_github(code = 'gh("/repos/{owner}/{repo}/issues", state = "open")')
#'
#' # Create an issue
#' btw_tool_github(code = '
#'   gh("POST /repos/{owner}/{repo}/issues",
#'      title = "Bug report",
#'      body = "Description of bug")
#' ')
#'
#' # Target a different repository
#' btw_tool_github(code = '
#'   owner <- "tidyverse"
#'   repo <- "dplyr"
#'   gh("/repos/{owner}/{repo}/issues/1")
#' ')
#' }
#'
#' @param code R code that calls `gh()` or `gh_whoami()`. The code will be
#'   evaluated in an environment where `owner` and `repo` variables are
#'   predefined (defaulting to the current repository if detected). The `gh()`
#'   function is available without needing to load the gh package.
#' @inheritParams btw_tool_docs_package_news
#' @return A `btw_tool_result` containing the result of the GitHub API call.
#'
#' @family github tools
#' @export
btw_tool_github <- function(code, `_intent`) {}

btw_tool_github_impl <- function(code) {
  check_string(code)

  result <- btw_eval_gh_code(code)

  # Convert result to btw_tool_result
  if (inherits(result, "btw_tool_result")) {
    return(result)
  }

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

CODE ENVIRONMENT:
* `owner` and `repo` variables are pre-defined, defaulting to the current repository if detected
* `gh()` function is available to call any GitHub API endpoint
* `gh_whoami()` is available to get current user information
* You can reassign `owner` and `repo` in your code to target different repositories

ENDPOINT VALIDATION:
* Most read operations (GET) are allowed by default
* Low-risk write operations (creating issues/PRs, adding comments) are allowed
* Dangerous operations (merging PRs, deleting repos, managing webhooks/secrets) are blocked
* If an endpoint is blocked, the error message will explain how to allow it

EXAMPLES:
```r
# Get an issue
gh("/repos/{owner}/{repo}/issues/123")

# List open issues
gh("/repos/{owner}/{repo}/issues", state = "open", .limit = 10)

# Create an issue
gh("POST /repos/{owner}/{repo}/issues",
   title = "Bug report",
   body = "Description")

# Get PR diff files
gh("/repos/{owner}/{repo}/pulls/456/files")

# Target a different repo
owner <- "tidyverse"
repo <- "dplyr"
gh("/repos/{owner}/{repo}/issues/1")
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
        )
      )
    )
  }
)
