#' @include tool-result.R
NULL

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
    owner = remote_info$username,
    repo = remote_info$repo
  )
}

# GitHub Issue Get ------------------------------------------------------------

#' Tool: GitHub Issue Get
#'
#' @examples
#' \dontrun{
#' btw_tool_github_issue_get(issue_number = 123)
#' btw_tool_github_issue_get(issue_number = 123, owner = "tidyverse", repo = "dplyr")
#' }
#'
#' @param issue_number The issue number to retrieve.
#' @param owner Optional GitHub repository owner. If not provided, will attempt
#'   to detect from the current git repository.
#' @param repo Optional GitHub repository name. If not provided, will attempt
#'   to detect from the current git repository.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the issue details.
#'
#' @family github tools
#' @export
btw_tool_github_issue_get <- function(issue_number, owner, repo, `_intent`) {}

btw_tool_github_issue_get_impl <- function(
  issue_number,
  owner = NULL,
  repo = NULL
) {
  check_installed("gh")
  check_number_whole(issue_number, min = 1)
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)

  repo_info <- get_github_repo(owner, repo)

  issue <- gh::gh(
    "/repos/{owner}/{repo}/issues/{issue_number}",
    owner = repo_info$owner,
    repo = repo_info$repo,
    issue_number = issue_number
  )

  labels <- vapply(issue$labels, function(x) x$name, character(1))
  labels_text <- if (length(labels) > 0) {
    paste(labels, collapse = ", ")
  } else {
    "none"
  }

  result <- sprintf(
    "# Issue #%d: %s\n\n**State:** %s\n**Author:** %s\n**Labels:** %s\n**Created:** %s\n\n## Description\n\n%s",
    issue$number,
    issue$title,
    issue$state,
    issue$user$login,
    labels_text,
    issue$created_at,
    issue$body %||% "(no description)"
  )

  btw_tool_result(
    result,
    data = issue,
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "GitHub Issue #%d: %s",
        issue$number,
        issue$title
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_issue_get",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_issue_get_impl,
      name = "btw_tool_github_issue_get",
      description = r"---(Get details of a GitHub issue.

WHEN TO USE:
* Use this tool to read an issue's title, description, state, and metadata.
* This returns only the issue itself, not comments. For the full thread, use btw_tool_github_issue_thread.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.
* If not in a git repo with a GitHub remote, you must provide both parameters.

RETURNS: Issue number, title, body, state, author, labels, and creation date.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Issue Get",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        issue_number = ellmer::type_number(
          "The issue number to retrieve."
        ),
        owner = ellmer::type_string(
          "Optional. GitHub repository owner (username or organization). Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        )
      )
    )
  }
)

# GitHub Issue Thread ---------------------------------------------------------

#' Tool: GitHub Issue Thread
#'
#' @examples
#' \dontrun{
#' btw_tool_github_issue_thread(issue_number = 123)
#' }
#'
#' @param issue_number The issue number to retrieve.
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the issue with all comments.
#'
#' @family github tools
#' @export
btw_tool_github_issue_thread <- function(
  issue_number,
  owner,
  repo,
  `_intent`
) {}

btw_tool_github_issue_thread_impl <- function(
  issue_number,
  owner = NULL,
  repo = NULL
) {
  check_installed("gh")
  check_number_whole(issue_number, min = 1)
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)

  repo_info <- get_github_repo(owner, repo)

  issue <- gh::gh(
    "/repos/{owner}/{repo}/issues/{issue_number}",
    owner = repo_info$owner,
    repo = repo_info$repo,
    issue_number = issue_number
  )

  comments <- gh::gh(
    "/repos/{owner}/{repo}/issues/{issue_number}/comments",
    owner = repo_info$owner,
    repo = repo_info$repo,
    issue_number = issue_number,
    .limit = Inf
  )

  labels <- vapply(issue$labels, function(x) x$name, character(1))
  labels_text <- if (length(labels) > 0) {
    paste(labels, collapse = ", ")
  } else {
    "none"
  }

  result <- sprintf(
    "# Issue #%d: %s\n\n**State:** %s\n**Author:** %s\n**Labels:** %s\n**Created:** %s\n**Comments:** %d\n\n## Description\n\n%s",
    issue$number,
    issue$title,
    issue$state,
    issue$user$login,
    labels_text,
    issue$created_at,
    issue$comments,
    issue$body %||% "(no description)"
  )

  if (length(comments) > 0) {
    result <- paste0(result, "\n\n## Comments\n")

    for (i in seq_along(comments)) {
      comment <- comments[[i]]
      comment_text <- sprintf(
        "\n### Comment %d by %s (%s)\n\n%s",
        i,
        comment$user$login,
        comment$created_at,
        comment$body
      )
      result <- paste0(result, comment_text)
    }
  }

  btw_tool_result(
    result,
    data = list(issue = issue, comments = comments),
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "GitHub Issue Thread #%d: %s",
        issue$number,
        issue$title
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_issue_thread",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_issue_thread_impl,
      name = "btw_tool_github_issue_thread",
      description = r"---(Get a GitHub issue with all comments.

WHEN TO USE:
* Use this tool to read an issue's complete discussion thread.
* This includes the issue description plus all comments in chronological order.
* For just the issue description without comments, use btw_tool_github_issue_get.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

RETURNS: Complete issue thread with description and all comments, including authors and timestamps.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Issue Thread",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        issue_number = ellmer::type_number(
          "The issue number to retrieve."
        ),
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        )
      )
    )
  }
)

# GitHub Pull Request Get -----------------------------------------------------

#' Tool: GitHub Pull Request Get
#'
#' @examples
#' \dontrun{
#' btw_tool_github_pull_request_get(pull_number = 456)
#' }
#'
#' @param pull_number The pull request number to retrieve.
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the pull request details.
#'
#' @family github tools
#' @export
btw_tool_github_pull_request_get <- function(
  pull_number,
  owner,
  repo,
  `_intent`
) {}

btw_tool_github_pull_request_get_impl <- function(
  pull_number,
  owner = NULL,
  repo = NULL
) {
  check_installed("gh")
  check_number_whole(pull_number, min = 1)
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)

  repo_info <- get_github_repo(owner, repo)

  pr <- gh::gh(
    "/repos/{owner}/{repo}/pulls/{pull_number}",
    owner = repo_info$owner,
    repo = repo_info$repo,
    pull_number = pull_number
  )

  result <- sprintf(
    "# Pull Request #%d: %s\n\n**State:** %s\n**Author:** %s\n**Created:** %s\n**Base:** %s\n**Head:** %s\n**Mergeable:** %s\n**Changed Files:** %d (+%d, -%d)\n\n## Description\n\n%s",
    pr$number,
    pr$title,
    pr$state,
    pr$user$login,
    pr$created_at,
    pr$base$ref,
    pr$head$ref,
    pr$mergeable %||% "unknown",
    pr$changed_files,
    pr$additions,
    pr$deletions,
    pr$body %||% "(no description)"
  )

  btw_tool_result(
    result,
    data = pr,
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "GitHub Pull Request #%d: %s",
        pr$number,
        pr$title
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_pull_request_get",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_pull_request_get_impl,
      name = "btw_tool_github_pull_request_get",
      description = r"---(Get details of a GitHub pull request.

WHEN TO USE:
* Use this tool to read a PR's title, description, state, and metadata.
* This returns the PR description and summary, not the actual code changes.
* For code changes, use btw_tool_github_pull_request_diff.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

RETURNS: PR number, title, body, state, author, base/head branches, and change statistics.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Pull Request Get",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        pull_number = ellmer::type_number(
          "The pull request number to retrieve."
        ),
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        )
      )
    )
  }
)

# GitHub Pull Request Diff ----------------------------------------------------

#' Tool: GitHub Pull Request Diff
#'
#' @examples
#' \dontrun{
#' btw_tool_github_pull_request_diff(pull_number = 456)
#' }
#'
#' @param pull_number The pull request number to retrieve.
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the pull request file changes.
#'
#' @family github tools
#' @export
btw_tool_github_pull_request_diff <- function(
  pull_number,
  owner,
  repo,
  `_intent`
) {}

btw_tool_github_pull_request_diff_impl <- function(
  pull_number,
  owner = NULL,
  repo = NULL
) {
  check_installed("gh")
  check_number_whole(pull_number, min = 1)
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)

  repo_info <- get_github_repo(owner, repo)

  files <- gh::gh(
    "/repos/{owner}/{repo}/pulls/{pull_number}/files",
    owner = repo_info$owner,
    repo = repo_info$repo,
    pull_number = pull_number,
    .limit = Inf
  )

  if (length(files) == 0) {
    return(btw_tool_result("No file changes in this pull request"))
  }

  result <- sprintf(
    "# Pull Request #%d File Changes\n\n**Total Files Changed:** %d\n\n",
    pull_number,
    length(files)
  )

  for (file in files) {
    file_header <- sprintf(
      "## %s\n\n**Status:** %s | **Changes:** +%d -%d\n",
      file$filename,
      file$status,
      file$additions,
      file$deletions
    )

    result <- paste0(result, file_header)

    if (!is.null(file$patch)) {
      result <- paste0(result, "\n```diff\n", file$patch, "\n```\n\n")
    } else {
      result <- paste0(result, "\n*(No patch available)*\n\n")
    }
  }

  btw_tool_result(
    result,
    data = files,
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "GitHub PR #%d File Changes",
        pull_number
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_pull_request_diff",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_pull_request_diff_impl,
      name = "btw_tool_github_pull_request_diff",
      description = r"---(Get the code changes (diff) for a GitHub pull request.

WHEN TO USE:
* Use this tool to see the actual code changes in a pull request.
* Returns file-by-file diffs with additions and deletions.
* For PR description and metadata, use btw_tool_github_pull_request_get.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

RETURNS: List of changed files with their diffs, showing additions, deletions, and modifications.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Pull Request Diff",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        pull_number = ellmer::type_number(
          "The pull request number to retrieve."
        ),
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        )
      )
    )
  }
)

# GitHub Issue Create ---------------------------------------------------------

#' Tool: GitHub Issue Create
#'
#' @examples
#' \dontrun{
#' btw_tool_github_issue_create(
#'   title = "Bug: function returns wrong value",
#'   body = "When calling foo(1), expected 2 but got 3"
#' )
#' }
#'
#' @param title The issue title.
#' @param body The issue body/description.
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @param labels Optional character vector of label names to apply.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the created issue details.
#'
#' @family github tools
#' @export
btw_tool_github_issue_create <- function(
  title,
  body,
  owner,
  repo,
  labels,
  `_intent`
) {}

btw_tool_github_issue_create_impl <- function(
  title,
  body,
  owner = NULL,
  repo = NULL,
  labels = NULL
) {
  check_installed("gh")
  check_string(title)
  check_string(body)
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)
  check_character(labels, allow_null = TRUE)

  repo_info <- get_github_repo(owner, repo)

  params <- list(
    title = title,
    body = body
  )

  if (!is.null(labels)) {
    params$labels <- as.list(labels)
  }

  issue <- gh::gh(
    "POST /repos/{owner}/{repo}/issues",
    owner = repo_info$owner,
    repo = repo_info$repo,
    .params = params
  )

  result <- sprintf(
    "Created issue #%d: %s\n\nURL: %s",
    issue$number,
    issue$title,
    issue$html_url
  )

  btw_tool_result(
    result,
    data = issue,
    display = list(
      markdown = md_code_block("", result),
      title = HTML(sprintf(
        "Created GitHub Issue #%d",
        issue$number
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_issue_create",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_issue_create_impl,
      name = "btw_tool_github_issue_create",
      description = r"---(Create a new GitHub issue.

WHEN TO USE:
* Use this tool to create a new issue in a GitHub repository.
* Provide a clear, descriptive title and detailed body.
* Optionally add labels to categorize the issue.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

IMPORTANT:
* This modifies the repository by creating a new issue.
* Ensure you have permission to create issues in the repository.

RETURNS: Created issue number and URL.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Issue Create",
        read_only_hint = FALSE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        title = ellmer::type_string(
          "The issue title."
        ),
        body = ellmer::type_string(
          "The issue body/description."
        ),
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        labels = ellmer::type_array(
          "Optional. Array of label names to apply to the issue.",
          items = ellmer::type_string(),
          required = FALSE
        )
      )
    )
  }
)

# GitHub Pull Request Create --------------------------------------------------

#' Tool: GitHub Pull Request Create
#'
#' @examples
#' \dontrun{
#' btw_tool_github_pull_request_create(
#'   title = "Add new feature",
#'   body = "This PR adds...",
#'   head = "feature-branch",
#'   base = "main"
#' )
#' }
#'
#' @param title The pull request title.
#' @param body The pull request body/description.
#' @param head The name of the branch containing changes.
#' @param base The name of the branch to merge into.
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the created pull request details.
#'
#' @family github tools
#' @export
btw_tool_github_pull_request_create <- function(
  title,
  body,
  head,
  base,
  owner,
  repo,
  `_intent`
) {}

btw_tool_github_pull_request_create_impl <- function(
  title,
  body,
  head,
  base,
  owner = NULL,
  repo = NULL
) {
  check_installed("gh")
  check_string(title)
  check_string(body)
  check_string(head)
  check_string(base)
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)

  repo_info <- get_github_repo(owner, repo)

  pr <- gh::gh(
    "POST /repos/{owner}/{repo}/pulls",
    owner = repo_info$owner,
    repo = repo_info$repo,
    title = title,
    body = body,
    head = head,
    base = base
  )

  result <- sprintf(
    "Created pull request #%d: %s\n\nBase: %s <- Head: %s\nURL: %s",
    pr$number,
    pr$title,
    pr$base$ref,
    pr$head$ref,
    pr$html_url
  )

  btw_tool_result(
    result,
    data = pr,
    display = list(
      markdown = md_code_block("", result),
      title = HTML(sprintf(
        "Created GitHub Pull Request #%d",
        pr$number
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_pull_request_create",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_pull_request_create_impl,
      name = "btw_tool_github_pull_request_create",
      description = r"---(Create a new GitHub pull request.

WHEN TO USE:
* Use this tool to create a pull request to merge changes from one branch into another.
* Ensure the head branch exists and has commits that differ from the base branch.
* Provide a clear title and detailed description of the changes.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

IMPORTANT:
* This modifies the repository by creating a new pull request.
* Ensure you have permission to create PRs in the repository.
* The head branch must exist and have commits ahead of the base branch.

RETURNS: Created pull request number and URL.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Pull Request Create",
        read_only_hint = FALSE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        title = ellmer::type_string(
          "The pull request title."
        ),
        body = ellmer::type_string(
          "The pull request body/description."
        ),
        head = ellmer::type_string(
          "The name of the branch containing your changes."
        ),
        base = ellmer::type_string(
          "The name of the branch you want to merge into (e.g., 'main' or 'master')."
        ),
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        )
      )
    )
  }
)

# GitHub Issues List ----------------------------------------------------------

#' Tool: GitHub Issues List
#'
#' @examples
#' \dontrun{
#' btw_tool_github_issues_list()
#' btw_tool_github_issues_list(state = "open", labels = "bug")
#' btw_tool_github_issues_list(author = "hadley", assignee = "jennybc")
#' }
#'
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @param state Optional issue state filter: `"open"`, `"closed"`, or `"all"`.
#'   Defaults to `"open"`.
#' @param labels Optional character vector of label names to filter by.
#' @param author Optional GitHub username to filter issues by author.
#' @param assignee Optional GitHub username to filter issues by assignee.
#' @param max Maximum number of issues to retrieve. Defaults to 30.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a list of issues matching the filters.
#'
#' @family github tools
#' @export
btw_tool_github_issues_list <- function(
  owner,
  repo,
  state,
  labels,
  author,
  assignee,
  max,
  `_intent`
) {}

btw_tool_github_issues_list_impl <- function(
  owner = NULL,
  repo = NULL,
  state = "open",
  labels = NULL,
  author = NULL,
  assignee = NULL,
  max = 30
) {
  check_installed("gh")
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)
  state <- arg_match(state, c("open", "closed", "all"))
  check_character(labels, allow_null = TRUE)
  check_string(author, allow_null = TRUE)
  check_string(assignee, allow_null = TRUE)
  check_number_whole(max, min = 1)

  repo_info <- get_github_repo(owner, repo)

  params <- list(
    state = state,
    per_page = min(max, 100)
  )

  if (!is.null(labels)) {
    params$labels <- paste(labels, collapse = ",")
  }
  if (!is.null(author)) {
    params$creator <- author
  }
  if (!is.null(assignee)) {
    params$assignee <- assignee
  }

  issues <- gh::gh(
    "/repos/{owner}/{repo}/issues",
    owner = repo_info$owner,
    repo = repo_info$repo,
    .params = params,
    .limit = max
  )

  if (length(issues) == 0) {
    msg <- "No issues found"
    filters <- c()
    if (state != "all") {
      filters <- c(filters, sprintf("state=%s", state))
    }
    if (!is.null(labels)) {
      filters <- c(filters, sprintf("labels=%s", paste(labels, collapse = ",")))
    }
    if (!is.null(author)) {
      filters <- c(filters, sprintf("author=%s", author))
    }
    if (!is.null(assignee)) {
      filters <- c(filters, sprintf("assignee=%s", assignee))
    }
    if (length(filters) > 0) {
      msg <- paste0(msg, " matching filters: ", paste(filters, collapse = ", "))
    }
    return(btw_tool_result(msg))
  }

  result <- sprintf(
    "# Issues in %s/%s\n\n**Total:** %d\n\n",
    repo_info$owner,
    repo_info$repo,
    length(issues)
  )

  for (issue in issues) {
    issue_labels <- vapply(issue$labels, function(x) x$name, character(1))
    labels_text <- if (length(issue_labels) > 0) {
      sprintf(" [%s]", paste(issue_labels, collapse = ", "))
    } else {
      ""
    }

    issue_body_preview <- substr(issue$body %||% "(no description)", 1, 200)
    if (nchar(issue$body %||% "") > 200) {
      issue_body_preview <- paste0(issue_body_preview, "...")
    }

    issue_text <- sprintf(
      "## #%d: %s\n\n**State:** %s | **Author:** %s%s\n\n%s\n\n",
      issue$number,
      issue$title,
      issue$state,
      issue$user$login,
      labels_text,
      issue_body_preview
    )
    result <- paste0(result, issue_text)
  }

  btw_tool_result(
    result,
    data = issues,
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "GitHub Issues: %s/%s",
        repo_info$owner,
        repo_info$repo
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_issues_list",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_issues_list_impl,
      name = "btw_tool_github_issues_list",
      description = r"---(List issues in a GitHub repository with optional filtering.

WHEN TO USE:
* Use this tool to browse or search for issues in a repository.
* Filter by state (open/closed), labels, author, or assignee.
* Returns a summary of each issue with truncated description.
* For full issue details, use btw_tool_github_issue_get or btw_tool_github_issue_thread.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

RETURNS: List of issues with number, title, state, author, labels, and description preview.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Issues List",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        state = ellmer::type_enum(
          c("open", "closed", "all"),
          'Optional. Filter by issue state: "open" (default), "closed", or "all".',
          required = FALSE
        ),
        labels = ellmer::type_array(
          "Optional. Filter by label names. Issues must have all specified labels.",
          items = ellmer::type_string(),
          required = FALSE
        ),
        author = ellmer::type_string(
          "Optional. Filter by issue author's GitHub username.",
          required = FALSE
        ),
        assignee = ellmer::type_string(
          "Optional. Filter by assignee's GitHub username.",
          required = FALSE
        ),
        max = ellmer::type_number(
          "Optional. Maximum number of issues to retrieve. Defaults to 30.",
          required = FALSE
        )
      )
    )
  }
)

# GitHub Pull Requests List ---------------------------------------------------

#' Tool: GitHub Pull Requests List
#'
#' @examples
#' \dontrun{
#' btw_tool_github_pull_requests_list()
#' btw_tool_github_pull_requests_list(state = "closed")
#' btw_tool_github_pull_requests_list(author = "hadley")
#' }
#'
#' @param owner Optional GitHub repository owner.
#' @param repo Optional GitHub repository name.
#' @param state Optional pull request state filter: `"open"`, `"closed"`, or
#'   `"all"`. Defaults to `"open"`.
#' @param author Optional GitHub username to filter PRs by author.
#' @param assignee Optional GitHub username to filter PRs by assignee.
#' @param max Maximum number of PRs to retrieve. Defaults to 30.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a list of pull requests matching the filters.
#'
#' @family github tools
#' @export
btw_tool_github_pull_requests_list <- function(
  owner,
  repo,
  state,
  author,
  assignee,
  max,
  `_intent`
) {}

btw_tool_github_pull_requests_list_impl <- function(
  owner = NULL,
  repo = NULL,
  state = "open",
  author = NULL,
  assignee = NULL,
  max = 30
) {
  check_installed("gh")
  check_string(owner, allow_null = TRUE)
  check_string(repo, allow_null = TRUE)
  state <- arg_match(state, c("open", "closed", "all"))
  check_string(author, allow_null = TRUE)
  check_string(assignee, allow_null = TRUE)
  check_number_whole(max, min = 1)

  repo_info <- get_github_repo(owner, repo)

  # Build search query for author/assignee filtering
  # The /repos/{owner}/{repo}/pulls endpoint doesn't support author/assignee filters
  # So we need to use the search API if those filters are present
  if (!is.null(author) || !is.null(assignee)) {
    query_parts <- sprintf("repo:%s/%s is:pr", repo_info$owner, repo_info$repo)

    if (state != "all") {
      query_parts <- paste(query_parts, sprintf("is:%s", state))
    }
    if (!is.null(author)) {
      query_parts <- paste(query_parts, sprintf("author:%s", author))
    }
    if (!is.null(assignee)) {
      query_parts <- paste(query_parts, sprintf("assignee:%s", assignee))
    }

    search_result <- gh::gh(
      "/search/issues",
      q = query_parts,
      per_page = min(max, 100),
      .limit = max
    )

    prs <- search_result$items
  } else {
    # Use the simpler pulls endpoint
    params <- list(
      state = state,
      per_page = min(max, 100)
    )

    prs <- gh::gh(
      "/repos/{owner}/{repo}/pulls",
      owner = repo_info$owner,
      repo = repo_info$repo,
      .params = params,
      .limit = max
    )
  }

  if (length(prs) == 0) {
    msg <- "No pull requests found"
    filters <- c()
    if (state != "all") {
      filters <- c(filters, sprintf("state=%s", state))
    }
    if (!is.null(author)) {
      filters <- c(filters, sprintf("author=%s", author))
    }
    if (!is.null(assignee)) {
      filters <- c(filters, sprintf("assignee=%s", assignee))
    }
    if (length(filters) > 0) {
      msg <- paste0(msg, " matching filters: ", paste(filters, collapse = ", "))
    }
    return(btw_tool_result(msg))
  }

  result <- sprintf(
    "# Pull Requests in %s/%s\n\n**Total:** %d\n\n",
    repo_info$owner,
    repo_info$repo,
    length(prs)
  )

  for (pr in prs) {
    # Handle both pulls and search results format
    pr_state <- pr$state %||% "unknown"
    pr_author <- pr$user$login %||% "unknown"
    pr_number <- pr$number %||% 0
    pr_title <- pr$title %||% "Untitled"
    pr_body <- pr$body %||% "(no description)"

    pr_text <- sprintf(
      "## #%d: %s\n\n**State:** %s | **Author:** %s\n\n%s\n\n",
      pr_number,
      pr_title,
      pr_state,
      pr_author,
      substr(pr_body, 1, 200)
    )
    result <- paste0(result, pr_text)
  }

  btw_tool_result(
    result,
    data = prs,
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "GitHub Pull Requests: %s/%s",
        repo_info$owner,
        repo_info$repo
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_github_pull_requests_list",
  group = "github",
  tool = function() {
    ellmer::tool(
      btw_tool_github_pull_requests_list_impl,
      name = "btw_tool_github_pull_requests_list",
      description = r"---(List pull requests in a GitHub repository with optional filtering.

WHEN TO USE:
* Use this tool to browse or search for pull requests in a repository.
* Filter by state (open/closed), author, or assignee.
* Returns a summary of each PR with truncated description.
* For full PR details, use btw_tool_github_pull_request_get or btw_tool_github_pull_request_diff.

REPOSITORY DETECTION:
* If `owner` and `repo` are not provided, will attempt to detect from the current git repository.

RETURNS: List of PRs with number, title, state, author, and description preview.
      )---",
      annotations = ellmer::tool_annotations(
        title = "GitHub Pull Requests List",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gh")
      ),
      arguments = list(
        owner = ellmer::type_string(
          "Optional. GitHub repository owner. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        repo = ellmer::type_string(
          "Optional. GitHub repository name. Will be detected from git remote if not provided.",
          required = FALSE
        ),
        state = ellmer::type_enum(
          c("open", "closed", "all"),
          'Optional. Filter by PR state: "open" (default), "closed", or "all".',
          required = FALSE
        ),
        author = ellmer::type_string(
          "Optional. Filter by PR author's GitHub username.",
          required = FALSE
        ),
        assignee = ellmer::type_string(
          "Optional. Filter by assignee's GitHub username.",
          required = FALSE
        ),
        max = ellmer::type_number(
          "Optional. Maximum number of PRs to retrieve. Defaults to 30.",
          required = FALSE
        )
      )
    )
  }
)
