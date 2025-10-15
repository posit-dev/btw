#' @include tool-result.R
NULL

# Git Status ------------------------------------------------------------------

#' Tool: Git Status
#'
#' @examples
#' \dontrun{
#' btw_tool_git_status()
#' }
#'
#' @param staged Optional. Return only staged (`TRUE`) or unstaged files
#'   (`FALSE`). Use `NULL` to show both (default).
#' @param pathspec Optional character vector with paths to match.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of file statuses.
#'
#' @family Tools
#' @export
btw_tool_git_status <- function(staged, pathspec, `_intent`) {}

btw_tool_git_status_impl <- function(
  staged = NULL,
  pathspec = NULL
) {
  check_installed("gert")
  check_bool(staged, allow_null = TRUE)
  check_character(pathspec, allow_null = TRUE)

  status <- gert::git_status(staged = staged, pathspec = pathspec)

  if (nrow(status) == 0) {
    return(btw_tool_result("No changes to report"))
  }

  # Select relevant columns
  fields <- c("file", "status", "staged")
  status_display <- status[fields]

  md_res <- md_table(status_display)

  btw_tool_result(
    md_res,
    data = status_display,
    display = list(markdown = md_res)
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_status",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_status_impl,
      name = "btw_tool_git_status",
      description = r"---(Show the status of the git working directory.

WHEN TO USE:
* Use this tool to see which files have been modified, staged, or are untracked.
* This is typically the first tool to call when working with git operations.

RETURNS: A table showing file paths, their status (new, modified, deleted, etc.), and whether they are staged.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Status",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        staged = ellmer::type_boolean(
          "Optional. Return only staged (TRUE) or unstaged files (FALSE). Use NULL to show both (default).",
          required = FALSE
        ),
        pathspec = ellmer::type_array(
          "Optional. Character vector with paths to match.",
          items = ellmer::type_string(),
          required = FALSE
        )
      )
    )
  }
)

# Git Diff --------------------------------------------------------------------

#' Tool: Git Diff
#'
#' @examples
#' \dontrun{
#' btw_tool_git_diff()
#' btw_tool_git_diff(ref = "HEAD")
#' }
#'
#' @param ref Optional reference such as `"HEAD"`, a commit SHA, or two refs
#'   separated by `..` (e.g., `"main..feature"`). Use `NULL` (default) to diff
#'   the working directory against the repository index (unstaged changes).
#'   Use `"HEAD"` to see staged changes.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a diff patch as a formatted string.
#'
#' @family Tools
#' @export
btw_tool_git_diff <- function(ref, `_intent`) {}

btw_tool_git_diff_impl <- function(ref = NULL) {
  check_installed("gert")
  check_string(ref, allow_null = TRUE)

  diff_patch <- gert::git_diff_patch(ref = ref)

  if (length(diff_patch) == 0 || nzchar(diff_patch) == FALSE) {
    msg <- if (is.null(ref)) {
      "No unstaged changes to show"
    } else {
      sprintf("No changes to show for ref: %s", ref)
    }
    return(btw_tool_result(msg))
  }

  # Format as a code block
  value <- md_code_block("diff", diff_patch)

  btw_tool_result(
    value,
    display = list(
      markdown = value,
      title = HTML(sprintf(
        "Diff%s",
        if (!is.null(ref)) sprintf(" (%s)", ref) else ""
      ))
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_diff",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_diff_impl,
      name = "btw_tool_git_diff",
      description = r"---(View changes in the working directory or between commits.

WHEN TO USE:
* Use with no arguments to see unstaged changes (working directory vs. index).
* Use with `ref = "HEAD"` to see staged changes (index vs. HEAD).
* Use with `ref = "commit1..commit2"` to compare two commits.
* Use with a branch name to compare current HEAD with that branch.

RETURNS: A unified diff patch showing the changes.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Diff",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        ref = ellmer::type_string(
          'Optional reference like "HEAD", a commit SHA, or "ref1..ref2". NULL (default) shows unstaged changes.',
          required = FALSE
        )
      )
    )
  }
)

# Git Log ---------------------------------------------------------------------

#' Tool: Git Log
#'
#' @examples
#' \dontrun{
#' btw_tool_git_log()
#' btw_tool_git_log(max = 20, ref = "main")
#' }
#'
#' @param ref Revision string with a branch/tag/commit value. Defaults to
#'   `"HEAD"`.
#' @param max Maximum number of commits to retrieve. Defaults to 100.
#' @param after Optional date or timestamp: only include commits after this
#'   date.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of commit history.
#'
#' @family Tools
#' @export
btw_tool_git_log <- function(ref, max, after, `_intent`) {}

btw_tool_git_log_impl <- function(
  ref = "HEAD",
  max = 100,
  after = NULL
) {
  check_installed("gert")
  check_string(ref)
  check_number_whole(max, min = 1)
  check_string(after, allow_null = TRUE)

  log <- gert::git_log(ref = ref, max = max, after = after)

  if (nrow(log) == 0) {
    return(btw_tool_result("No commits found"))
  }

  # Select and format relevant columns
  fields <- c("commit", "author", "time", "message")
  log_display <- log[fields]

  # Truncate commit SHA to 7 characters for display
  log_display$commit <- substr(log_display$commit, 1, 7)

  # Truncate long messages
  log_display$message <- vapply(
    log_display$message,
    function(msg) {
      first_line <- strsplit(msg, "\n")[[1]][1]
      if (nchar(first_line) > 60) {
        paste0(substr(first_line, 1, 57), "...")
      } else {
        first_line
      }
    },
    character(1)
  )

  md_res <- md_table(log_display)

  btw_tool_result(
    md_res,
    data = log_display,
    display = list(markdown = md_res)
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_log",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_log_impl,
      name = "btw_tool_git_log",
      description = r"---(Show the commit history for a repository.

WHEN TO USE:
* Use this tool to view recent commits and their messages.
* Useful for understanding the history of changes before making new commits.
* Can filter by branch, number of commits, or date range.

RETURNS: A table with commit SHA (short), author, timestamp, and message.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Log",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        ref = ellmer::type_string(
          'Revision string (branch/tag/commit). Defaults to "HEAD".',
          required = FALSE
        ),
        max = ellmer::type_number(
          "Maximum number of commits to retrieve. Defaults to 100.",
          required = FALSE
        ),
        after = ellmer::type_string(
          "Optional date or timestamp: only include commits after this date.",
          required = FALSE
        )
      )
    )
  }
)

# Git Commit ------------------------------------------------------------------

#' Tool: Git Commit
#'
#' @examples
#' \dontrun{
#' btw_tool_git_commit(message = "Fix bug in analysis", files = c("analysis.R"))
#' }
#'
#' @param message A commit message describing the changes.
#' @param files Optional character vector of file paths to stage and commit.
#'   Use `"."` to stage all changed files. If `NULL`, commits currently staged files.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the commit SHA.
#'
#' @family Tools
#' @export
btw_tool_git_commit <- function(message, files, `_intent`) {}

btw_tool_git_commit_impl <- function(
  message,
  files = NULL
) {
  check_installed("gert")
  check_string(message)
  check_character(files, allow_null = TRUE)

  # Stage files if provided
  if (!is.null(files)) {
    gert::git_add(files)
  }

  # Create commit
  commit_sha <- gert::git_commit(message = message)

  result <- sprintf(
    "Created commit: %s\nMessage: %s",
    substr(commit_sha, 1, 7),
    message
  )

  btw_tool_result(
    result,
    data = list(sha = commit_sha, message = message),
    display = list(
      markdown = md_code_block("", result),
      title = HTML("Git Commit")
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_commit",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_commit_impl,
      name = "btw_tool_git_commit",
      description = r"---(Stage files and create a git commit.

WHEN TO USE:
* Use this tool to commit changes after reviewing them with git_status and git_diff.
* If `files` is provided, those files will be staged before committing.
* If `files` is NULL, only currently staged files will be committed.

IMPORTANT:
* Always provide a clear, descriptive commit message.
* Review changes with git_status and git_diff before committing.
* This modifies the repository state.

RETURNS: The commit SHA and confirmation message.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Commit",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        message = ellmer::type_string(
          "A commit message describing the changes."
        ),
        files = ellmer::type_array(
          'Optional. Files to stage before committing. Use "." for all changed files, or NULL to commit already staged files.',
          items = ellmer::type_string(),
          required = FALSE
        )
      )
    )
  }
)

# Git Branch List -------------------------------------------------------------

#' Tool: Git Branch List
#'
#' @examples
#' \dontrun{
#' btw_tool_git_branch_list()
#' btw_tool_git_branch_list(local = FALSE)  # remote branches
#' }
#'
#' @param local Optional. Set `TRUE` to show only local branches, `FALSE` for
#'   remote branches, or `NULL` for both. Defaults to `NULL`.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of branches.
#'
#' @family Tools
#' @export
btw_tool_git_branch_list <- function(local, `_intent`) {}

btw_tool_git_branch_list_impl <- function(local = NULL) {
  check_installed("gert")
  check_bool(local, allow_null = TRUE)

  branches <- gert::git_branch_list(local = local)

  if (nrow(branches) == 0) {
    return(btw_tool_result("No branches found"))
  }

  # Select relevant columns
  fields <- c("name", "ref", "upstream", "updated")
  # Only include columns that exist
  fields <- intersect(fields, names(branches))
  branches_display <- branches[fields]

  md_res <- md_table(branches_display)

  btw_tool_result(
    md_res,
    data = branches_display,
    display = list(markdown = md_res)
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_branch_list",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_branch_list_impl,
      name = "btw_tool_git_branch_list",
      description = r"---(List git branches in the repository.

WHEN TO USE:
* Use this tool to see available branches before checking out or creating a new branch.
* Shows local branches by default, but can also show remote branches.

RETURNS: A table of branch names, refs, upstream tracking, and last update time.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Branches",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        local = ellmer::type_boolean(
          "Optional. TRUE for local branches only, FALSE for remote branches only, NULL for both (default).",
          required = FALSE
        )
      )
    )
  }
)

# Git Branch Create -----------------------------------------------------------

#' Tool: Git Branch Create
#'
#' @examples
#' \dontrun{
#' btw_tool_git_branch_create(branch = "feature/new-analysis")
#' }
#'
#' @param branch Name of the new branch to create.
#' @param ref Optional reference point for the new branch. Defaults to `"HEAD"`.
#' @param checkout Whether to check out the new branch after creation. Defaults to `TRUE`.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a confirmation message.
#'
#' @family Tools
#' @export
btw_tool_git_branch_create <- function(branch, ref, checkout, `_intent`) {}

btw_tool_git_branch_create_impl <- function(
  branch,
  ref = "HEAD",
  checkout = TRUE
) {
  check_installed("gert")
  check_string(branch)
  check_string(ref)
  check_bool(checkout)

  gert::git_branch_create(branch = branch, ref = ref, checkout = checkout)

  result <- sprintf(
    "Created branch '%s' from '%s'%s",
    branch,
    ref,
    if (checkout) " and checked it out" else ""
  )

  btw_tool_result(
    result,
    display = list(
      markdown = md_code_block("", result),
      title = HTML("Git Branch Create")
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_branch_create",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_branch_create_impl,
      name = "btw_tool_git_branch_create",
      description = r"---(Create a new git branch.

WHEN TO USE:
* Use this tool to create a new branch for feature development or bug fixes.
* By default, the new branch is checked out automatically.
* The branch is created from HEAD unless a different ref is specified.

IMPORTANT:
* This modifies the repository state.
* Ensure you're on the correct branch before creating from HEAD.

RETURNS: Confirmation message with branch name and ref.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Branch Create",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        branch = ellmer::type_string(
          "Name of the new branch to create."
        ),
        ref = ellmer::type_string(
          'Optional reference point (branch/tag/commit) for the new branch. Defaults to "HEAD".',
          required = FALSE
        ),
        checkout = ellmer::type_boolean(
          "Whether to check out the new branch after creation. Defaults to TRUE.",
          required = FALSE
        )
      )
    )
  }
)

# Git Branch Checkout ---------------------------------------------------------

#' Tool: Git Branch Checkout
#'
#' @examples
#' \dontrun{
#' btw_tool_git_branch_checkout(branch = "main")
#' }
#'
#' @param branch Name of branch to check out.
#' @param force Whether to force checkout even with uncommitted changes. Defaults to `FALSE`.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a confirmation message.
#'
#' @family Tools
#' @export
btw_tool_git_branch_checkout <- function(branch, force, `_intent`) {}

btw_tool_git_branch_checkout_impl <- function(
  branch,
  force = FALSE
) {
  check_installed("gert")
  check_string(branch)
  check_bool(force)

  gert::git_branch_checkout(branch = branch, force = force)

  result <- sprintf("Checked out branch '%s'", branch)

  btw_tool_result(
    result,
    display = list(
      markdown = md_code_block("", result),
      title = HTML("Git Checkout")
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_git_branch_checkout",
  group = "git",
  tool = function() {
    ellmer::tool(
      btw_tool_git_branch_checkout_impl,
      name = "btw_tool_git_branch_checkout",
      description = r"---(Switch to a different git branch.

WHEN TO USE:
* Use this tool to switch between existing branches.
* Check git_status first to ensure no uncommitted changes will be lost.
* Use git_branch_list to see available branches.

IMPORTANT:
* This modifies the repository state and working directory.
* Will fail if there are uncommitted changes unless force = TRUE.
* Using force = TRUE can lose uncommitted changes.

RETURNS: Confirmation message with branch name.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Checkout",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() rlang::is_installed("gert")
      ),
      arguments = list(
        branch = ellmer::type_string(
          "Name of the branch to check out."
        ),
        force = ellmer::type_boolean(
          "Whether to force checkout even with uncommitted changes. Defaults to FALSE. Use with caution.",
          required = FALSE
        )
      )
    )
  }
)
