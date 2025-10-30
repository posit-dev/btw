#' @include tool-result.R
NULL

btw_can_register_git_tool <- function() {
  if (!rlang::is_installed("gert")) {
    return(FALSE)
  }

  tryCatch(
    {
      gert::git_info()
      TRUE
    },
    error = function(e) FALSE
  )
}

# Git Status ------------------------------------------------------------------

#' Tool: Git Status
#'
#' This tool allows the LLM to run [gert::git_status()], equivalent to `git
#' status` in the terminal, and to see the current status of the working
#' directory.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'   writeLines("hello, world", "hello.md")
#'
#'   # What the LLM sees
#'   cat(btw_tool_git_status()@value)
#' })
#'
#' @param include One of `"both"`, `"staged"`, or `"unstaged"`. Use `"both"` to
#'   show both staged and unstaged files (default).
#' @param pathspec Optional character vector with paths to match.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of file statuses.
#'
#' @family git tools
#' @export
btw_tool_git_status <- function(include, pathspec, `_intent`) {}

btw_tool_git_status_impl <- function(
  include = c("both", "staged", "unstaged"),
  pathspec = NULL
) {
  check_installed("gert")
  include <- arg_match(include)
  check_character(pathspec, allow_null = TRUE)

  staged <- switch(
    include,
    both = NULL,
    staged = TRUE,
    unstaged = FALSE
  )

  status <- gert::git_status(staged = staged, pathspec = pathspec)

  if (nrow(status) == 0) {
    return(btw_tool_result("No changes to report"))
  }

  md_res <- glue_(
    "{{ status$file }} [{{ status$status }}]{{ ifelse(status$staged, ' +staged', ' -unstaged') }}"
  )

  btw_tool_result(
    paste(md_res, collapse = "\n"),
    data = status,
    display = list(markdown = paste("*", md_res, collapse = "\n"))
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
* If a staged file also has unstaged modifications, only the staged file and status are listed.

RETURNS: A list of file paths, their status (new, modified, deleted, etc.), and whether they are staged or unstaged.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Status",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = btw_can_register_git_tool
      ),
      arguments = list(
        include = ellmer::type_enum(
          c("both", "staged", "unstaged"),
          'Optional. One of "both" (default), "staged", or "unstaged" to filter status output.',
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
#' This tool allows an LLM to run [gert::git_diff_patch()], equivalent to
#' `git diff` in the terminal, and to see the detailed changes made in a commit.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'
#'   writeLines("hello, world", "hello.md")
#'   gert::git_add("hello.md")
#'   gert::git_commit("Initial commit")
#'
#'   writeLines("hello, universe", "hello.md")
#'
#'   # What the LLM sees
#'   cat(btw_tool_git_diff()@value)
#' })
#'
#' @inheritParams gert::git_diff_patch
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a diff patch as a formatted string.
#'
#' @family git tools
#' @export
btw_tool_git_diff <- function(ref, `_intent`) {}

btw_tool_git_diff_impl <- function(ref = NULL) {
  check_installed("gert")
  check_string(ref, allow_null = TRUE)

  diff_patch <- gert::git_diff_patch(ref = ref)

  if (length(diff_patch) == 0 || all(!nzchar(diff_patch))) {
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
        "Git Diff%s",
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
      description = r"---(View changes in the working directory or a commit.

WHEN TO USE:
* Use with no arguments to see unstaged changes (working directory vs. index).
* Use with `ref = "HEAD"` to see staged changes (index vs. HEAD).
* Use with `ref = "commit_sha"` to see the diff of that commit.

RETURNS: A unified diff patch showing the changes for a single commit.

LIMITATION: This tool does not support diffing between two arbitrary commits.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Diff",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = btw_can_register_git_tool
      ),
      arguments = list(
        ref = ellmer::type_string(
          'Optional reference like "HEAD" or a commit id. `null` (default) shows unstaged changes.',
          required = FALSE
        )
      )
    )
  }
)

# Git Log ---------------------------------------------------------------------

#' Tool: Git Log
#'
#' This tool allows an LLM to run [gert::git_log()], equivalent to `git log` in
#' the terminal, and to see the commit history of a repository.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'
#'   writeLines("hello, world", "hello.md")
#'   gert::git_add("hello.md")
#'   gert::git_commit("Initial commit")
#'
#'   writeLines("hello, universe", "hello.md")
#'   gert::git_add("hello.md")
#'   gert::git_commit("Update hello.md")
#'
#'   # What the LLM sees
#'   cat(btw_tool_git_log()@value)
#' })
#'
#' @param ref Revision string with a branch/tag/commit value. Defaults to
#'   `"HEAD"`.
#' @param max Maximum number of commits to retrieve. Defaults to 10.
#' @param after Optional date or timestamp: only include commits after this
#'   date.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of commit history.
#'
#' @family git tools
#' @export
btw_tool_git_log <- function(ref, max, after, `_intent`) {}

btw_tool_git_log_impl <- function(
  ref = "HEAD",
  max = 10,
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
  names(log)[names(log) == "files"] <- "n_files"
  fields <- c("message", "author", "time", "n_files", "commit")

  # Truncate commit SHA to 7 characters for display
  log_display <- log
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

  btw_tool_result(
    md_kv_table(log_display[fields]),
    data = log,
    display = list(
      markdown = md_table(log_display[rev(fields)])
    )
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

RETURNS: A list of commits with SHA (short), author, timestamp, number of files, and message.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Log",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = btw_can_register_git_tool
      ),
      arguments = list(
        ref = ellmer::type_string(
          'Revision string (branch/tag/commit). Defaults to "HEAD".',
          required = FALSE
        ),
        max = ellmer::type_number(
          "Maximum number of commits to retrieve. Defaults to 10.",
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
#' This tool allows an LLM stage files and create a git commit. This tool uses a
#' combination of [gert::git_add()] to stage files and [gert::git_commit()] to
#' commit them, which is equivalent to `git add` and `git commit` in the
#' terminal, respectively.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'   writeLines("hello, world", "hello.md")
#'
#'   res <- btw_tool_git_commit("Initial commit", files = "hello.md")
#'
#'   # What the LLM sees
#'   cat(res@value)
#' })
#'
#' @param message A commit message describing the changes.
#' @param files Optional character vector of file paths to stage and commit.
#'   Use `"."` to stage all changed files. If `NULL`, commits currently staged files.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns the commit SHA.
#'
#' @family git tools
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
      title = "Git Commit"
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
* Use this tool to commit changes.
* If `files` is provided, those files will be staged before committing.
* If `files` is NULL, only currently staged files will be committed.

IMPORTANT:
* Always provide a clear, descriptive commit message.
* You can review changes with btw_tool_git_status and btw_tool_git_diff before committing.
* This modifies the repository state.

RETURNS: The commit SHA and confirmation message.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Commit",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = btw_can_register_git_tool
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
#' This tool allows an LLM to list git branches in the repository using
#' [gert::git_branch_list()], equivalent to `git branch` in the terminal.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'   fs::file_touch("hello.md")
#'   gert::git_add("hello.md")
#'   gert::git_commit("Initial commit")
#'
#'   gert::git_branch_create("feature-1")
#'   gert::git_branch_create("feature-2")
#'
#'   # What the LLM sees
#'   cat(btw_tool_git_branch_list()@value)
#' })
#'
#' @param include Once of `"local"` (default), `"remote"`, or `"all"` to filter
#'   branches to local branches only, remote branches only, or all branches.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of branches.
#'
#' @family git tools
#' @export
btw_tool_git_branch_list <- function(include, `_intent`) {}

btw_tool_git_branch_list_impl <- function(
  include = c("local", "remote", "all")
) {
  check_installed("gert")
  include <- arg_match(include)

  local <- switch(include, local = TRUE, remote = FALSE, all = NULL)

  branches <- gert::git_branch_list(local = local)

  if (nrow(branches) == 0) {
    return(btw_tool_result("No branches found"))
  }

  # Select relevant columns
  fields <- c("name", "upstream", "updated")

  branches <- branches[order(branches$updated, decreasing = TRUE), ]

  branches_llm <- glue_(
    "{{ branches$name }} [{{ branches$updated }}]{{ ifelse(!is.na(branches$upstream), paste(' ->', branches$upstream), '') }} "
  )

  btw_tool_result(
    paste(branches_llm, collapse = "\n"),
    data = branches,
    display = list(markdown = md_table(branches[fields]))
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

RETURNS: A table of branch names, upstream tracking, and last update time.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Branches",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = btw_can_register_git_tool
      ),
      arguments = list(
        include = ellmer::type_enum(
          c("local", "remote", "all"),
          'Optional. Filter branches to "local" (default), "remote", or "all".',
          required = FALSE
        )
      )
    )
  }
)

# Git Branch Create -----------------------------------------------------------

#' Tool: Git Branch Create
#'
#' Allows an LLM to create a new git branch using [gert::git_branch_create()],
#' equivalent to `git branch <branch>` in the terminal.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'
#'   fs::file_touch("hello.md")
#'   gert::git_add("hello.md")
#'   gert::git_commit("Initial commit")
#'
#'   # LLM creates a new branch
#'   res <- btw_tool_git_branch_create(branch = "feature/new-analysis")
#'
#'   # What the LLM sees
#'   cat(res@value)
#' })
#'
#' @param branch Name of the new branch to create.
#' @param ref Optional reference point for the new branch. Defaults to `"HEAD"`.
#' @param checkout Whether to check out the new branch after creation. Defaults
#'   to `TRUE`.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a confirmation message.
#'
#' @family git tools
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
    "Created branch `%s` from `%s`%s.",
    branch,
    ref,
    if (checkout) " and checked it out" else ""
  )

  btw_tool_result(
    result,
    display = list(
      markdown = result,
      title = HTML(sprintf(
        "Git Create Branch <code>%s</code>",
        branch
      ))
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
        btw_can_register = btw_can_register_git_tool
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
#' Allows an LLM to switch to a different git branch using
#' [gert::git_branch_checkout()], equivalent to `git checkout <branch>` in
#' the terminal.
#'
#' @examplesIf rlang::is_installed("gert")
#' withr::with_tempdir({
#'   gert::git_init()
#'   fs::file_touch("hello.md")
#'
#'   gert::git_add("hello.md")
#'   gert::git_commit("Initial commit")
#'
#'   gert::git_branch_create("feature-1")
#'
#'   # LLM checks out an existing branch
#'   res <- btw_tool_git_branch_checkout(branch = "feature-1")
#'
#'   # What the LLM sees
#'   cat(res@value)
#' })
#'
#' @param branch Name of branch to check out.
#' @param force Whether to force checkout even with uncommitted changes.
#'   Defaults to `FALSE`.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a confirmation message.
#'
#' @family git tools
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
* Check btw_tool_git_status first to ensure no uncommitted changes will be lost.
* Use btw_tool_git_branch_list to see available branches.

IMPORTANT:
* This modifies the repository state and working directory.
* Will fail if there are uncommitted changes unless force is true.
* Using `force: true` can lose uncommitted changes.

RETURNS: Confirmation message with branch name.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Git Checkout",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = btw_can_register_git_tool
      ),
      arguments = list(
        branch = ellmer::type_string(
          "Name of the branch to check out."
        ),
        force = ellmer::type_boolean(
          "Whether to force checkout even with uncommitted changes. Defaults to false. Use with caution.",
          required = FALSE
        )
      )
    )
  }
)
