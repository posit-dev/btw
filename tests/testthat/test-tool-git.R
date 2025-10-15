test_that("btw_tool_git_status()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Empty repo - no changes
  result <- btw_tool_git_status()
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "No changes")

  # Create a new file
  writeLines("test content", "test.txt")

  result <- btw_tool_git_status()
  expect_btw_tool_result(result)
  expect_match(result@value, "test\\.txt")
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Check staged only (should be empty)
  result_staged_empty <- btw_tool_git_status(staged = TRUE)
  expect_btw_tool_result(result_staged_empty, has_data = FALSE)
  expect_match(result_staged_empty@value, "No changes")
  expect_snapshot(
    cli::cat_line(result_staged_empty@value),
    transform = scrub_git_details
  )

  # Stage the file
  gert::git_add("test.txt")

  # Check staged only
  result_staged <- btw_tool_git_status(staged = TRUE)
  expect_match(result_staged@value, "test\\.txt")
  expect_snapshot(
    cli::cat_line(result_staged@value),
    transform = scrub_git_details
  )

  # Check unstaged only (should be empty)
  result_unstaged <- btw_tool_git_status(staged = FALSE)
  expect_btw_tool_result(result_unstaged, has_data = FALSE)
  expect_match(result_unstaged@value, "No changes")

  cat("\nmore content", file = "test.txt", append = TRUE)
  expect_snapshot(
    cli::cat_line(btw_tool_git_status()@value),
    transform = scrub_git_details
  )
  expect_snapshot(
    cli::cat_line(btw_tool_git_status(staged = FALSE)@value),
    transform = scrub_git_details
  )

  gert::git_commit("Add test.txt")
  expect_snapshot(
    cli::cat_line(btw_tool_git_status()@value),
    transform = scrub_git_details
  )

  gert::git_add("test.txt")
  expect_snapshot(
    cli::cat_line(btw_tool_git_status()@value),
    transform = scrub_git_details
  )
})

test_that("btw_tool_git_diff()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # No changes initially
  result <- btw_tool_git_diff()
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "No.*changes")

  # Create and stage a file
  writeLines("line 1", "test.txt")
  gert::git_add("test.txt")
  gert::git_commit("Initial commit")

  # Modify the file
  writeLines(c("line 1", "line 2"), "test.txt")

  # Check unstaged diff (working dir vs index)
  result <- btw_tool_git_diff()
  expect_btw_tool_result(result, has_data = FALSE)
  expect_true(any(grepl("line 2", result@value, fixed = TRUE)))
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Stage the changes
  gert::git_add("test.txt")

  # Check staged diff (index vs HEAD) - shows both lines being modified from HEAD
  result_staged <- btw_tool_git_diff(ref = "HEAD")
  expect_btw_tool_result(result_staged, has_data = FALSE)
  # The staged diff shows the change from HEAD, which includes adding line 2
  expect_true(any(grepl("@@", result_staged@value, fixed = TRUE))) # Has diff hunks

  # Commit and verify no more unstaged changes
  gert::git_commit("Add line 2")
  result_no_changes <- btw_tool_git_diff()
  expect_btw_tool_result(result_no_changes, has_data = FALSE)
  expect_match(result_no_changes@value, "No.*changes")
})

test_that("btw_tool_git_log()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Create initial commit
  writeLines("test", "test.txt")
  gert::git_add("test.txt")
  gert::git_commit("Initial commit")

  result <- btw_tool_git_log()
  expect_btw_tool_result(result)
  expect_match(result@value, "Initial commit")
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Test max parameter
  writeLines("test2", "test2.txt")
  gert::git_add("test2.txt")
  gert::git_commit("Second commit")

  result_limited <- btw_tool_git_log(max = 1)
  expect_btw_tool_result(result_limited)
  expect_match(result_limited@value, "Second commit")
})

test_that("btw_tool_git_commit()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Create a file
  writeLines("test content", "test.txt")

  # Commit with files parameter
  result <- btw_tool_git_commit(
    message = "Add test file",
    files = "test.txt"
  )
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "Add test file")
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Verify commit was created
  log <- gert::git_log(max = 1)
  expect_equal(nrow(log), 1)
  expect_match(log$message, "Add test file")

  # Test committing already staged files
  writeLines("more content", "test2.txt")
  gert::git_add("test2.txt")

  result2 <- btw_tool_git_commit(
    message = "Add second file",
    files = NULL
  )
  expect_btw_tool_result(result2, has_data = FALSE)
  expect_match(result2@value, "Add second file")
})

test_that("btw_tool_git_branch_list()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Create initial commit (needed for branches to show up)
  writeLines("test", "test.txt")
  gert::git_add("test.txt")
  gert::git_commit("Initial commit")

  result <- btw_tool_git_branch_list()
  expect_btw_tool_result(result)
  # Should show at least the default branch (main or master)
  expect_true(nrow(result@extra$data) >= 1)
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Create a new branch
  gert::git_branch_create("feature", checkout = FALSE)

  result2 <- btw_tool_git_branch_list()
  expect_btw_tool_result(result2)
  expect_true(nrow(result2@extra$data) >= 2)
  expect_match(result2@value, "feature")
})

test_that("btw_tool_git_branch_create()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Create initial commit
  writeLines("test", "test.txt")
  gert::git_add("test.txt")
  gert::git_commit("Initial commit")

  result <- btw_tool_git_branch_create(
    branch = "feature-branch",
    checkout = FALSE
  )
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "feature-branch")
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Verify branch exists
  branches <- gert::git_branch_list()
  expect_true("feature-branch" %in% branches$name)
})

test_that("btw_tool_git_branch_checkout()", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Create initial commit
  writeLines("test", "test.txt")
  gert::git_add("test.txt")
  gert::git_commit("Initial commit")

  # Get current branch name
  current_branch <- gert::git_branch()

  # Create and checkout new branch
  gert::git_branch_create("feature", checkout = FALSE)

  result <- btw_tool_git_branch_checkout(branch = "feature")
  expect_btw_tool_result(result, has_data = FALSE)
  expect_match(result@value, "feature")
  expect_snapshot(cli::cat_line(result@value), transform = scrub_git_details)

  # Verify we're on the new branch
  expect_equal(gert::git_branch(), "feature")

  # Checkout back to original branch
  result2 <- btw_tool_git_branch_checkout(branch = current_branch)
  expect_btw_tool_result(result2, has_data = FALSE)
  expect_equal(gert::git_branch(), current_branch)
})

test_that("git tools require gert to be installed", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  local_mocked_bindings(
    is_installed = function(pkg) pkg != "gert"
  )

  expect_error(
    btw_tool_git_status(),
    "gert"
  )

  expect_error(
    btw_tool_git_diff(),
    "gert"
  )

  expect_error(
    btw_tool_git_log(),
    "gert"
  )

  expect_error(
    btw_tool_git_commit(message = "test", files = "test.txt"),
    "gert"
  )

  expect_error(
    btw_tool_git_branch_list(),
    "gert"
  )

  expect_error(
    btw_tool_git_branch_create(branch = "test"),
    "gert"
  )

  expect_error(
    btw_tool_git_branch_checkout(branch = "main"),
    "gert"
  )
})

test_that("git tools validate arguments", {
  skip_if_not_installed("gert")
  local_temp_git_repo()

  # btw_tool_git_status
  expect_error(btw_tool_git_status(staged = "invalid"))
  expect_error(btw_tool_git_status(pathspec = 123))

  # btw_tool_git_diff
  expect_error(btw_tool_git_diff(ref = 123))

  # btw_tool_git_log
  expect_error(btw_tool_git_log(ref = 123))
  expect_error(btw_tool_git_log(max = "invalid"))
  expect_error(btw_tool_git_log(max = 0))

  # btw_tool_git_commit
  expect_error(btw_tool_git_commit(message = 123))
  expect_error(btw_tool_git_commit(message = "test", files = 123))

  # btw_tool_git_branch_list
  expect_error(btw_tool_git_branch_list(local = "invalid"))

  # btw_tool_git_branch_create
  expect_error(btw_tool_git_branch_create(branch = 123))
  expect_error(btw_tool_git_branch_create(branch = "test", ref = 123))
  expect_error(
    btw_tool_git_branch_create(branch = "test", checkout = "invalid")
  )

  # btw_tool_git_branch_checkout
  expect_error(btw_tool_git_branch_checkout(branch = 123))
  expect_error(btw_tool_git_branch_checkout(branch = "test", force = "invalid"))
})

test_that("git tools work together", {
  skip_if_not_installed("gert")
  local_temp_git_repo()

  # Workflow 1: Check status, stage, commit, verify log
  # Create some files
  writeLines("Initial content", "file1.txt")
  writeLines("More content", "file2.txt")

  # Check status to see untracked files
  status1 <- btw_tool_git_status()
  expect_match(status1@value, "file1\\.txt")
  expect_match(status1@value, "file2\\.txt")

  # Stage and commit first file
  commit1 <- btw_tool_git_commit(
    message = "Add file1",
    files = "file1.txt"
  )
  expect_btw_tool_result(commit1, has_data = FALSE)

  # Verify log shows the commit
  log1 <- btw_tool_git_log(max = 1)
  expect_match(log1@value, "Add file1")

  # Extract commit SHA from log data to use in diff
  commit_sha <- log1@extra$data$commit[1]
  expect_false(grepl("[^a-f0-9]", commit_sha)) # Should be hex SHA

  # Workflow 2: Make changes, check diff, stage, commit
  # Modify file1
  writeLines(c("Initial content", "Added line"), "file1.txt")

  # Check diff shows unstaged changes
  diff_unstaged <- btw_tool_git_diff()
  expect_btw_tool_result(diff_unstaged, has_data = FALSE)
  # Should show a diff with changes
  expect_true(any(grepl("@@", diff_unstaged@value, fixed = TRUE)))
  expect_snapshot(
    cli::cat_line(diff_unstaged@value),
    transform = scrub_git_details
  )

  # Stage the changes
  gert::git_add("file1.txt")

  # Check diff shows staged changes
  diff_staged <- btw_tool_git_diff(ref = "HEAD")
  expect_btw_tool_result(diff_staged, has_data = FALSE)
  # Should show a diff with changes
  expect_true(any(grepl("@@", diff_staged@value, fixed = TRUE)))
  expect_snapshot(
    cli::cat_line(diff_staged@value),
    transform = scrub_git_details
  )

  # Commit the changes
  commit2 <- btw_tool_git_commit(
    message = "Update file1",
    files = NULL # Already staged
  )
  expect_btw_tool_result(commit2, has_data = FALSE)

  # Workflow 3: Use log to find commits
  # Get log of both commits
  log_all <- btw_tool_git_log(max = 10)
  expect_btw_tool_result(log_all)
  expect_equal(nrow(log_all@extra$data), 2)
  expect_match(log_all@value, "Add file1")
  expect_match(log_all@value, "Update file1")
  expect_snapshot(cli::cat_line(log_all@value), transform = scrub_git_details)

  # Get commits SHAs from log - verifies we can use log data
  commit_shas <- log_all@extra$data$commit
  expect_equal(length(commit_shas), 2)
  # All should be hex SHAs
  expect_true(!any(grepl("[^a-f0-9]", commit_shas)))

  # Workflow 4: Multiple file workflow with status tracking
  # Add second file
  commit3 <- btw_tool_git_commit(
    message = "Add file2",
    files = "file2.txt"
  )
  expect_btw_tool_result(commit3, has_data = FALSE)

  # Create third file but don't commit
  writeLines("Third file", "file3.txt")

  # Status should show file3 as untracked, others committed
  status_final <- btw_tool_git_status()
  expect_match(status_final@value, "file3\\.txt")
  expect_no_match(status_final@value, "file1\\.txt")
  expect_no_match(status_final@value, "file2\\.txt")
  expect_snapshot(
    cli::cat_line(status_final@value),
    transform = scrub_git_details
  )

  # Verify log shows all three commits
  log_final <- btw_tool_git_log(max = 10)
  expect_equal(nrow(log_final@extra$data), 3)
  expect_match(log_final@value, "Add file1")
  expect_match(log_final@value, "Update file1")
  expect_match(log_final@value, "Add file2")
  expect_snapshot(cli::cat_line(log_final@value), transform = scrub_git_details)
})
