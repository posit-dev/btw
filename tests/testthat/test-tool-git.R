test_that("btw_tool_git_status() works", {
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

  # Stage the file
  gert::git_add("test.txt")

  # Check staged only
  result_staged <- btw_tool_git_status(staged = TRUE)
  expect_match(result_staged@value, "test\\.txt")

  # Check unstaged only (should be empty)
  result_unstaged <- btw_tool_git_status(staged = FALSE)
  expect_btw_tool_result(result_unstaged, has_data = FALSE)
  expect_match(result_unstaged@value, "No changes")
})

test_that("btw_tool_git_diff() works", {
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

  # Stage the changes
  gert::git_add("test.txt")

  # Check staged diff (index vs HEAD) - shows both lines being modified from HEAD
  result_staged <- btw_tool_git_diff(ref = "HEAD")
  expect_btw_tool_result(result_staged, has_data = FALSE)
  # The staged diff shows the change from HEAD, which includes adding line 2
  expect_true(any(grepl("@@", result_staged@value, fixed = TRUE)))  # Has diff hunks

  # Commit and verify no more unstaged changes
  gert::git_commit("Add line 2")
  result_no_changes <- btw_tool_git_diff()
  expect_btw_tool_result(result_no_changes, has_data = FALSE)
  expect_match(result_no_changes@value, "No.*changes")
})

test_that("btw_tool_git_log() works", {
  skip_if_not_installed("gert")

  local_temp_git_repo()

  # Create initial commit
  writeLines("test", "test.txt")
  gert::git_add("test.txt")
  gert::git_commit("Initial commit")

  result <- btw_tool_git_log()
  expect_btw_tool_result(result)
  expect_match(result@value, "Initial commit")

  # Test max parameter
  writeLines("test2", "test2.txt")
  gert::git_add("test2.txt")
  gert::git_commit("Second commit")

  result_limited <- btw_tool_git_log(max = 1)
  expect_btw_tool_result(result_limited)
  expect_match(result_limited@value, "Second commit")
})

test_that("btw_tool_git_commit() works", {
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

test_that("btw_tool_git_branch_list() works", {
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

  # Create a new branch
  gert::git_branch_create("feature", checkout = FALSE)

  result2 <- btw_tool_git_branch_list()
  expect_btw_tool_result(result2)
  expect_true(nrow(result2@extra$data) >= 2)
  expect_match(result2@value, "feature")
})

test_that("btw_tool_git_branch_create() works", {
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

  # Verify branch exists
  branches <- gert::git_branch_list()
  expect_true("feature-branch" %in% branches$name)
})

test_that("btw_tool_git_branch_checkout() works", {
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

  # Verify we're on the new branch
  expect_equal(gert::git_branch(), "feature")

  # Checkout back to original branch
  result2 <- btw_tool_git_branch_checkout(branch = current_branch)
  expect_btw_tool_result(result2, has_data = FALSE)
  expect_equal(gert::git_branch(), current_branch)
})

test_that("git tools require gert to be installed", {
  skip_if_not_installed("gert")
  skip("Manual test only - requires gert to be uninstalled")

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
  expect_error(btw_tool_git_branch_create(branch = "test", checkout = "invalid"))

  # btw_tool_git_branch_checkout
  expect_error(btw_tool_git_branch_checkout(branch = 123))
  expect_error(btw_tool_git_branch_checkout(branch = "test", force = "invalid"))
})
