local_temp_git_repo <- function(
  user_name = "Test User",
  user_email = "test@example.com",
  .local_envir = parent.frame()
) {
  repo <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(repo, .local_envir = .local_envir)
  gert::git_init(".")

  # Set default branch to 'main' instead of 'master'
  gert::git_config_set("init.defaultBranch", "main")
  . <- system("git symbolic-ref HEAD refs/heads/main", intern = TRUE)

  gert::git_config_set("user.name", user_name)
  gert::git_config_set("user.email", user_email)

  invisible(repo)
}

local_git_info <- function(mock_has_git = TRUE, .local_envir = parent.frame()) {
  mock_git_info <- function() {
    if (!mock_has_git) {
      stop("Not in a git repository or gert cannot access git.")
    }

    list(
      path = "/path/to/repo",
      bare = FALSE,
      head = "refs/heads/main",
      shorthand = "main",
      commit = "abcdef01234567890",
      remote = "origin",
      upstream = "origin/main",
      reflist = c("refs/heads/main", "refs/remotes/origin/main")
    )
  }

  local_mocked_bindings(
    git_info = mock_git_info,
    .package = "gert",
    .env = .local_envir
  )
}

scrub_git_details <- function(x) {
  # Replace timestamps (YYYY-MM-DD HH:MM:SS format)
  x <- gsub(
    "\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}",
    "2025-10-11 12:13:14",
    x
  )

  # Replace 7-character commit SHAs
  x <- gsub(
    "\\b[0-9a-f]{7}\\b",
    "abcd123",
    x
  )

  x
}
