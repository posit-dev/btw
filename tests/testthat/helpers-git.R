local_temp_git_repo <- function(
  user_name = "Test User",
  user_email = "test@example.com",
  .local_envir = parent.frame()
) {
  repo <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(repo, .local_envir = .local_envir)
  gert::git_init(".")

  gert::git_config_set("init.defaultBranch", "main")
  gert::git_config_set("user.name", user_name)
  gert::git_config_set("user.email", user_email)

  invisible(repo)
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
