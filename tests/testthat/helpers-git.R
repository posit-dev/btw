local_temp_git_repo <- function(
  user_name = "Test User",
  user_email = "test@example.com",
  .local_envir = parent.frame()
) {
  repo <- withr::local_tempdir(.local_envir = .local_envir)
  withr::local_dir(repo, .local_envir = .local_envir)
  gert::git_init(".")

  # Configure user if not set
  if (!gert::user_is_configured()) {
    gert::git_config_set("user.name", user_name)
    gert::git_config_set("user.email", user_email)
  }

  invisible(repo)
}
