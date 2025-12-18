test_that("btw_skills_system_prompt() works", {
  skip_if_not_snapshot_env()
  expect_snapshot(cat(btw_skills_system_prompt()))
})

test_that("skills prompt is included in btw_client() system prompt", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "beep"))

  with_mocked_platform(ide = "rstudio", {
    chat <- btw_client(path_btw = FALSE)
  })

  system_prompt <- chat$get_system_prompt()

  expect_match(system_prompt, "## Skills", fixed = TRUE)
  expect_match(system_prompt, "<name>skill-creator</name>", fixed = TRUE)
})
