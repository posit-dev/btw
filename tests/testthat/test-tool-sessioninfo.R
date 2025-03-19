test_that("btw_tool_describe_platform() works", {
  with_mocked_platform({
    platform_description <- btw_tool_describe_platform()
  })

  expect_snapshot(cat(platform_description), transform = scrub_system_info)
})

test_that("btw_tool_describe_platform() detects RStudio, Positron, VS Code", {
  withr::with_envvar(list(POSITRON = 1), {
    expect_match(btw_tool_describe_platform(), "UI: Positron")
  })

  withr::with_envvar(list(POSITRON = "", RSTUDIO = 1), {
    expect_match(btw_tool_describe_platform(), "UI: RStudio")
  })

  withr::with_envvar(
    list(POSITRON = "", RSTUDIO = "", TERM_PROGRAM = "vscode"),
    {
      expect_match(btw_tool_describe_platform(), "UI: VS Code")
    }
  )
})
