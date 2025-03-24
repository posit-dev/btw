test_that("btw_tool_session_platform_info() works", {
  with_mocked_platform({
    platform_description <- btw_tool_session_platform_info()
  })

  expect_snapshot(cat(platform_description), transform = scrub_system_info)
})

test_that("btw_tool_session_platform_info() detects RStudio, Positron, VS Code", {
  withr::with_envvar(list(POSITRON = 1), {
    expect_match(btw_tool_session_platform_info(), "UI: Positron")
  })

  withr::with_envvar(list(POSITRON = "", RSTUDIO = 1), {
    expect_match(btw_tool_session_platform_info(), "UI: RStudio")
  })

  withr::with_envvar(
    list(POSITRON = "", RSTUDIO = "", TERM_PROGRAM = "vscode"),
    {
      expect_match(btw_tool_session_platform_info(), "UI: VS Code")
    }
  )
})

test_that("btw_tool_session_package_info()", {
  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["dplyr"]],
    expect_snapshot(cat(btw_tool_session_package_info("dplyr")))
  )

  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["digest"]],
    expect_snapshot(cat(btw_tool_session_package_info(
      "digest",
      c("Imports", "Suggests")
    )))
  )

  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["attached"]],
    expect_snapshot(cat(btw_tool_session_package_info("attached")))
  )

  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["loaded"]],
    expect_snapshot(cat(btw_tool_session_package_info("loaded")))
  )
})
