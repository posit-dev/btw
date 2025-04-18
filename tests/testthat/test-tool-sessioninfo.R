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

  with_mocked_bindings(
    package_info = function(packages, dependencies) {
      stopifnot(
        identical(packages, c("dplyr", "tidyr")),
        isFALSE(dependencies)
      )
      package_info_mock_results[["dplyr,tidyr"]]
    },
    expect_snapshot(cat(btw_tool_session_package_info("dplyr,tidyr", "false")))
  )
})

test_that("btw_this('@attached_packages')", {
  expect_equal(
    btw_this("@attached_packages"),
    I(btw_tool_session_package_info("attached"))
  )
})

test_that("btw_this('@loaded_packages')", {
  expect_equal(
    btw_this("@loaded_packages"),
    I(btw_tool_session_package_info("loaded"))
  )
})

test_that("btw_this('@installed_packages')", {
  expect_equal(
    btw_this("@installed_packages"),
    I(btw_tool_session_package_info("installed"))
  )
})
