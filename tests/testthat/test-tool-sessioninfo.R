local_sessioninfo_quarto_version()

test_that("btw_tool_sessioninfo_platform() works", {
  with_mocked_platform({
    platform_data <- platform_info()
    platform_description <- btw_tool_sessioninfo_platform()
  })

  expect_btw_tool_result(platform_description, has_data = FALSE)
  for (nm in names(platform_description@extra)) {
    expect_equal(
      platform_description@extra[[!!nm]],
      platform_data[[!!toupper(nm)]]
    )
  }

  expect_snapshot(
    cat(platform_description@value),
    transform = scrub_system_info
  )
})

test_that("btw_tool_sessioninfo_platform() detects RStudio, Positron, VS Code", {
  withr::with_envvar(list(POSITRON = 1), {
    expect_match(btw_tool_sessioninfo_platform()@value, "UI: Positron")
  })

  withr::with_envvar(list(POSITRON = "", RSTUDIO = 1), {
    expect_match(btw_tool_sessioninfo_platform()@value, "UI: RStudio")
  })

  withr::with_envvar(
    list(POSITRON = "", RSTUDIO = "", TERM_PROGRAM = "vscode"),
    {
      expect_match(btw_tool_sessioninfo_platform()@value, "UI: VS Code")
    }
  )
})

test_that("btw_tool_sessioninfo_package()", {
  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["dplyr"]],
    {
      expect_btw_tool_result(btw_tool_sessioninfo_package("dplyr"))
      expect_snapshot(cat(btw_tool_sessioninfo_package("dplyr")@value))
    }
  )

  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["digest"]],
    {
      expect_btw_tool_result(
        btw_tool_sessioninfo_package("digest", c("Imports", "Suggests"))
      )
      expect_snapshot(cat(
        btw_tool_sessioninfo_package("digest", c("Imports", "Suggests"))@value
      ))
    }
  )

  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["attached"]],
    {
      expect_btw_tool_result(btw_tool_sessioninfo_package("attached"))
      expect_snapshot(cat(
        btw_tool_sessioninfo_package("attached")@value
      ))
    }
  )

  with_mocked_bindings(
    package_info = function(...) package_info_mock_results[["loaded"]],
    {
      expect_btw_tool_result(btw_tool_sessioninfo_package("loaded"))
      expect_snapshot(cat(
        btw_tool_sessioninfo_package("loaded")@value
      ))
    }
  )

  with_mocked_bindings(
    package_info = function(packages, dependencies) {
      stopifnot(
        identical(packages, c("dplyr", "tidyr")),
        isFALSE(dependencies)
      )
      package_info_mock_results[["dplyr,tidyr"]]
    },
    {
      expect_btw_tool_result(
        btw_tool_sessioninfo_package("dplyr,tidyr", "false"),
      )
      expect_snapshot(cat(
        btw_tool_sessioninfo_package("dplyr,tidyr", "false")@value
      ))
    }
  )
})

test_that("btw_this('@attached_packages')", {
  expect_type(btw_this("@attached_packages"), "character")
  expect_equal(
    btw_this("@attached_packages"),
    I(btw_tool_sessioninfo_package("attached")@value)
  )
})

test_that("btw_this('@loaded_packages')", {
  expect_type(btw_this("@loaded_packages"), "character")
  expect_equal(
    btw_this("@loaded_packages"),
    I(btw_tool_sessioninfo_package("loaded")@value)
  )
})

test_that("btw_this('@installed_packages')", {
  local_mocked_bindings(
    package_info = function(x, ...) data.frame(packages = x)
  )

  expect_type(btw_this("@installed_packages"), "character")
  expect_equal(
    btw_this("@installed_packages"),
    I(btw_tool_sessioninfo_package("installed")@value)
  )
})
