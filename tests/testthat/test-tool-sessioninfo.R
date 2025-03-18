test_that("btw_tool_describe_platform() works", {
  local_mocked_bindings(
    platform_date = function() "DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)"
  )

  withr::local_locale(c(LC_COLLATE = "fr_FR.UTF-8", LC_CTYPE = "fr_FR.UTF-8"))
  withr::local_timezone("Europe/Paris")
  withr::local_language("FR")

  withr::local_envvar(list(POSITRON = 1))

  expect_snapshot(
    cat(btw_tool_describe_platform()),
    transform = function(x) {
      x <- sub(R.version.string, "R VERSION", x, fixed = TRUE)
      x <- sub(sessioninfo::os_name(), "OPERATING SYSTEM", x, fixed = TRUE)
      x <- sub(version$system, "SYSTEM VERSION", x, fixed = TRUE)
      x
    }
  )
})

test_that("btw_tool_describe_platform() detects RStudio, Positron, VS Code", {
  withr::with_envvar(list(POSITRON = 1), {
    expect_match(btw_tool_describe_platform(), "is Positron")
  })

  withr::with_envvar(list(POSITRON = "", RSTUDIO = 1), {
    expect_match(btw_tool_describe_platform(), "is RStudio")
  })

  withr::with_envvar(
    list(POSITRON = "", RSTUDIO = "", TERM_PROGRAM = "vscode"),
    {
      expect_match(btw_tool_describe_platform(), "is VS Code")
    }
  )
})
