test_that("deprecated session tools emit deprecation warnings", {
  expect_warning(
    btw_tool_session_platform_info(),
    class = "lifecycle_warning_deprecated"
  )

  expect_warning(
    btw_tool_session_package_info(packages = "btw"),
    class = "lifecycle_warning_deprecated"
  )

  expect_warning(
    btw_tool_session_check_package_installed(package_name = "btw"),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("deprecated search tools emit deprecation warnings", {
  skip_if_offline()

  expect_warning(
    btw_tool_search_packages(query = "shiny"),
    class = "lifecycle_warning_deprecated"
  )

  expect_warning(
    btw_tool_search_package_info(package_name = "btw"),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("deprecated file tools emit deprecation warnings", {
  withr::with_tempdir({
    writeLines("test content", "test.txt")

    expect_warning(
      btw_tool_files_list_files(),
      class = "lifecycle_warning_deprecated"
    )

    expect_warning(
      btw_tool_files_read_text_file(path = "test.txt"),
      class = "lifecycle_warning_deprecated"
    )

    expect_warning(
      btw_tool_files_write_text_file(path = "test2.txt", content = "hello"),
      class = "lifecycle_warning_deprecated"
    )
  })
})

test_that("btw_tools() with old group names resolves to new tools", {
  local_enable_tools()

  # Old group names should still work (resolve to new tools)
  # Suppress warnings since we're just checking functionality here
  withr::local_options(lifecycle_verbosity = "quiet")

  tools_session <- btw_tools("session")
  tools_sessioninfo <- btw_tools("sessioninfo")

  # Both should return the same tools

  expect_equal(
    sort(names(tools_session)),
    sort(names(tools_sessioninfo))
  )

  tools_search <- btw_tools("search")
  tools_cran <- btw_tools("cran")

  expect_equal(
    sort(names(tools_search)),
    sort(names(tools_cran))
  )
})

test_that("btw_tools() with old tool names resolves to new tools", {
  local_enable_tools()

  # Old tool names should still work (resolve to new tools)
  # Suppress warnings since we're just checking functionality here
  withr::local_options(lifecycle_verbosity = "quiet")

  # Session platform info
  tools_old <- btw_tools("btw_tool_session_platform_info")
  tools_new <- btw_tools("btw_tool_sessioninfo_platform")
  expect_equal(names(tools_old), names(tools_new))

  # Files list
  tools_old <- btw_tools("btw_tool_files_list_files")
  tools_new <- btw_tools("btw_tool_files_list")
  expect_equal(names(tools_old), names(tools_new))

  # Session check package installed
  tools_old <- btw_tools("btw_tool_session_check_package_installed")
  tools_new <- btw_tools("btw_tool_sessioninfo_is_package_installed")
  expect_equal(names(tools_old), names(tools_new))
})

test_that("new sessioninfo tools work without warnings", {
  expect_no_warning(btw_tool_sessioninfo_platform())
  expect_no_warning(btw_tool_sessioninfo_package(packages = "btw"))
  expect_no_warning(btw_tool_sessioninfo_is_package_installed(package_name = "btw"))
})

test_that("new cran tools work without warnings", {
  skip_if_offline()

  expect_no_warning(btw_tool_cran_search(query = "shiny"))
  expect_no_warning(btw_tool_cran_package(package_name = "btw"))
})

test_that("new file tools work without warnings", {
  withr::with_tempdir({
    writeLines("test content", "test.txt")

    expect_no_warning(btw_tool_files_list())
    expect_no_warning(btw_tool_files_read(path = "test.txt"))
    expect_no_warning(btw_tool_files_write(path = "test2.txt", content = "hello"))
  })
})

test_that("btw_tools() with new group names works without warnings", {
  local_enable_tools()

  expect_no_warning(tools_sessioninfo <- btw_tools("sessioninfo"))
  expect_no_warning(tools_cran <- btw_tools("cran"))
  expect_no_warning(tools_files <- btw_tools("files"))

  expect_true(length(tools_sessioninfo) > 0)
  expect_true(length(tools_cran) > 0)
  expect_true(length(tools_files) > 0)
})

test_that("btw_tools() with new tool names works without warnings", {
  local_enable_tools()

  expect_no_warning(btw_tools("btw_tool_sessioninfo_platform"))
  expect_no_warning(btw_tools("sessioninfo_platform"))
  expect_no_warning(btw_tools("btw_tool_sessioninfo_is_package_installed"))
  expect_no_warning(btw_tools("sessioninfo_is_package_installed"))
  expect_no_warning(btw_tools("btw_tool_cran_search"))
  expect_no_warning(btw_tools("cran_search"))
  expect_no_warning(btw_tools("btw_tool_files_list"))
  expect_no_warning(btw_tools("files_list"))
})
