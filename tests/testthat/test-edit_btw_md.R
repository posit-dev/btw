test_that("use_btw_md() creates btw.md in project scope", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  # Create a project marker
  writeLines("test", "DESCRIPTION")

  local_mocked_bindings(
    use_build_ignore_btw_md = function(...) invisible()
  )

  expect_snapshot(
    path <- use_btw_md("project")
  )

  expect_true(fs::file_exists("btw.md"))
  expect_equal(basename(path), "btw.md")
  expect_true(fs::file_exists(path))

  # Check content has YAML frontmatter
  content <- readLines("btw.md")
  expect_match(content[1], "^---$")
  expect_match(content[2], "^client:")
  expect_match(content[3], "^tools:")
})

test_that("use_btw_md() creates btw.md in user scope", {
  wd <- withr::local_tempdir()

  local_mocked_bindings(
    path_home = function(...) fs::path(wd, ...),
    .package = "fs"
  )
  local_mocked_bindings(
    use_build_ignore_btw_md = function(...) invisible()
  )

  expect_snapshot(
    path <- use_btw_md("user")
  )

  expect_true(fs::file_exists(fs::path(wd, "btw.md")))
  expect_equal(basename(path), "btw.md")
})

test_that("use_btw_md() creates btw.md in sub-directory path", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  subdir <- fs::path(wd, "subdir")
  fs::dir_create(subdir)

  local_mocked_bindings(
    use_build_ignore_btw_md = function(...) invisible()
  )

  expect_snapshot(
    path <- use_btw_md(subdir)
  )
  expect_true(fs::file_exists(fs::path(subdir, "btw.md")))
  expect_equal(fs::path_norm(path), fs::path_norm(fs::path(subdir, "btw.md")))
})

test_that("use_btw_md() creates AGENTS.md with correct template", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  local_mocked_bindings(
    use_build_ignore_btw_md = function(...) invisible()
  )

  expect_snapshot(
    path <- use_btw_md("AGENTS.md")
  )

  expect_true(fs::file_exists("AGENTS.md"))
  expect_equal(basename(path), "AGENTS.md")

  # Check content does NOT have YAML frontmatter
  content <- readLines("AGENTS.md")
  expect_false(any(grepl("^---$", content)))
  expect_false(any(grepl("client:", content)))
  expect_true(any(grepl("tidyverse", content)))
})

test_that("use_btw_md() does not overwrite existing file", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  local_mocked_bindings(
    use_build_ignore_btw_md = function(...) invisible()
  )

  writeLines("existing content", "btw.md")

  expect_snapshot(
    path <- use_btw_md("btw.md")
  )

  content <- readLines("btw.md")
  expect_equal(content, "existing content")
})

test_that("use_btw_md() adds to .Rbuildignore in R package", {
  skip_if_not_installed("usethis")

  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  # Create a package structure
  writeLines("Package: testpkg", "DESCRIPTION")
  usethis::ui_silence({
    usethis::local_project(wd)
    suppressMessages(path <- use_btw_md("project"))
  })

  # Check .Rbuildignore was created and contains btw.md
  if (fs::file_exists(".Rbuildignore")) {
    buildignore <- readLines(".Rbuildignore")
    expect_match(buildignore, "btw\\.md", fixed = TRUE)
  }
})

test_that("use_btw_md() handles .Rbuildignore even when file exists", {
  skip_if_not_installed("usethis")

  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  # Create a package structure
  writeLines("Package: testpkg", "DESCRIPTION")

  # Create existing btw.md
  writeLines("existing", "btw.md")

  # Should still add to .Rbuildignore
  usethis::ui_silence({
    usethis::local_project(wd)
    suppressMessages(use_btw_md("project"))
  })

  if (fs::file_exists(".Rbuildignore")) {
    buildignore <- readLines(".Rbuildignore")
    expect_match(buildignore, "btw\\.md", fixed = TRUE)
  }
})

test_that("edit_btw_md() errors if file doesn't exist", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  expect_error(
    edit_btw_md("nonexistent.md"),
    "does not exist"
  )
})

test_that("edit_btw_md() without args finds file like btw_client", {
  wd <- withr::local_tempdir(tmpdir = file.path(tempdir(), "btw-test"))
  withr::local_dir(wd)

  # Create btw.md in parent
  writeLines("project config", file.path(wd, "..", "btw.md"))

  local_mocked_bindings(
    is_installed = function(pkg) pkg != "rstudioapi",
    .package = "rlang"
  )

  with_mocked_bindings(
    file.edit = function(file) {
      expect_true(fs::file_exists(file))
      expect_match(basename(file), "btw\\.md")
    },
    .package = "utils",
    suppressMessages(edit_btw_md())
  )
})

test_that("edit_btw_md() finds AGENTS.md if btw.md not found", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  writeLines("agents config", "AGENTS.md")

  local_mocked_bindings(
    is_installed = function(pkg) pkg != "rstudioapi",
    .package = "rlang"
  )
  with_mocked_bindings(
    file.edit = function(file) {
      expect_equal(basename(file), "AGENTS.md")
    },
    .package = "utils",
    suppressMessages(edit_btw_md())
  )
})

test_that("edit_btw_md() errors when no file found", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  expect_error(
    edit_btw_md(),
    "Could not find"
  )
})

test_that("edit_btw_md() uses rstudioapi when available", {
  skip_if_not_installed("rstudioapi")

  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  writeLines("test", "btw.md")

  navigate_called <- FALSE
  with_mocked_bindings(
    isAvailable = function() TRUE,
    navigateToFile = function(file) {
      navigate_called <<- TRUE
      expect_true(fs::file_exists(file))
    },
    .package = "rstudioapi",
    suppressMessages(edit_btw_md("btw.md"))
  )

  expect_true(navigate_called)
})

test_that("resolve_btw_md_path() handles different scopes", {
  wd <- withr::local_tempdir()
  withr::local_dir(wd)

  writeLines("test", "DESCRIPTION")

  # Project scope
  path <- resolve_btw_md_path("project", for_creation = TRUE)
  expect_equal(basename(path), "btw.md")

  # Directory path
  subdir <- fs::path(wd, "sub")
  fs::dir_create(subdir)
  path <- resolve_btw_md_path(subdir, for_creation = TRUE)
  expect_equal(basename(path), "btw.md")
  expect_true(grepl("sub", path))

  # File path
  path <- resolve_btw_md_path("custom.md", for_creation = TRUE)
  expect_equal(basename(path), "custom.md")
})

test_that("resolve_btw_md_path() with NULL errors when for_creation=TRUE", {
  expect_error(
    resolve_btw_md_path(NULL, for_creation = TRUE),
    "scope.*must be specified"
  )
})

test_that("btw_md_template() selects correct template", {
  # btw.md template
  template <- btw_md_template("btw.md")
  expect_true(fs::file_exists(template))
  expect_match(basename(template), "^btw.*\\.md$")

  # AGENTS.md template
  template <- btw_md_template("AGENTS.md")
  expect_true(fs::file_exists(template))
  expect_match(basename(template), "AGENTS\\.md")

  # Custom name gets btw.md template
  template <- btw_md_template("custom.md")
  expect_match(basename(template), "^btw.*\\.md$")
})
