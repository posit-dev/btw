use_latest_pandoc()

test_that("btw_this.function()", {
  skip_if_not_snapshot_env()
  expect_snapshot(cli::cat_line(btw_this(dplyr::mutate)))
})

test_that("btw_this('{pkg}')", {
  # Gets the intro vignette if one is available
  expect_equal(
    btw_this("{dplyr}"),
    c(
      btw_tool_docs_package_help_topics("dplyr")@value,
      "",
      btw_tool_docs_vignette("dplyr")@value
    )
  )

  # Otherwise returns the help index
  expect_equal(
    btw_this("{cli}"),
    btw_tool_docs_package_help_topics("cli")@value
  )
})

test_that("btw_this.btw_docs_topic()", {
  expect_equal(
    btw_this(?dplyr::mutate),
    btw_this("?dplyr::mutate")
  )
})

test_that("btw_this() handles literal strings", {
  expect_equal(
    as.character(btw_this("letters[3]")),
    "letters[3]"
  )
})

test_that("btw_this('@last_error')", {
  with_mocked_bindings(
    last_error = function() {
      stop(
        "Can't show last error because no error was recorded yet",
        call. = FALSE
      )
    },
    expect_warning(expect_equal(btw_this("@last_error"), btw_ignore()))
  )

  with_mocked_bindings(
    last_error = function() {
      rlang::catch_cnd(abort("That didn't work.", trace = FALSE, call = NULL))
    },
    # btw_this("@last_error")
    expect_snapshot(cat(btw_this("@last_error")))
  )
})

test_that('btw_this("@last_value")', {
  local_mocked_bindings(
    get_last_value = function() mtcars[2:4, ]
  )

  expect_equal(
    btw_this("@last_value"),
    btw_this(mtcars[2:4, ])
  )
})


# Test @pkg command -----------------------------------------------------------

test_that("@pkg command works like {pkg} syntax", {
  # Both syntaxes should produce identical results
  expect_equal(
    btw_this("@pkg dplyr"),
    btw_this("{dplyr}")
  )

  expect_equal(
    btw_this("@pkg cli"),
    btw_this("{cli}")
  )
})

test_that("@pkg requires package name", {
  expect_error(
    btw_this("@pkg"),
    "@pkg.*must be followed by a package name"
  )

  expect_error(
    btw_this("@pkg "),
    "@pkg.*must be followed by a package name"
  )
})

# Test @help command ----------------------------------------------------------

test_that("@help command works with pkg::topic syntax", {
  expect_equal(
    btw_this("@help dplyr::mutate"),
    btw_this("?dplyr::mutate")
  )
})

test_that("@help command works with space-separated syntax", {
  expect_equal(
    btw_this("@help dplyr mutate"),
    btw_this("?dplyr::mutate")
  )
})

test_that("@help command works with topic only", {
  # Should search all packages
  result <- btw_this("@help mutate")
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("@help requires topic", {
  expect_error(
    btw_this("@help"),
    "@help.*must be followed by a help topic"
  )

  expect_error(
    btw_this("@help "),
    "@help.*must be followed by a help topic"
  )
})

# Test @git commands ----------------------------------------------------------

test_that("@git status works with mocked implementation", {
  local_mocked_bindings(
    btw_tool_git_status_impl = function(include, pathspec) {
      btw_tool_result("Status: OK")
    }
  )
  local_git_info()

  result <- btw_this("@git status")
  expect_match(result, "Status: OK", all = FALSE)
  expect_match(result, "git status", all = FALSE)
})

test_that("@git status accepts include argument", {
  local_mocked_bindings(
    btw_tool_git_status_impl = function(include, pathspec) {
      btw_tool_result(paste("Include:", include))
    }
  )
  local_git_info()

  result <- btw_this("@git status staged")
  expect_match(result, "Include: staged", all = FALSE)

  result <- btw_this("@git status unstaged")
  expect_match(result, "Include: unstaged", all = FALSE)

  result <- btw_this("@git status both")
  expect_match(result, "Include: both", all = FALSE)
})

test_that("@git diff works", {
  local_mocked_bindings(
    btw_tool_git_diff_impl = function(ref) {
      ref_str <- if (is.null(ref)) "NULL" else ref
      btw_tool_result(paste("Ref:", ref_str))
    }
  )
  local_git_info()

  result <- btw_this("@git diff")
  expect_match(result, "Ref: NULL", all = FALSE)

  result <- btw_this("@git diff HEAD")
  expect_match(result, "Ref: HEAD", all = FALSE)
})

test_that("@git log works with defaults", {
  local_mocked_bindings(
    btw_tool_git_log_impl = function(ref, max, after = NULL) {
      btw_tool_result(paste("Log:", ref, max))
    }
  )
  local_git_info()

  result <- btw_this("@git log")
  expect_match(result, "Log: HEAD 10", all = FALSE)
})

test_that("@git log works with custom ref", {
  local_mocked_bindings(
    btw_tool_git_log_impl = function(ref, max, after = NULL) {
      btw_tool_result(paste("Log:", ref, max))
    }
  )
  local_git_info()

  result <- btw_this("@git log main")
  expect_match(result, "Log: main 10", all = FALSE)
})

test_that("@git log works with custom max", {
  local_mocked_bindings(
    btw_tool_git_log_impl = function(ref, max, after = NULL) {
      btw_tool_result(paste("Log:", ref, max))
    }
  )
  local_git_info()

  result <- btw_this("@git log main 20")
  expect_match(result, "Log: main 20", all = FALSE)
})

test_that("@git requires gert package", {
  local_mocked_bindings(
    is_installed = function(pkg) pkg != "gert"
  )
  local_git_info()

  expect_error(
    btw_this("@git status"),
    "gert"
  )
})

test_that("@git requires subcommand", {
  local_git_info()

  expect_error(
    btw_this("@git"),
    "@git.*must be followed by a subcommand"
  )
})

test_that("@git rejects unknown subcommands", {
  local_git_info()

  expect_error(
    btw_this("@git unknown"),
    "Unknown git subcommand"
  )
})

test_that("@git log validates max argument", {
  local_git_info()

  expect_error(
    btw_this("@git log main abc"),
    "Invalid max value"
  )

  expect_error(
    btw_this("@git log main -5"),
    "Invalid max value"
  )
})

# Test @issue and @pr commands ------------------------------------------------

test_that("@issue and @pr parse number-only format", {
  result <- parse_github_reference("#65")
  expect_null(result$owner)
  expect_null(result$repo)
  expect_equal(result$number, 65)

  result <- parse_github_reference("123")
  expect_null(result$owner)
  expect_null(result$repo)
  expect_equal(result$number, 123)
})

test_that("@issue and @pr parse owner/repo#number format", {
  result <- parse_github_reference("posit-dev/btw#65")
  expect_equal(result$owner, "posit-dev")
  expect_equal(result$repo, "btw")
  expect_equal(result$number, 65)

  result <- parse_github_reference("tidyverse/dplyr#1234")
  expect_equal(result$owner, "tidyverse")
  expect_equal(result$repo, "dplyr")
  expect_equal(result$number, 1234)
})

test_that("@issue and @pr parse owner/repo number format", {
  result <- parse_github_reference("posit-dev/btw 65")
  expect_equal(result$owner, "posit-dev")
  expect_equal(result$repo, "btw")
  expect_equal(result$number, 65)

  result <- parse_github_reference("tidyverse/dplyr 1234")
  expect_equal(result$owner, "tidyverse")
  expect_equal(result$repo, "dplyr")
  expect_equal(result$number, 1234)

  # Multiple spaces should be handled
  result <- parse_github_reference("tidyverse/dplyr  1234")
  expect_equal(result$owner, "tidyverse")
  expect_equal(result$repo, "dplyr")
  expect_equal(result$number, 1234)
})

test_that("parse_github_reference handles edge cases", {
  # Format with # but no number
  expect_error(
    parse_github_reference("owner/repo#"),
    "Invalid GitHub reference format"
  )

  # Format with / but no # or space
  expect_error(
    parse_github_reference("owner/repo"),
    "Invalid GitHub reference format"
  )
})

test_that("@issue and @pr handle invalid formats", {
  expect_error(
    parse_github_reference("invalid"),
    "Invalid GitHub reference format"
  )

  expect_error(
    parse_github_reference("owner#123"), # Missing repo
    "Invalid GitHub reference format"
  )

  expect_error(
    parse_github_reference("invalid/format/here"),
    "Invalid GitHub reference format"
  )
})

test_that("@issue and @pr require issue number", {
  skip_if_not(is_installed("gh"))

  expect_error(
    btw_this("@issue"),
    "@issue.*must be followed"
  )

  expect_error(
    btw_this("@pr "),
    "@pr.*must be followed"
  )
})

test_that("@issue detects current repo when only number provided", {
  skip_if_not(is_installed("gh"))

  local_posit_dev_btw_repo()
  local_mocked_gh(
    list(
      number = 65,
      title = "Test Issue",
      html_url = "https://github.com/posit-dev/btw/issues/65",
      state = "open",
      user = list(login = "testuser"),
      created_at = "2025-01-01T00:00:00Z",
      updated_at = "2025-01-01T00:00:00Z",
      body = "Test body",
      labels = list(),
      pull_request = NULL,
      closed_at = NULL,
      merged_at = NULL,
      milestone = NULL
    )
  )

  result <- btw_this("@issue #65")
  expect_match(result, "github-issue")
  expect_match(result, 'owner="posit-dev"')
  expect_match(result, 'repo="btw"')
  expect_match(result, 'number="65"')
  expect_match(result, "Test Issue")
  expect_match(result, "Test body")

  skip_if_not_snapshot_env()
  expect_snapshot(cli::cat_line(result))
})

test_that("@pr marks pull requests correctly", {
  skip_if_not(is_installed("gh"))

  local_posit_dev_btw_repo()
  local_mocked_gh(
    list(
      number = 64,
      title = "Test PR",
      html_url = "https://github.com/posit-dev/btw/pull/64",
      state = "closed",
      user = list(login = "testuser"),
      created_at = "2025-01-01T00:00:00Z",
      updated_at = "2025-01-01T00:00:00Z",
      merged_at = "2025-01-02T00:00:00Z",
      body = "Test PR body",
      labels = list(
        list(name = "bug"),
        list(name = "urgent")
      ),
      pull_request = list(
        url = "https://api.github.com/repos/posit-dev/btw/pulls/64"
      ),
      closed_at = "2025-01-02T00:00:00Z",
      milestone = list(title = "v1.0")
    )
  )

  result <- btw_this("@pr #64")
  expect_match(result, "github-pull-request")
  expect_match(result, "Pull Request")
  expect_match(result, "merged: 2025-01-02T00:00:00Z")

  skip_if_not_snapshot_env()
  expect_snapshot(cli::cat_line(result))
})

test_that("format_github_item handles empty body", {
  skip_if_not(is_installed("gh"))

  item <- list(
    number = 1,
    title = "Test",
    html_url = "https://github.com/test/test/issues/1",
    state = "open",
    user = list(login = "user"),
    created_at = "2025-01-01T00:00:00Z",
    updated_at = "2025-01-01T00:00:00Z",
    body = "",
    labels = list(),
    pull_request = NULL,
    closed_at = NULL,
    merged_at = NULL,
    milestone = NULL
  )

  result <- format_github_item(item, "owner", "repo", "issue")
  expect_match(result, "_No description provided._")
})

# Test backward compatibility -------------------------------------------------

test_that("legacy {pkg} syntax still works", {
  expect_equal(
    btw_this("{dplyr}"),
    c(
      btw_tool_docs_package_help_topics("dplyr")@value,
      "",
      btw_tool_docs_vignette("dplyr")@value
    )
  )
})

test_that("legacy ?topic syntax still works", {
  expect_equal(
    btw_this(?dplyr::mutate),
    btw_this("?dplyr::mutate")
  )
})

test_that("legacy ./path syntax still works", {
  skip_on_cran()

  # Create a temporary file for testing
  temp_file <- tempfile(fileext = ".txt")
  writeLines("test content", temp_file)
  on.exit(unlink(temp_file))

  # Change to temp directory
  old_dir <- getwd()
  temp_dir <- dirname(temp_file)
  setwd(temp_dir)
  on.exit(setwd(old_dir), add = TRUE)

  # Test reading file
  result <- btw_this(paste0("./", basename(temp_file)))
  expect_match(result, "test content")
})

# Test unknown @ commands -----------------------------------------------------

test_that("unknown @ commands return user prompt", {
  # Unknown commands should fall through to user prompt
  result <- btw_this("@unknown_command")
  expect_s3_class(result, "btw_user_prompt")
  expect_equal(as.character(result), "@unknown_command")
})

test_that("@unknown_with_args returns user prompt", {
  result <- btw_this("@unknown_command with args")
  expect_s3_class(result, "btw_user_prompt")
  expect_equal(as.character(result), "@unknown_command with args")
})

# Test @ command edge cases ---------------------------------------------------

test_that("@ commands handle extra whitespace", {
  expect_equal(
    btw_this("@pkg  dplyr"), # Extra space
    btw_this("@pkg dplyr")
  )

  expect_equal(
    btw_this("  @help dplyr::mutate  "), # Leading/trailing
    btw_this("@help dplyr::mutate")
  )
})

test_that("@ commands are case-sensitive", {
  # @PKG should not match @pkg - should be treated as user prompt
  result <- btw_this("@PKG dplyr")
  expect_s3_class(result, "btw_user_prompt")
})

# Test @news command ----------------------------------------------------------

test_that("@news command works", {
  local_mocked_bindings(
    btw_tool_docs_package_news_impl = function(package_name, search_term) {
      btw_tool_result(paste("News for", package_name, search_term))
    }
  )

  result <- btw_this("@news dplyr")
  expect_match(result, "News for dplyr")

  result <- btw_this("@news dplyr join_by")
  expect_match(result, "News for dplyr join_by")
})

test_that("@news requires package name", {
  expect_error(
    btw_this("@news"),
    "@news.*must be followed by a package name"
  )

  expect_error(
    btw_this("@news "),
    "@news.*must be followed by a package name"
  )
})

# Test @url command -----------------------------------------------------------

test_that("@url requires chromote", {
  local_mocked_bindings(
    has_chromote = function() FALSE
  )

  expect_error(
    btw_this("@url https://example.com"),
    "chromote"
  )
})

test_that("@url requires URL", {
  local_mocked_bindings(
    has_chromote = function() TRUE,
    read_url_main_content = function(...) NULL
  )

  expect_error(
    btw_this("@url"),
    "@url.*must be followed by a valid URL"
  )

  expect_error(
    btw_this("@url "),
    "@url.*must be followed by a valid URL"
  )
})
