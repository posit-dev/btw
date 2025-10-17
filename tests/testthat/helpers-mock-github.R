local_posit_dev_btw_repo <- function(env = parent.frame()) {
  local_mocked_bindings(
    get_github_repo = function(...) {
      list(
        owner = "posit-dev",
        repo = "btw"
      )
    },
    .env = env
  )
}

local_mocked_gh <- function(mock_value, env = parent.frame()) {
  if (missing(mock_value)) {
    mock_fn <- function(endpoint, ...) {
      list(endpoint = endpoint, payload = list(...))
    }
  } else {
    mock_fn <- function(...) mock_value
  }
  local_mocked_bindings(
    btw_gh_fields = function() NULL,
    .env = env
  )
  local_mocked_bindings(
    gh = mock_fn,
    .package = "gh",
    .env = env
  )
}

mock_btw_issue_37 <- list(
  number = 37L,
  html_url = "https://github.com/posit-dev/btw/issues/37",
  title = "Enable dark mode in pkgdown site",
  body = "https://pkgdown.r-lib.org/articles/customise.html?q=light%20sw#light-switch"
)

mock_btw_issues_open <- list(
  list(
    number = 56L,
    title = "add tool to run queries against data frames",
    html_url = "https://github.com/posit-dev/btw/pull/56"
  ),
  list(
    number = 52L,
    title = "a tool to explore/manipulate data frames",
    html_url = "https://github.com/posit-dev/btw/issues/52"
  ),
  list(
    number = 43L,
    title = "Article: Discuss how to use `btw.md` and `btw(path_btw=)` for different tasks",
    html_url = "https://github.com/posit-dev/btw/issues/43"
  ),
  list(
    number = 42L,
    title = "Pre-formatted btw tasks",
    html_url = "https://github.com/posit-dev/btw/issues/42"
  ),
  list(
    number = 38L,
    title = "List data sets",
    html_url = "https://github.com/posit-dev/btw/issues/38"
  )
)
