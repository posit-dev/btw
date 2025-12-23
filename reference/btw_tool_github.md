# Tool: GitHub

Execute R code that calls the GitHub API using
[`gh::gh()`](https://gh.r-lib.org/reference/gh.html).

This tool is designed such that models can write very limited R code to
call [`gh::gh()`](https://gh.r-lib.org/reference/gh.html) and
protections are inserted to prevent the model from calling unsafe or
destructive actions via the API. The **Endpoint Validation** section
below describes how API endpoints are validated to ensure safety.

While this tool *can* execute R code, the code is evaluated in an
environment where only a limited set of functions and variables are
available. In particular, only the `gh()` and `gh_whoami()` functions
from the `gh` package are available, along with `owner` and `repo`
variables that are pre-defined to point to the current repository (if
detected). This allows models to focus on writing GitHub API calls
without needing to load packages or manage authentication.

### Endpoint Validation

This tool uses endpoint validation to ensure only safe GitHub API
operations are performed. By default, most read operations and low-risk
write operations (like creating issues or PRs) are allowed, while
dangerous operations (like merging PRs or deleting repositories) are
blocked.

To customize which endpoints are allowed or blocked, use the
`btw.github.allow` and `btw.github.block` options:

    # Allow a specific endpoint
    options(btw.github.allow = c(
      getOption("btw.github.allow"),
      "GET /repos/*/*/topics"
    ))

    # Block a specific endpoint
    options(btw.github.block = c(
      getOption("btw.github.block"),
      "GET /repos/*/*/branches"
    ))

You can also set these options in your
[btw.md](https://posit-dev.github.io/btw/reference/use_btw_md.md) file
under the `options` field:

    tools: github
    options:
      github:
        allow:
          - "PATCH /repos/*/*/pulls/*" # Allow converting PRs to/from draft
          - "POST /repos/*/*/git/refs" # Allow creating branches
        block:
          - "DELETE /repos/**" # Block any delete action under /repos

The precedence order for rules is:

1.  User block rules (checked first, highest priority)

2.  User allow rules

3.  Built-in block rules

4.  Built-in allow rules

5.  Default: reject (if no rules match)

### Additional Examples

    # Get an issue
    btw_tool_github(
      code = 'gh("/repos/{owner}/{repo}/issues/123", owner = owner, repo = repo)'
    )

    # Create an issue
    btw_tool_github(code = r"(
      gh(
        "POST /repos/{owner}/{repo}/issues",
        title = \"Bug report\",
        body = \"Description of bug\",
        owner = owner,
        repo = repo
      )
    )")

    # Target a different repository
    btw_tool_github(code = 'gh("/repos/tidyverse/dplyr/issues/123")')

## Usage

``` r
btw_tool_github(code, fields = "default", `_intent` = "")
```

## Arguments

- code:

  R code that calls `gh()` or `gh_whoami()`. The code will be evaluated
  in an environment where `owner` and `repo` variables are predefined
  (defaulting to the current repository if detected). The `gh()`
  function is available without needing to load the gh package.

- fields:

  Optional character vector of GitHub API response fields to retain. If
  provided, only these fields will be included in the result. Defaults
  to a curated set of commonly used fields.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A `btw_tool_result` containing the result of the GitHub API call.

## Examples

``` r
# This tool requires the gh package and authentication to GitHub.
# See additional examples in the documentation above.
```
