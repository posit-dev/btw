# Tool: Git Status

This tool allows the LLM to run
[`gert::git_status()`](https://docs.ropensci.org/gert/reference/git_commit.html),
equivalent to `git status` in the terminal, and to see the current
status of the working directory.

## Usage

``` r
btw_tool_git_status(
  include = c("both", "staged", "unstaged"),
  pathspec = NULL,
  `_intent` = ""
)
```

## Arguments

- include:

  One of `"both"`, `"staged"`, or `"unstaged"`. Use `"both"` to show
  both staged and unstaged files (default).

- pathspec:

  Optional character vector with paths to match.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a character table of file statuses.

## See also

Other git tools:
[`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_checkout.md),
[`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_create.md),
[`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_list.md),
[`btw_tool_git_commit()`](https://posit-dev.github.io/btw/reference/btw_tool_git_commit.md),
[`btw_tool_git_diff()`](https://posit-dev.github.io/btw/reference/btw_tool_git_diff.md),
[`btw_tool_git_log()`](https://posit-dev.github.io/btw/reference/btw_tool_git_log.md)

## Examples

``` r
withr::with_tempdir({
  gert::git_init()
  gert::git_config_set("user.name", "R Example")
  gert::git_config_set("user.email", "ex@example.com")

  writeLines("hello, world", "hello.md")

  # What the LLM sees
  cat(btw_tool_git_status()@value)
})
#> hello.md [new] -unstaged
```
