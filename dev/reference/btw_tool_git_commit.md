# Tool: Git Commit

This tool allows an LLM stage files and create a git commit. This tool
uses a combination of
[`gert::git_add()`](https://docs.ropensci.org/gert/reference/git_commit.html)
to stage files and
[`gert::git_commit()`](https://docs.ropensci.org/gert/reference/git_commit.html)
to commit them, which is equivalent to `git add` and `git commit` in the
terminal, respectively.

## Usage

``` r
btw_tool_git_commit(message, files = NULL, `_intent` = "")
```

## Arguments

- message:

  A commit message describing the changes.

- files:

  Optional character vector of file paths to stage and commit. Use `"."`
  to stage all changed files. If `NULL`, commits currently staged files.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns the commit SHA.

## See also

Other git tools:
[`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_checkout.md),
[`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_create.md),
[`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_list.md),
[`btw_tool_git_diff()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_diff.md),
[`btw_tool_git_log()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_log.md),
[`btw_tool_git_status()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_status.md)

## Examples

``` r
withr::with_tempdir({
  gert::git_init()
  gert::git_config_set("user.name", "R Example")
  gert::git_config_set("user.email", "ex@example.com")

  writeLines("hello, world", "hello.md")

  res <- btw_tool_git_commit("Initial commit", files = "hello.md")

  # What the LLM sees
  cat(res@value)
})
#> Created commit: e2cbecb
#> Message: Initial commit
```
