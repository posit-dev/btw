# Tool: Git Branch List

This tool allows an LLM to list git branches in the repository using
[`gert::git_branch_list()`](https://docs.ropensci.org/gert/reference/git_branch.html),
equivalent to `git branch` in the terminal.

## Usage

``` r
btw_tool_git_branch_list(include = c("local", "remote", "all"), `_intent` = "")
```

## Arguments

- include:

  Once of `"local"` (default), `"remote"`, or `"all"` to filter branches
  to local branches only, remote branches only, or all branches.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a character table of branches.

## See also

Other git tools:
[`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_checkout.md),
[`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_create.md),
[`btw_tool_git_commit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_commit.md),
[`btw_tool_git_diff()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_diff.md),
[`btw_tool_git_log()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_log.md),
[`btw_tool_git_status()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_status.md)

## Examples

``` r
withr::with_tempdir({
  gert::git_init()
  gert::git_config_set("user.name", "R Example")
  gert::git_config_set("user.email", "ex@example.com")

  fs::file_touch("hello.md")
  gert::git_add("hello.md")
  gert::git_commit("Initial commit")

  gert::git_branch_create("feature-1")
  gert::git_branch_create("feature-2")

  # What the LLM sees
  cat(btw_tool_git_branch_list()@value)
})
#> feature-1 [2025-12-15 18:33:17] 
#> feature-2 [2025-12-15 18:33:17] 
#> master [2025-12-15 18:33:17] 
```
