# Tool: Git Branch Checkout

Allows an LLM to switch to a different git branch using
[`gert::git_branch_checkout()`](https://docs.ropensci.org/gert/reference/git_branch.html),
equivalent to `git checkout <branch>` in the terminal.

## Usage

``` r
btw_tool_git_branch_checkout(branch, force = FALSE, `_intent` = "")
```

## Arguments

- branch:

  Name of branch to check out.

- force:

  Whether to force checkout even with uncommitted changes. Defaults to
  `FALSE`.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a confirmation message.

## See also

Other git tools:
[`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_create.md),
[`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_list.md),
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

  # LLM checks out an existing branch
  res <- btw_tool_git_branch_checkout(branch = "feature-1")

  # What the LLM sees
  cat(res@value)
})
#> Checked out branch 'feature-1'
```
