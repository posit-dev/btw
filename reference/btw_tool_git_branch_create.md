# Tool: Git Branch Create

Allows an LLM to create a new git branch using
[`gert::git_branch_create()`](https://docs.ropensci.org/gert/reference/git_branch.html),
equivalent to `git branch <branch>` in the terminal.

## Usage

``` r
btw_tool_git_branch_create(
  branch,
  ref = "HEAD",
  checkout = TRUE,
  `_intent` = ""
)
```

## Arguments

- branch:

  Name of the new branch to create.

- ref:

  Optional reference point for the new branch. Defaults to `"HEAD"`.

- checkout:

  Whether to check out the new branch after creation. Defaults to
  `TRUE`.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a confirmation message.

## See also

Other git tools:
[`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_checkout.md),
[`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_list.md),
[`btw_tool_git_commit()`](https://posit-dev.github.io/btw/reference/btw_tool_git_commit.md),
[`btw_tool_git_diff()`](https://posit-dev.github.io/btw/reference/btw_tool_git_diff.md),
[`btw_tool_git_log()`](https://posit-dev.github.io/btw/reference/btw_tool_git_log.md),
[`btw_tool_git_status()`](https://posit-dev.github.io/btw/reference/btw_tool_git_status.md)

## Examples

``` r
withr::with_tempdir({
  gert::git_init()
  gert::git_config_set("user.name", "R Example")
  gert::git_config_set("user.email", "ex@example.com")

  fs::file_touch("hello.md")
  gert::git_add("hello.md")
  gert::git_commit("Initial commit")

  # LLM creates a new branch
  res <- btw_tool_git_branch_create(branch = "feature/new-analysis")

  # What the LLM sees
  cat(res@value)
})
#> Created branch `feature/new-analysis` from `HEAD` and checked it out.
```
