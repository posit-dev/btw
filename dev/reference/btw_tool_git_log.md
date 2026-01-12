# Tool: Git Log

This tool allows an LLM to run
[`gert::git_log()`](https://docs.ropensci.org/gert/reference/git_commit.html),
equivalent to `git log` in the terminal, and to see the commit history
of a repository.

## Usage

``` r
btw_tool_git_log(ref = "HEAD", max = 10, after = NULL, `_intent` = "")
```

## Arguments

- ref:

  Revision string with a branch/tag/commit value. Defaults to `"HEAD"`.

- max:

  Maximum number of commits to retrieve. Defaults to 10.

- after:

  Optional date or timestamp: only include commits after this date.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a character table of commit history.

## See also

Other git tools:
[`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_checkout.md),
[`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_create.md),
[`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_list.md),
[`btw_tool_git_commit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_commit.md),
[`btw_tool_git_diff()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_diff.md),
[`btw_tool_git_status()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_status.md)

## Examples

``` r
withr::with_tempdir({
  gert::git_init()
  gert::git_config_set("user.name", "R Example")
  gert::git_config_set("user.email", "ex@example.com")

  writeLines("hello, world", "hello.md")
  gert::git_add("hello.md")
  gert::git_commit("Initial commit")

  writeLines("hello, universe", "hello.md")
  gert::git_add("hello.md")
  gert::git_commit("Update hello.md")

  # What the LLM sees
  cat(btw_tool_git_log()@value)
})
#> message: Update hello.md
#> author: R Example <ex@example.com>
#> time: 2026-01-12 16:16:03
#> n_files: 1
#> commit: e979055
#> 
#> message: Initial commit
#> author: R Example <ex@example.com>
#> time: 2026-01-12 16:16:03
#> n_files: 1
#> commit: 75fbec6
```
