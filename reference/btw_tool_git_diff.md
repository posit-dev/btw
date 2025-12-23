# Tool: Git Diff

This tool allows an LLM to run
[`gert::git_diff_patch()`](https://docs.ropensci.org/gert/reference/git_diff.html),
equivalent to `git diff` in the terminal, and to see the detailed
changes made in a commit.

## Usage

``` r
btw_tool_git_diff(ref = NULL, `_intent` = "")
```

## Arguments

- ref:

  a reference such as `"HEAD"`, or a commit id, or `NULL` to the diff
  the working directory against the repository index.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

Returns a diff patch as a formatted string.

## See also

Other git tools:
[`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_checkout.md),
[`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_create.md),
[`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/reference/btw_tool_git_branch_list.md),
[`btw_tool_git_commit()`](https://posit-dev.github.io/btw/reference/btw_tool_git_commit.md),
[`btw_tool_git_log()`](https://posit-dev.github.io/btw/reference/btw_tool_git_log.md),
[`btw_tool_git_status()`](https://posit-dev.github.io/btw/reference/btw_tool_git_status.md)

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

  # What the LLM sees
  cat(btw_tool_git_diff()@value)
})
#> ```diff diff --git a/hello.md b/hello.md
#> index 4b5fa63..d972925 100644
#> --- a/hello.md
#> +++ b/hello.md
#> @@ -1 +1 @@
#> -hello, world
#> +hello, universe
#>  ```
```
