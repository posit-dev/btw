# Tools: Register tools from btw

The `btw_tools()` function provides a list of tools that can be
registered with an ellmer chat via `chat$register_tools()` that allow
the chat to interface with your computational environment. Chats
returned by this function have access to the tools:

### Group: agent

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_agent_subagent()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_agent_subagent.md) |  |
| Delegate a task to a specialized assistant that can work independently with its own conversation thread. |  |

### Group: cran

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_cran_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_package.md) | Describe a CRAN package. |
| [`btw_tool_cran_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_search.md) | Search for an R package on CRAN. |

### Group: docs

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_docs_available_vignettes()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md) | List available vignettes for an R package. |
| [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md) | Get help page from package. |
| [`btw_tool_docs_package_help_topics()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md) | Get available help topics for an R package. |
| [`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md) | Read the release notes (NEWS) for a package. |
| [`btw_tool_docs_vignette()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md) | Get a package vignette in plain text. |

### Group: env

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md) | Show the data frame or table or get information about the structure of a data frame or table. |
| [`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md) | List and describe items in the R session's global environment. |

### Group: files

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_files_edit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_edit.md) | Edit a text file using hashline references for precise, targeted modifications. |
| [`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md) | List files or directories in the project. |
| [`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md) | Read the contents of a text file. |
| [`btw_tool_files_replace()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_replace.md) | Find and replace exact string occurrences in a text file. |
| [`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md) | Search code files in the project. |
| [`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md) | Write content to a text file. |

### Group: git

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_checkout.md) | Switch to a different git branch. |
| [`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_create.md) | Create a new git branch. |
| [`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_list.md) | List git branches in the repository. |
| [`btw_tool_git_commit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_commit.md) | Stage files and create a git commit. |
| [`btw_tool_git_diff()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_diff.md) | View changes in the working directory or a commit. |
| [`btw_tool_git_log()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_log.md) | Show the commit history for a repository. |
| [`btw_tool_git_status()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_status.md) | Show the status of the git working directory. |

### Group: github

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_github()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_github.md) | Execute R code that calls the GitHub API using gh(). |

### Group: ide

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md) | Read the contents of the editor that is currently open in the user's IDE. |

### Group: pkg

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md) | Run comprehensive package checks. |
| [`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md) | Compute test coverage for an R package. |
| [`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md) | Generate package documentation. |
| [`btw_tool_pkg_load_all()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_load_all.md) | Load package code to verify it loads correctly. |
| [`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md) | Run testthat tests for an R package. |

### Group: run

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md) | Run R code. |

### Group: sessioninfo

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_sessioninfo_is_package_installed()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_is_package_installed.md) | Check if a package is installed in the current session. |
| [`btw_tool_sessioninfo_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_package.md) | Verify that a specific package is installed, or find out which packages are in use in the current session. |
| [`btw_tool_sessioninfo_platform()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_platform.md) | Describes the R version, operating system, language and locale settings for the user's system. |

### Group: web

|  |  |
|----|----|
| Name | Description |
| [`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md) | Read a web page and convert it to Markdown format. |

## Usage

``` r
btw_tools(...)
```

## Arguments

- ...:

  Optional names of tools or tool groups to include when registering
  tools. By default all btw tools are included. For example, use
  `"docs"` to include only the documentation related tools, or
  `"env", "docs", "session"` for the collection of environment,
  documentation and session tools, and so on.

  The names provided can be:

  1.  The name of a tool, such as `"btw_tool_env_describe_data_frame"`.

  2.  The name of a tool group, such as `"env"`, which will include all
      tools in that group.

  3.  The tool name without the `btw_tool_` prefix, such as
      `"env_describe_data_frame"`.

## Value

Registers the tools with `chat`, updating the `chat` object in place.
The `chat` input is returned invisibly.

## Examples

``` r
# requires an ANTHROPIC_API_KEY
ch <- ellmer::chat_anthropic()
#> Using model = "claude-sonnet-4-5-20250929".

# register all of the available tools
ch$register_tools(btw_tools())
#> Warning: GitHub tools are not available because you are not authenticated with the gh
#> package.
#> ℹ Run `gh::gh_whoami()` to check your authentication status.
#> ℹ Run `gitcreds::gitcreds_set()` or set the GITHUB_PAT environment variable to
#>   authenticate.
#> This warning is displayed once per session.

# or register only the tools related to fetching documentation
ch$register_tools(btw_tools("docs"))
#> Replacing existing btw_tool_docs_package_news tool.
#> Replacing existing btw_tool_docs_package_help_topics tool.
#> Replacing existing btw_tool_docs_help_page tool.
#> Replacing existing btw_tool_docs_available_vignettes tool.
#> Replacing existing btw_tool_docs_vignette tool.

# ensure that the current tools persist
ch$register_tools(c(ch$get_tools(), btw_tools()))
#> Replacing existing btw_tool_agent_subagent tool.
#> Replacing existing btw_tool_cran_search tool.
#> Replacing existing btw_tool_cran_package tool.
#> Replacing existing btw_tool_docs_package_news tool.
#> Replacing existing btw_tool_docs_package_help_topics tool.
#> Replacing existing btw_tool_docs_help_page tool.
#> Replacing existing btw_tool_docs_available_vignettes tool.
#> Replacing existing btw_tool_docs_vignette tool.
#> Replacing existing btw_tool_env_describe_data_frame tool.
#> Replacing existing btw_tool_env_describe_environment tool.
#> Replacing existing btw_tool_files_edit tool.
#> Replacing existing btw_tool_files_list tool.
#> Replacing existing btw_tool_files_read tool.
#> Replacing existing btw_tool_files_replace tool.
#> Replacing existing btw_tool_files_search tool.
#> Replacing existing btw_tool_files_write tool.
#> Replacing existing btw_tool_git_status tool.
#> Replacing existing btw_tool_git_diff tool.
#> Replacing existing btw_tool_git_log tool.
#> Replacing existing btw_tool_git_commit tool.
#> Replacing existing btw_tool_git_branch_list tool.
#> Replacing existing btw_tool_git_branch_create tool.
#> Replacing existing btw_tool_git_branch_checkout tool.
#> Replacing existing btw_tool_ide_read_current_editor tool.
#> Replacing existing btw_tool_pkg_coverage tool.
#> Replacing existing btw_tool_pkg_document tool.
#> Replacing existing btw_tool_pkg_check tool.
#> Replacing existing btw_tool_pkg_test tool.
#> Replacing existing btw_tool_pkg_load_all tool.
#> Replacing existing btw_tool_sessioninfo_is_package_installed tool.
#> Replacing existing btw_tool_sessioninfo_platform tool.
#> Replacing existing btw_tool_sessioninfo_package tool.
#> Replacing existing btw_tool_web_read_url tool.
#> Replacing existing btw_tool_agent_subagent tool.
#> Replacing existing btw_tool_cran_search tool.
#> Replacing existing btw_tool_cran_package tool.
#> Replacing existing btw_tool_docs_package_news tool.
#> Replacing existing btw_tool_docs_package_help_topics tool.
#> Replacing existing btw_tool_docs_help_page tool.
#> Replacing existing btw_tool_docs_available_vignettes tool.
#> Replacing existing btw_tool_docs_vignette tool.
#> Replacing existing btw_tool_env_describe_data_frame tool.
#> Replacing existing btw_tool_env_describe_environment tool.
#> Replacing existing btw_tool_files_edit tool.
#> Replacing existing btw_tool_files_list tool.
#> Replacing existing btw_tool_files_read tool.
#> Replacing existing btw_tool_files_replace tool.
#> Replacing existing btw_tool_files_search tool.
#> Replacing existing btw_tool_files_write tool.
#> Replacing existing btw_tool_git_status tool.
#> Replacing existing btw_tool_git_diff tool.
#> Replacing existing btw_tool_git_log tool.
#> Replacing existing btw_tool_git_commit tool.
#> Replacing existing btw_tool_git_branch_list tool.
#> Replacing existing btw_tool_git_branch_create tool.
#> Replacing existing btw_tool_git_branch_checkout tool.
#> Replacing existing btw_tool_ide_read_current_editor tool.
#> Replacing existing btw_tool_pkg_coverage tool.
#> Replacing existing btw_tool_pkg_document tool.
#> Replacing existing btw_tool_pkg_check tool.
#> Replacing existing btw_tool_pkg_test tool.
#> Replacing existing btw_tool_pkg_load_all tool.
#> Replacing existing btw_tool_sessioninfo_is_package_installed tool.
#> Replacing existing btw_tool_sessioninfo_platform tool.
#> Replacing existing btw_tool_sessioninfo_package tool.
#> Replacing existing btw_tool_web_read_url tool.
```
