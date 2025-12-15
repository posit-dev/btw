# Package index

## Core functions

- [`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md) :
  Plain-text descriptions of R objects
- [`btw_this()`](https://posit-dev.github.io/btw/dev/reference/btw_this.md)
  : Describe something for use by an LLM
- [`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  : Create a btw-enhanced ellmer chat client
- [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
  : Tools: Register tools from btw
- [`btw_mcp_server()`](https://posit-dev.github.io/btw/dev/reference/mcp.md)
  [`btw_mcp_session()`](https://posit-dev.github.io/btw/dev/reference/mcp.md)
  : Start a Model Context Protocol server with btw tools

## Project Context

- [`use_btw_md()`](https://posit-dev.github.io/btw/dev/reference/use_btw_md.md)
  [`edit_btw_md()`](https://posit-dev.github.io/btw/dev/reference/use_btw_md.md)
  : Create or edit a btw.md context file
- [`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_btw_md.md)
  : Task: Initialize Project Context File

## Describe R stuff

- [`btw_this()`](https://posit-dev.github.io/btw/dev/reference/btw_this.md)
  : Describe something for use by an LLM
- [`btw_this(`*`<character>`*`)`](https://posit-dev.github.io/btw/dev/reference/btw_this.character.md)
  : Describe objects
- [`btw_this(`*`<data.frame>`*`)`](https://posit-dev.github.io/btw/dev/reference/btw_this.data.frame.md)
  [`btw_this(`*`<tbl>`*`)`](https://posit-dev.github.io/btw/dev/reference/btw_this.data.frame.md)
  : Describe a data frame in plain text
- [`btw_this(`*`<environment>`*`)`](https://posit-dev.github.io/btw/dev/reference/btw_this.environment.md)
  : Describe the contents of an environment

## Tasks and Agents

Tasks, or agents, are higher-level functions that combine system prompts
and tools to perform more complex operations.

Tasks are typically designed for interactive use (e.g. to
collaboratively create a project context file), but they can also be
used programmatically.

- [`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_btw_md.md)
  : Task: Initialize Project Context File
- [`btw_task_create_readme()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_readme.md)
  : Task: Create a Polished README

## Tools

Register all of these tools with an
[ellmer](https://ellmer.tidyverse.org) chat using
`chat$register_tools(btw_tools())`. These tools generally all have
counterparts available via
[`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md) and
[`btw_this()`](https://posit-dev.github.io/btw/dev/reference/btw_this.md)
for interactive use. The long and awkward function names are for LLMs
and to avoid name clashes with other tools.

- [`btw_tool_docs_package_news()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_docs_package_news.md)
  : Tool: Package Release Notes
- [`btw_tool_env_describe_data_frame()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_data_frame.md)
  : Tool: Describe data frame
- [`btw_tool_env_describe_environment()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_env_describe_environment.md)
  : Tool: Describe an environment
- [`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_code_search.md)
  : Tool: Code Search in Project
- [`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list_files.md)
  : Tool: List files
- [`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read_text_file.md)
  : Tool: Read a file
- [`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write_text_file.md)
  : Tool: Write a text file
- [`btw_tool_git_branch_checkout()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_checkout.md)
  : Tool: Git Branch Checkout
- [`btw_tool_git_branch_create()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_create.md)
  : Tool: Git Branch Create
- [`btw_tool_git_branch_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_branch_list.md)
  : Tool: Git Branch List
- [`btw_tool_git_commit()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_commit.md)
  : Tool: Git Commit
- [`btw_tool_git_diff()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_diff.md)
  : Tool: Git Diff
- [`btw_tool_git_log()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_log.md)
  : Tool: Git Log
- [`btw_tool_git_status()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_git_status.md)
  : Tool: Git Status
- [`btw_tool_github()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_github.md)
  : Tool: GitHub
- [`btw_tool_ide_read_current_editor()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_ide_read_current_editor.md)
  : Tool: Read current file
- [`btw_tool_docs_package_help_topics()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)
  [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)
  [`btw_tool_docs_available_vignettes()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)
  [`btw_tool_docs_vignette()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)
  : Tool: Describe R package documentation
- [`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md)
  : Tool: Run R CMD check on a package
- [`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md)
  : Tool: Generate package documentation
- [`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md)
  : Tool: Run package tests
- [`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md)
  **\[experimental\]** : Tool: Run R code
- [`btw_tool_search_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_search_package_info.md)
  : Tool: Describe a CRAN package
- [`btw_tool_search_packages()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_search_packages.md)
  : Tool: Search for an R package on CRAN
- [`btw_tool_session_check_package_installed()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_check_package_installed.md)
  : Tool: Check if a package is installed
- [`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_package_info.md)
  : Tool: Gather information about a package or currently loaded
  packages
- [`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_session_platform_info.md)
  : Tool: Describe user's platform
- [`btw_tool_web_read_url()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_web_read_url.md)
  : Tool: Read a Web Page as Markdown
