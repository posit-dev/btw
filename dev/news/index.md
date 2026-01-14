# Changelog

## btw (development version)

- BREAKING CHANGE: Several tool groups and tool names have been renamed
  for clarity ([\#159](https://github.com/posit-dev/btw/issues/159)):

  | Old Name | New Name |
  |----|----|
  | `session` group | `sessioninfo` group |
  | `search` group | `cran` group |
  | [`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_sessioninfo_platform()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_platform.md) |
  | [`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_sessioninfo_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_package.md) |
  | [`btw_tool_session_check_package_installed()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_sessioninfo_is_package_installed()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_sessioninfo_is_package_installed.md) |
  | [`btw_tool_search_packages()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_cran_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_search.md) |
  | [`btw_tool_search_package_info()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_cran_package()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_cran_package.md) |
  | [`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_files_list()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_list.md) |
  | [`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_files_read()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_read.md) |
  | [`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_files_write()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_write.md) |
  | [`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md) | [`btw_tool_files_search()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_files_search.md) |

  The old names and group aliases continue to work but emit deprecation
  warnings. The `btw.files_code_search.extensions` and
  `btw.files_code_search.exclusions` options have also been renamed to
  `btw.files_search.extensions` and `btw.files_search.exclusions`, with
  the old option names emitting deprecation warnings.

- New
  [`btw_tool_pkg_load_all()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_load_all.md)
  tool runs
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  in an isolated subprocess to verify package code loads correctly and
  trigger recompilation of compiled code. Useful for quick validation
  during development without running full tests or affecting the current
  R session ([\#156](https://github.com/posit-dev/btw/issues/156)).

- `btw.md` now supports configuring multiple client options. When
  [`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  is called interactively, you’ll be presented with a menu to choose
  which client to use. Clients can be specified as an array:

  ``` yaml
  client:
    - anthropic/claude-sonnet-4
    - openai/gpt-4.1
    - aws_bedrock/us.anthropic.claude-sonnet-4-20250514-v1:0
  ```

  Or as named aliases:

  ``` yaml
  client:
    haiku: aws_bedrock/us.anthropic.claude-haiku-4-5-20251001-v1:0
    sonnet: anthropic/claude-sonnet-4
    chatgpt: openai/gpt-5.2
  ```

  With aliases, you can select a client by name in the interactive menu
  or pass the alias directly via `btw_client(client = "alias")`. In
  non-interactive contexts, the first client is used automatically
  ([\#153](https://github.com/posit-dev/btw/issues/153)).

- New
  [`btw_tool_agent_subagent()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_agent_subagent.md)
  tool enables hierarchical agent workflows by allowing an orchestrating
  LLM to delegate tasks to subagents. Each subagent runs in its own
  isolated chat session with restricted tool access and maintains
  conversation state that can be resumed via `session_id`. This allows
  you to delegate tasks to smaller cheaper models or reduce context
  bloat in the main conversation
  ([\#149](https://github.com/posit-dev/btw/issues/149)).

- New
  [`btw_agent_tool()`](https://posit-dev.github.io/btw/dev/reference/btw_agent_tool.md)
  allows you to create specialized custom subagents from `btw.md` style
  markdown files. Agent files are automatically discovered from
  `.btw/agent-*.md` (project and user directories) and `.claude/agents/`
  (for Claude Code compatibility), and are registered as callable tools
  in
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).
  Custom agents can specify their own system prompts, icons, models, and
  available tools
  ([\#149](https://github.com/posit-dev/btw/issues/149)).

## btw 1.1.0

CRAN release: 2025-12-22

- [`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now supports reading `CLAUDE.md` files as project context files.
  `CLAUDE.md` files are searched after `AGENTS.md` but before user-level
  `btw.md`. YAML frontmatter in `CLAUDE.md` files is stripped but not
  used for configuration
  ([\#146](https://github.com/posit-dev/btw/issues/146)).

- [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now shows a rich diff view in the
  [`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/dev/reference/deprecated.md)
  tool, if the [diffviewer](https://diffviewer.r-lib.org) package is
  installed ([\#144](https://github.com/posit-dev/btw/issues/144)).

- [`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md) now
  correctly handles character matrices
  ([\#139](https://github.com/posit-dev/btw/issues/139)).

- [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now correctly displays input tokens with ellmer v0.4.0. Previously, we
  were showing the total input tokens sent across all API calls, rather
  than the current number of input tokens as of the last API call. We
  now show the size of the input context window (i.e. the number of
  input tokens that will be used for the conversation history in the
  next API call). Note that token usage is dependent on the provider and
  model used and is only an estimate.

- New “pkg” tool group with package development tools:
  [`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_document.md),
  [`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_check.md),
  [`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_test.md)
  and
  [`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_pkg_coverage.md)
  provide LLMs with the ability to document, check, test, and check test
  coverage of R packages during development
  ([\#133](https://github.com/posit-dev/btw/issues/133),
  [\#136](https://github.com/posit-dev/btw/issues/136)).

- New
  [`btw_tool_run_r()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md)
  tool allows LLMs to run R code and to see the output, including of
  plots. Because this tool lets LLMs run R arbitrary R code in the
  global environment (which can be great but can also have security
  implications), it is opt-in and disabled by default. See
  [`?btw_tool_run_r`](https://posit-dev.github.io/btw/dev/reference/btw_tool_run_r.md)
  for more details
  ([\#126](https://github.com/posit-dev/btw/issues/126)).

- [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md)
  now uses markdown headings and sections for argument descriptions,
  rather than a table. This is considerably more token efficient when
  the argument descriptions have more than one paragraph and can’t be
  converted into a markdown table
  ([@jeanchristophe13v](https://github.com/jeanchristophe13v),
  [\#123](https://github.com/posit-dev/btw/issues/123)).

- btw now removes large inline base64-encoded images, replacing them
  with a placeholder containing the image’s alt text
  ([@jeanchristophe13v](https://github.com/jeanchristophe13v),
  [\#119](https://github.com/posit-dev/btw/issues/119)).

- [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now works correctly with [ellmer](https://ellmer.tidyverse.org) v0.4.0
  ([\#121](https://github.com/posit-dev/btw/issues/121)).

- [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  now shows more precise cost estimates when the cost is less than
  \$0.10 ([\#121](https://github.com/posit-dev/btw/issues/121)).

## btw 1.0.0

CRAN release: 2025-11-04

- Initial CRAN submission.
