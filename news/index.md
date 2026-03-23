# Changelog

## btw 1.2.1

CRAN release: 2026-03-23

### Bug fixes

- User-level config locations are now consistent across skills, agents,
  and `btw.md` discovery. Skills installed with btw v1.2.0 are still
  discovered for backwards compatibility
  ([\#182](https://github.com/posit-dev/btw/issues/182)).

- The `btw` CLI now loads `datasets`, `utils`, `stats`, and `methods` by
  declaring them in the Rapp `#| launcher:` frontmatter, reducing
  surprises for users who expect standard R packages to be available
  ([\#181](https://github.com/posit-dev/btw/issues/181)).

## btw 1.2.0

CRAN release: 2026-03-16

### Breaking changes

- Several tool groups and tool names have been renamed for clarity
  ([\#159](https://github.com/posit-dev/btw/issues/159)):

  | Old Name | New Name |
  |----|----|
  | `session` group | `sessioninfo` group |
  | `search` group | `cran` group |
  | [`btw_tool_session_platform_info()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_sessioninfo_platform()`](https://posit-dev.github.io/btw/reference/btw_tool_sessioninfo_platform.md) |
  | [`btw_tool_session_package_info()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_sessioninfo_package()`](https://posit-dev.github.io/btw/reference/btw_tool_sessioninfo_package.md) |
  | [`btw_tool_session_check_package_installed()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_sessioninfo_is_package_installed()`](https://posit-dev.github.io/btw/reference/btw_tool_sessioninfo_is_package_installed.md) |
  | [`btw_tool_search_packages()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_cran_search()`](https://posit-dev.github.io/btw/reference/btw_tool_cran_search.md) |
  | [`btw_tool_search_package_info()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_cran_package()`](https://posit-dev.github.io/btw/reference/btw_tool_cran_package.md) |
  | [`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_files_list()`](https://posit-dev.github.io/btw/reference/btw_tool_files_list.md) |
  | [`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_files_read()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read.md) |
  | [`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_files_write()`](https://posit-dev.github.io/btw/reference/btw_tool_files_write.md) |
  | [`btw_tool_files_code_search()`](https://posit-dev.github.io/btw/reference/deprecated.md) | [`btw_tool_files_search()`](https://posit-dev.github.io/btw/reference/btw_tool_files_search.md) |

  The old names and group aliases continue to work but emit deprecation
  warnings. The `btw.files_code_search.extensions` and
  `btw.files_code_search.exclusions` options have also been renamed to
  `btw.files_search.extensions` and `btw.files_search.exclusions`, with
  the old option names emitting deprecation warnings.

### New features

- New `btw` CLI provides command-line access to btw’s tool groups —
  **docs**, **pkg**, **info**, and **cran** — powered by
  [Rapp](https://github.com/r-lib/Rapp). Install with
  [`install_btw_cli()`](https://posit-dev.github.io/btw/reference/install_btw_cli.md)
  and run commands like `btw docs help dplyr::mutate` or
  `btw cran search "tidyverse"`. Output is designed for humans and LLMs:
  colored and formatted for terminals, plain markdown when piped, with a
  `--json` flag to return pipable JSON in select commands
  ([\#176](https://github.com/posit-dev/btw/issues/176)).

- `btw.md` now supports configuring multiple client options. When
  [`btw_client()`](https://posit-dev.github.io/btw/reference/btw_client.md)
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

- [`btw_agent_tool()`](https://posit-dev.github.io/btw/reference/btw_agent_tool.md)
  allows you to create specialized custom subagents from `btw.md` style
  markdown files. Agent files are automatically discovered from
  `.btw/agent-*.md` (project and user directories) and `.claude/agents/`
  (for Claude Code compatibility), and are registered as callable tools
  in
  [`btw_tools()`](https://posit-dev.github.io/btw/reference/btw_tools.md).
  Custom agents can specify their own system prompts, icons, models, and
  available tools
  ([\#149](https://github.com/posit-dev/btw/issues/149)).

- [`btw_task()`](https://posit-dev.github.io/btw/reference/btw_task.md)
  runs pre-formatted LLM tasks defined in markdown files with YAML
  frontmatter. Task files support template variable interpolation via
  `{{ variable }}` syntax, optional client and tool configuration, and
  all four execution modes (`"app"`, `"console"`, `"client"`, `"tool"`).
  Task files can also specify `title`, `icon`, `description`, and `name`
  fields in their YAML frontmatter to customize the tool definition when
  used in `"tool"` mode
  ([\#169](https://github.com/posit-dev/btw/issues/169)).

- [`btw_tool_agent_subagent()`](https://posit-dev.github.io/btw/reference/btw_tool_agent_subagent.md)
  enables hierarchical agent workflows by allowing an orchestrating LLM
  to delegate tasks to subagents. Each subagent runs in its own isolated
  chat session with restricted tool access and maintains conversation
  state that can be resumed via `session_id`. This allows you to
  delegate tasks to smaller cheaper models or reduce context bloat in
  the main conversation
  ([\#149](https://github.com/posit-dev/btw/issues/149)).

- [`btw_tool_files_edit()`](https://posit-dev.github.io/btw/reference/btw_tool_files_edit.md)
  makes targeted, validated line-based edits to files using `replace`,
  `insert_after`, and `replace_range` actions. Edits are anchored to
  content hashes, so stale edits are rejected if the file has changed.
  To support this,
  [`btw_tool_files_read()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read.md)
  now annotates each line with a short hash
  (e.g. `2:f1a| return("world")`) when called as a tool
  ([\#167](https://github.com/posit-dev/btw/issues/167)).

- [`btw_tool_files_replace()`](https://posit-dev.github.io/btw/reference/btw_tool_files_replace.md)
  finds and replaces exact string occurrences in a file. By default it
  requires the string to appear exactly once to prevent unintended
  changes; set `replace_all = TRUE` to replace all occurrences
  ([\#167](https://github.com/posit-dev/btw/issues/167)).

- [`btw_tool_pkg_load_all()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_load_all.md)
  runs
  [`pkgload::load_all()`](https://pkgload.r-lib.org/reference/load_all.html)
  in an isolated subprocess to verify package code loads correctly and
  trigger recompilation of compiled code. Useful for quick validation
  during development without running full tests or affecting the current
  R session ([\#156](https://github.com/posit-dev/btw/issues/156)).

- [`btw_tool_skill()`](https://posit-dev.github.io/btw/reference/btw_tool_skill.md)
  adds support for [Agent Skills](https://agentskills.io). Skills are
  modular, on-demand capabilities that provide specialized instructions,
  bundled scripts, reference docs, and asset templates to the LLM.
  Skills are discovered automatically from the btw package, attached R
  packages with `inst/skills/` directories, user-level skills, and
  project-level skills in `.btw/skills/` or `.agents/skills/`. When the
  skill tool is included in the chat client, available skills are listed
  in the system prompt. Use
  [`btw_skill_install_github()`](https://posit-dev.github.io/btw/reference/btw_skill_install_github.md)
  or
  [`btw_skill_install_package()`](https://posit-dev.github.io/btw/reference/btw_skill_install_package.md)
  to install skills from external sources, and
  [`btw_task_create_skill()`](https://posit-dev.github.io/btw/reference/btw_task_create_skill.md)
  to interactively create new skills
  ([\#145](https://github.com/posit-dev/btw/issues/145)).

### Bug fixes

- [`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  no longer errors with “argument is of length zero” when run outside of
  an IDE (thanks [@HenrikBengtsson](https://github.com/HenrikBengtsson),
  [\#168](https://github.com/posit-dev/btw/issues/168)).

- [`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  no longer errors when provider built-in tools (e.g.
  [`claude_tool_web_search()`](https://ellmer.tidyverse.org/reference/claude_tool_web_search.html))
  are registered with a btw client. Built-in tools now appear in the app
  sidebar under a “Built-in” group
  ([\#175](https://github.com/posit-dev/btw/issues/175)).

- [`btw_tool_files_read()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read.md)
  now correctly handles UTF-8 files containing CJK (Chinese, Japanese,
  Korean) characters. Previously, the text-file detection could truncate
  a read buffer mid-way through a multi-byte character, causing the file
  to be rejected as binary (thanks
  [@bianchenhao](https://github.com/bianchenhao),
  [\#170](https://github.com/posit-dev/btw/issues/170)).

- [`btw_tool_files_read()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read.md)
  now correctly reads valid UTF-8 files containing non-ASCII characters
  (e.g., Cyrillic). Previously, these files were incorrectly rejected on
  Windows with non-English locales when
  [`Encoding()`](https://rdrr.io/r/base/Encoding.html) returned
  “unknown” even though they were valid UTF-8 (thanks
  [@RKonstantinR](https://github.com/RKonstantinR),
  [\#160](https://github.com/posit-dev/btw/issues/160)).

## btw 1.1.0

CRAN release: 2025-12-22

- [`btw_client()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  now supports reading `CLAUDE.md` files as project context files.
  `CLAUDE.md` files are searched after `AGENTS.md` but before user-level
  `btw.md`. YAML frontmatter in `CLAUDE.md` files is stripped but not
  used for configuration
  ([\#146](https://github.com/posit-dev/btw/issues/146)).

- [`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  now shows a rich diff view in the
  [`btw_tool_files_write_text_file()`](https://posit-dev.github.io/btw/reference/deprecated.md)
  tool, if the [diffviewer](https://diffviewer.r-lib.org) package is
  installed ([\#144](https://github.com/posit-dev/btw/issues/144)).

- [`btw()`](https://posit-dev.github.io/btw/reference/btw.md) now
  correctly handles character matrices
  ([\#139](https://github.com/posit-dev/btw/issues/139)).

- [`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  now correctly displays input tokens with ellmer v0.4.0. Previously, we
  were showing the total input tokens sent across all API calls, rather
  than the current number of input tokens as of the last API call. We
  now show the size of the input context window (i.e. the number of
  input tokens that will be used for the conversation history in the
  next API call). Note that token usage is dependent on the provider and
  model used and is only an estimate.

- New “pkg” tool group with package development tools:
  [`btw_tool_pkg_document()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_document.md),
  [`btw_tool_pkg_check()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_check.md),
  [`btw_tool_pkg_test()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_test.md)
  and
  [`btw_tool_pkg_coverage()`](https://posit-dev.github.io/btw/reference/btw_tool_pkg_coverage.md)
  provide LLMs with the ability to document, check, test, and check test
  coverage of R packages during development
  ([\#133](https://github.com/posit-dev/btw/issues/133),
  [\#136](https://github.com/posit-dev/btw/issues/136)).

- New
  [`btw_tool_run_r()`](https://posit-dev.github.io/btw/reference/btw_tool_run_r.md)
  tool allows LLMs to run R code and to see the output, including of
  plots. Because this tool lets LLMs run R arbitrary R code in the
  global environment (which can be great but can also have security
  implications), it is opt-in and disabled by default. See
  [`?btw_tool_run_r`](https://posit-dev.github.io/btw/reference/btw_tool_run_r.md)
  for more details
  ([\#126](https://github.com/posit-dev/btw/issues/126)).

- [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/reference/btw_tool_package_docs.md)
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

- [`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  now works correctly with [ellmer](https://ellmer.tidyverse.org) v0.4.0
  ([\#121](https://github.com/posit-dev/btw/issues/121)).

- [`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  now shows more precise cost estimates when the cost is less than
  \$0.10 ([\#121](https://github.com/posit-dev/btw/issues/121)).

## btw 1.0.0

CRAN release: 2025-11-04

- Initial CRAN submission.
