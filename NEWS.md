# btw (development version)

# btw 1.2.0

## Breaking changes

* Several tool groups and tool names have been renamed for clarity (#159):

  | Old Name | New Name |
  |----------|----------|
  | `session` group | `sessioninfo` group |
  | `search` group | `cran` group |
  | `btw_tool_session_platform_info()` | `btw_tool_sessioninfo_platform()` |
  | `btw_tool_session_package_info()` | `btw_tool_sessioninfo_package()` |
  | `btw_tool_session_check_package_installed()` | `btw_tool_sessioninfo_is_package_installed()` |
  | `btw_tool_search_packages()` | `btw_tool_cran_search()` |
  | `btw_tool_search_package_info()` | `btw_tool_cran_package()` |
  | `btw_tool_files_list_files()` | `btw_tool_files_list()` |
  | `btw_tool_files_read_text_file()` | `btw_tool_files_read()` |
  | `btw_tool_files_write_text_file()` | `btw_tool_files_write()` |
  | `btw_tool_files_code_search()` | `btw_tool_files_search()` |

  The old names and group aliases continue to work but emit deprecation
  warnings. The `btw.files_code_search.extensions` and
  `btw.files_code_search.exclusions` options have also been renamed to
  `btw.files_search.extensions` and `btw.files_search.exclusions`, with the old
  option names emitting deprecation warnings.

## New features

* New `btw` CLI provides command-line access to btw's tool groups — **docs**,
  **pkg**, **info**, and **cran** — powered by
  [Rapp](https://github.com/r-lib/Rapp). Install with `install_btw_cli()` and
  run commands like `btw docs help dplyr::mutate` or `btw cran search
  "tidyverse"`. Output is designed for humans and LLMs: colored and formatted
  for terminals, plain markdown when piped, with a `--json` flag to return
  pipable JSON in select commands (#176).

* `btw.md` now supports configuring multiple client options. When
  `btw_client()` is called interactively, you'll be presented with a menu to
  choose which client to use. Clients can be specified as an array:

  ```yaml
  client:
    - anthropic/claude-sonnet-4
    - openai/gpt-4.1
    - aws_bedrock/us.anthropic.claude-sonnet-4-20250514-v1:0
  ```

  Or as named aliases:

  ```yaml
  client:
    haiku: aws_bedrock/us.anthropic.claude-haiku-4-5-20251001-v1:0
    sonnet: anthropic/claude-sonnet-4
    chatgpt: openai/gpt-5.2
  ```

  With aliases, you can select a client by name in the interactive menu or pass
  the alias directly via `btw_client(client = "alias")`. In non-interactive
  contexts, the first client is used automatically (#153).

* `btw_agent_tool()` allows you to create specialized custom subagents from
  `btw.md` style markdown files. Agent files are automatically discovered from
  `.btw/agent-*.md` (project and user directories) and `.claude/agents/` (for
  Claude Code compatibility), and are registered as callable tools in
  `btw_tools()`. Custom agents can specify their own system prompts, icons,
  models, and available tools (#149).

* `btw_task()` runs pre-formatted LLM tasks defined in markdown files with YAML
  frontmatter. Task files support template variable interpolation via
  `{{ variable }}` syntax, optional client and tool configuration, and all four
  execution modes (`"app"`, `"console"`, `"client"`, `"tool"`). Task files can
  also specify `title`, `icon`, `description`, and `name` fields in their YAML
  frontmatter to customize the tool definition when used in `"tool"` mode
  (#169).

* `btw_tool_agent_subagent()` enables hierarchical agent workflows by allowing
  an orchestrating LLM to delegate tasks to subagents. Each subagent runs in
  its own isolated chat session with restricted tool access and maintains
  conversation state that can be resumed via `session_id`. This allows you to
  delegate tasks to smaller cheaper models or reduce context bloat in the main
  conversation (#149).

* `btw_tool_files_edit()` makes targeted, validated line-based edits to files
  using `replace`, `insert_after`, and `replace_range` actions. Edits are
  anchored to content hashes, so stale edits are rejected if the file has
  changed. To support this, `btw_tool_files_read()` now annotates each line
  with a short hash (e.g. `2:f1a|  return("world")`) when called as a tool
  (#167).

* `btw_tool_files_replace()` finds and replaces exact string occurrences in a
  file. By default it requires the string to appear exactly once to prevent
  unintended changes; set `replace_all = TRUE` to replace all occurrences
  (#167).

* `btw_tool_pkg_load_all()` runs `pkgload::load_all()` in an isolated
  subprocess to verify package code loads correctly and trigger recompilation
  of compiled code. Useful for quick validation during development without
  running full tests or affecting the current R session (#156).

* `btw_tool_skill()` adds support for [Agent Skills](https://agentskills.io).
  Skills are modular, on-demand capabilities that provide specialized
  instructions, bundled scripts, reference docs, and asset templates to the
  LLM. Skills are discovered automatically from the btw package, attached R
  packages with `inst/skills/` directories, user-level skills, and
  project-level skills in `.btw/skills/` or `.agents/skills/`. When the skill
  tool is included in the chat client, available skills are listed in the
  system prompt. Use `btw_skill_install_github()` or
  `btw_skill_install_package()` to install skills from external sources, and
  `btw_task_create_skill()` to interactively create new skills (#145).

## Bug fixes

* `btw_app()` no longer errors with "argument is of length zero" when run
  outside of an IDE (thanks @HenrikBengtsson, #168).

* `btw_app()` no longer errors when provider built-in tools (e.g.
  `claude_tool_web_search()`) are registered with a btw client.
  Built-in tools now appear in the app sidebar under a "Built-in" group (#175).

* `btw_tool_files_read()` now correctly handles UTF-8 files containing CJK
  (Chinese, Japanese, Korean) characters. Previously, the text-file detection
  could truncate a read buffer mid-way through a multi-byte character, causing
  the file to be rejected as binary (thanks @bianchenhao, #170).

* `btw_tool_files_read()` now correctly reads valid UTF-8 files containing
  non-ASCII characters (e.g., Cyrillic). Previously, these files were
  incorrectly rejected on Windows with non-English locales when `Encoding()`
  returned "unknown" even though they were valid UTF-8 (thanks @RKonstantinR,
  #160).

# btw 1.1.0

* `btw_client()` now supports reading `CLAUDE.md` files as project context files. `CLAUDE.md` files are searched after `AGENTS.md` but before user-level `btw.md`. YAML frontmatter in `CLAUDE.md` files is stripped but not used for configuration (#146).

* `btw_app()` now shows a rich diff view in the `btw_tool_files_write_text_file()` tool, if the `{diffviewer}` package is installed (#144).

* `btw()` now correctly handles character matrices (#139).

* `btw_app()` now correctly displays input tokens with ellmer v0.4.0. Previously, we were showing the total input tokens sent across all API calls, rather than the current number of input tokens as of the last API call. We now show the size of the input context window (i.e. the number of input tokens that will be used for the conversation history in the next API call). Note that token usage is dependent on the provider and model used and is only an estimate.

* New "pkg" tool group with package development tools: `btw_tool_pkg_document()`, `btw_tool_pkg_check()`, `btw_tool_pkg_test()` and `btw_tool_pkg_coverage()` provide LLMs with the ability to document, check, test, and check test coverage of R packages during development (#133, #136).

* New `btw_tool_run_r()` tool allows LLMs to run R code and to see the output, including of plots. Because this tool lets LLMs run R arbitrary R code in the global environment (which can be great but can also have security implications), it is opt-in and disabled by default. See `?btw_tool_run_r` for more details (#126).

* `btw_tool_docs_help_page()` now uses markdown headings and sections for argument descriptions, rather than a table. This is considerably more token efficient when the argument descriptions have more than one paragraph and can't be converted into a markdown table (@jeanchristophe13v, #123).

* btw now removes large inline base64-encoded images, replacing them with a placeholder containing the image's alt text (@jeanchristophe13v, #119).

* `btw_app()` now works correctly with `{ellmer}` v0.4.0 (#121).

* `btw_app()` now shows more precise cost estimates when the cost is less than $0.10 (#121).

# btw 1.0.0

* Initial CRAN submission.
