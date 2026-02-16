# Agent Skills in btw — Feature Documentation

> Current state as of 2026-02-16, branch `feat/skills`.

## Overview

btw implements the [Agent Skills specification](https://agentskills.io) to let
R users extend LLM capabilities with reusable, modular instructions. Skills are
directories containing a `SKILL.md` file (with YAML frontmatter + markdown
instructions) and optional resource subdirectories (`scripts/`, `references/`,
`assets/`).

The implementation follows a **progressive disclosure** pattern:

1. **Metadata** (~100 tokens/skill): Loaded at startup into the system prompt
2. **Instructions** (<5k tokens): Loaded when the agent activates a skill
3. **Resources** (unbounded): Loaded on-demand by the agent as needed

## Architecture

### Data Flow

```
btw_client() startup
    │
    ├── btw_skills_system_prompt()
    │       │
    │       ├── btw_skill_directories()    → list of dirs to scan
    │       ├── btw_list_skills()          → metadata from all valid skills
    │       │       └── validate_skill()   → skip invalid, warn
    │       │       └── extract_skill_metadata() → frontmatter::read_front_matter()
    │       │
    │       └── generates <available_skills> XML for system prompt
    │
    └── system prompt includes skill names, descriptions, locations

Agent activates a skill
    │
    ├── btw_tool_fetch_skill(skill_name)
    │       ├── find_skill()               → locate by name across all dirs
    │       ├── frontmatter::read_front_matter() → body content
    │       ├── list_skill_resources()      → recursive file listing
    │       └── returns SKILL.md body + resource paths as btw_tool_result
    │
    └── Agent uses file-read tools for references, adapts scripts to R
```

### Discovery Locations

Skills are discovered in this order (later entries override earlier by name):

| Priority | Location | Purpose |
|----------|----------|---------|
| 1 | `system.file("skills", package = "btw")` | Package-bundled skills |
| 2 | `tools::R_user_dir("btw", "config")/skills` | User-level (global install) |
| 3 | `.btw/skills/` in working directory | Project-level (preferred) |
| 4 | `.agents/skills/` in working directory | Project-level (cross-tool convention) |
| 5 | `.claude/skills/` in working directory | Project-level (Claude Code convention) |

All existing project-level directories are scanned for discovery. When
**installing or creating** skills with `scope = "project"`, if multiple
project dirs exist the user is prompted interactively; if none exist, defaults
to `.btw/skills/`.

Controlled by: `btw_skill_directories()`, `project_skill_subdirs()`,
`resolve_project_skill_dir()` — all in `R/tool-skills.R:93-162`.

### System Prompt Integration

The skills section is injected into the system prompt by `btw_client()` at
`R/btw_client.R:154`. It sits between the tools prompt and the project context
prompt. The section is only included if skills exist (`nzchar(skills_prompt)`).

The XML format follows the Agent Skills integration guide:

```xml
<available_skills>
<skill>
<name>skill-creator</name>
<description>Guide for creating effective skills...</description>
<location>/path/to/skills/skill-creator/SKILL.md</location>
</skill>
</available_skills>
```

Optional fields `<compatibility>` and `<allowed-tools>` are included when
present in the skill's frontmatter.

The explanation text before the XML comes from `inst/prompts/skills.md`.

### MCP Exclusion

The `btw_tool_fetch_skill` tool is excluded from `btw_mcp_server()` by default
via `btw_mcp_tools()` in `R/mcp.R:176`. Rationale: The `<available_skills>`
system prompt is injected by `btw_client()`, not by MCP. Without that metadata,
the model has no context about what skills exist. Filesystem-based agents (like
Claude Code) can read SKILL.md files directly via their `<location>` paths.

### Validation

`validate_skill()` (`R/tool-skills.R:242-343`) implements the full spec:

- `name`: required, max 64 chars, `^[a-z0-9][a-z0-9-]*[a-z0-9]$`, no `--`, must match directory name
- `description`: required, max 1024 chars
- `compatibility`: optional, max 500 chars
- `metadata`: optional, must be a key-value mapping
- `allowed-tools`: optional (experimental, not enforced)
- No unexpected frontmatter fields allowed

Invalid skills are **warned about and skipped** during discovery (not errors).

### Tool Registration

The fetch skill tool is registered via `.btw_add_to_tools()` at
`R/tool-skills.R:65-91` with:
- Group: `"skills"`
- Icon: `quick-reference` (see `R/tools.R:223`)
- Conditional registration: only if `btw_list_skills()` returns >0 skills

## Key Files

| File | What it contains |
|------|-----------------|
| `R/tool-skills.R` | All skills logic: tool, discovery, validation, resources, system prompt, user-facing functions |
| `R/mcp.R:156,176` | MCP server default tools exclude skills; `btw_mcp_tools()` helper |
| `R/btw_client.R:154-163` | System prompt injection point |
| `R/tools.R:223` | Skills group icon mapping |
| `inst/prompts/skills.md` | System prompt explanation text shown to the model |
| `inst/skills/skill-creator/` | Bundled meta-skill for creating new skills |
| `inst/skills/skill-creator/SKILL.md` | Comprehensive guide (~356 lines) |
| `inst/skills/skill-creator/scripts/` | Python helpers: `init_skill.py`, `quick_validate.py`, `package_skill.py` |
| `inst/skills/skill-creator/references/` | `workflows.md`, `output-patterns.md` |
| `tests/testthat/test-tool_skills.R` | 110 tests covering all functionality |
| `tests/testthat/_snaps/tool_skills.md` | Snapshot for system prompt output |
| `man/btw_tool_fetch_skill.Rd` | Tool documentation |
| `man/btw_skill_create.Rd` | `btw_skill_create()` docs |
| `man/btw_skill_validate.Rd` | `btw_skill_validate()` docs |
| `man/btw_skill_install_github.Rd` | `btw_skill_install_github()` docs |
| `man/btw_skill_install_package.Rd` | `btw_skill_install_package()` docs |
| `NAMESPACE` | Exports: `btw_tool_fetch_skill`, `btw_skill_create`, `btw_skill_validate`, `btw_skill_install_github`, `btw_skill_install_package` |

## Exported Functions

### For agents (tool)

- **`btw_tool_fetch_skill(skill_name)`** — Fetches a skill's SKILL.md body and
  lists resource paths. Returns a `btw_tool_result`.

### For users (interactive R)

- **`btw_skill_create(name, description, scope, resources)`** — Initialize a
  new skill directory with SKILL.md template. Validates name format.
- **`btw_skill_validate(path)`** — Validate a skill against the spec. Returns
  `list(valid, issues)`.
- **`btw_skill_install_github(repo, skill, ref, scope)`** — Install a skill
  from a GitHub repository. Downloads the repo zipball, discovers skills
  (directories containing `SKILL.md`), and installs. Requires the `gh` package.
- **`btw_skill_install_package(package, skill, scope)`** — Install a skill
  bundled in an R package's `inst/skills/` directory. Requires the target
  package to be installed.

## Internal Functions

| Function | Purpose |
|----------|---------|
| `btw_skill_directories()` | Returns all directories to scan for skills |
| `project_skill_subdirs()` | Returns the three project-level subdir paths |
| `resolve_project_skill_dir()` | Picks one project dir for install/create (interactive prompt if ambiguous) |
| `install_skill_from_dir(source_dir, scope)` | Validates and copies a skill directory to the target scope |
| `select_skill_dir(skill_dirs, skill, source_label)` | Shared selection logic: match by name, auto-select single, interactive menu |
| `btw_list_skills()` | Scans all dirs, validates, returns metadata list |
| `find_skill(name)` | Finds a specific skill by name across all dirs |
| `extract_skill_metadata(path)` | Parses YAML frontmatter from a SKILL.md file |
| `validate_skill(dir)` | Full spec validation, returns `list(valid, issues)` |
| `list_skill_resources(dir)` | Lists files in scripts/, references/, assets/ |
| `list_files_in_subdir(base, sub)` | Recursive file listing helper |
| `has_skill_resources(resources)` | Checks if any resources exist |
| `format_resources_listing(resources, base)` | Formats resource paths for display |
| `btw_skills_system_prompt()` | Generates full skills section for system prompt |
| `btw_mcp_tools()` | `btw_tools()` minus the skills group |

## Known Limitations

### Script Execution

Skills can bundle scripts in `scripts/`, but btw has no tool for executing
them directly. The system prompt instructs the agent to read scripts for
reference or adapt their logic into R code for `btw_tool_run_r`. This is
intentional — executing arbitrary bundled scripts requires a tool approval
system that btw does not yet have.

### `allowed-tools` Not Enforced

The `allowed-tools` frontmatter field is parsed and surfaced in the system
prompt, but btw does not enforce it. The spec itself marks this as experimental.

### No `btw_skill_list()` Export

There is no exported user-facing function to list available skills interactively.
The internal `btw_list_skills()` serves this purpose and skills are listed in
the system prompt and in error messages from `btw_tool_fetch_skill()`.

## Dependencies

- **`frontmatter`** (Imports): Used for YAML frontmatter parsing via
  `frontmatter::read_front_matter()`. This replaced the `yaml` package which
  was removed from Suggests.
- **`fs`** (Imports): Used for `fs::dir_copy()` in `install_skill_from_dir()`.
- **`gh`** (suggested at runtime): Used by `btw_skill_install_github()` to
  download repository zipballs. Checked via `rlang::check_installed()`.
- **`ellmer`** (Imports): Tool registration via `ellmer::tool()` and
  `ellmer::tool_annotations()`.

## Specification Reference

- Spec: `_dev/agents/agent-skills/specification.md` (local copy)
- Integration guide: `_dev/agents/agent-skills/spec-integrate-skills.md` (local copy)
- Canonical URL: https://agentskills.io
- Gap analysis: `_dev/agents/agent-skills/gap-analysis.md`
- Implementation plan: `_dev/agents/agent-skills/implementation-plan.md`

## Test Patterns

Tests use helpers defined at the top of `test-tool_skills.R`:

- `create_temp_skill(name, description, extra_frontmatter, body, dir)` — Creates
  a temp skill directory with valid SKILL.md. Uses `withr::local_tempdir()` for
  automatic cleanup.
- `local_skill_dirs(dirs)` — Mocks `btw_skill_directories()` to return only the
  specified directories, isolating tests from real installed skills.
- `create_github_zipball(skills)` — Builds a ZIP mimicking GitHub's zipball
  format (`owner-repo-sha/skill-name/SKILL.md`). Used with a mocked `gh::gh()`
  to test `btw_skill_install_github()` without network access.

GitHub install tests mock `gh::gh()` to copy a pre-built zipball to `.destfile`.
Package install tests mock `base::system.file()` to point to a temp directory.
Both mock `rlang::check_installed()` to skip dependency checks in the test env.

The snapshot test for `btw_skills_system_prompt()` uses a transform to scrub
machine-specific `<location>` paths.
