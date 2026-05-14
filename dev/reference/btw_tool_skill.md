# Tool: Load a skill

Load a skill's specialized instructions and list its bundled resources.

Skills are modular capabilities that extend Claude's functionality with
specialized knowledge, workflows, and tools. Each skill is a directory
containing a `SKILL.md` file with instructions and optional bundled
resources (scripts, references, assets).

When `btw_tool_skill` is included in the chat client's tools, btw
automatically injects information about available skills into the system
prompt so the model knows which skills are available. If the skill tool
is added after client creation, the model can call `btw_tool_skill("")`
(empty name) to get the current skill listing.

Skills are discovered from the following locations, in increasing order
of priority (later sources override earlier ones when skill names
conflict):

1.  Skills bundled with the btw package itself

2.  Skills from currently **attached** R packages — any package with an
    `inst/skills/` directory that is loaded via
    [`library()`](https://rdrr.io/r/base/library.html) or
    [`require()`](https://rdrr.io/r/base/library.html)

3.  User-level skills (`~/.btw/skills`, `~/.config/btw/skills`,
    `tools::R_user_dir("btw")/skills`). For backwards compatibility, the
    legacy `tools::R_user_dir("btw", "config")/skills` path used by
    briefly by btw 1.2.0 is also included at lower priority.

4.  Project-level skills (`.btw/skills/` or `.agents/skills/`)

The default user-level and project-level directories can be replaced by
setting the `btw.skills.paths` R option or the `BTW_SKILLS_PATHS`
environment variable. When set, the value **entirely replaces** all
user-level and project-level directories (items 3 and 4 above).
Package-bundled skills and skills from attached packages (items 1 and 2)
are always included regardless of this setting. The R option takes
precedence over the environment variable. Multiple paths can be provided
as a character vector (e.g.
`options(btw.skills.paths = c("/path/a", "/path/b"))`) or as a single
path-separator-delimited string (`:` on Unix/Mac, `;` on Windows, which
is the only form supported by environment variables). Non-existent paths
are silently skipped.

**Resolution timing:** options and environment variables are read at
**tool-registration time** (i.e. when
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
or
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
is called). The resolved paths are captured in the tool's closure so
that they remain correct even if the options are later modified or go
out of scope (for example, when
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
restores options after returning). If you need different directories for
a new session, create a new client.

## Usage

``` r
btw_tool_skill(name, `_intent` = "")
```

## Arguments

- name:

  The name of the skill to load, or `""` to list all available skills.

- \_intent:

  An optional string describing the intent of the tool use. When the
  tool is used by an LLM, the model will use this argument to explain
  why it called the tool.

## Value

A `btw_tool_result` containing the skill instructions and a listing of
bundled resources with their paths.

## See also

Other skills:
[`btw_skill_install_github()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_github.md),
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_package.md),
[`btw_skill_install_project()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_project.md)
