# Tool: Load a skill

Load a skill's specialized instructions and list its bundled resources.

Skills are modular capabilities that extend Claude's functionality with
specialized knowledge, workflows, and tools. Each skill is a directory
containing a `SKILL.md` file with instructions and optional bundled
resources (scripts, references, assets).

Skills are discovered from the following locations, in increasing order
of priority (later sources override earlier ones when skill names
conflict):

1.  Skills bundled with the btw package itself

2.  Skills from currently **attached** R packages — any package with an
    `inst/skills/` directory that is loaded via
    [`library()`](https://rdrr.io/r/base/library.html) or
    [`require()`](https://rdrr.io/r/base/library.html)

3.  User-level skills (`tools::R_user_dir("btw", "config")/skills`)

4.  Project-level skills (`.btw/skills/` or `.agents/skills/`)

## Usage

``` r
btw_tool_skill(name, `_intent` = "")
```

## Arguments

- name:

  The name of the skill to load.

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
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_package.md)
