# Render a skill's entry for a system prompt

Returns the `<skill>` block for one skill: its name, description, and
location, plus its compatibility notes and allowed tools when present.
This is the same block that
[`btw_client()`](https://posit-dev.github.io/btw/reference/btw_client.md)
writes into its system prompt.

Compose the listing yourself. For example, wrap the blocks for all
skills in an `<available_skills>` element, the way btw does it.

If the skill doesn't exist, the error lists the available skill names.

## Usage

``` r
btw_skill_prompt(skill_name)
```

## Arguments

- skill_name:

  The name of the skill, e.g. `"skill-creator"`.

## Value

A single string with the skill's `<skill>` block.

## See also

[`btw_skills_register_slash_commands()`](https://posit-dev.github.io/btw/reference/btw_skills_register_slash_commands.md)
to expose skills as slash commands in a `shinychat::chat_server()`
session.

Other skills:
[`btw_skill_install_github()`](https://posit-dev.github.io/btw/reference/btw_skill_install_github.md),
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/reference/btw_skill_install_package.md),
[`btw_skill_install_project()`](https://posit-dev.github.io/btw/reference/btw_skill_install_project.md),
[`btw_skills_register_slash_commands()`](https://posit-dev.github.io/btw/reference/btw_skills_register_slash_commands.md),
[`btw_tool_skill()`](https://posit-dev.github.io/btw/reference/btw_tool_skill.md)

## Examples

``` r
cat(btw_skill_prompt("skill-creator"))
#> <skill>
#> <name>skill-creator</name>
#> <description>Guide for creating effective skills. This skill should be used when users want to create a new skill (or update an existing skill) that extends Claude's capabilities with specialized knowledge, workflows, or tool integrations.</description>
#> <location>/home/runner/work/_temp/Library/btw/skills/skill-creator/SKILL.md</location>
#> </skill>
```
