# Render a skill's entry for a system prompt

Returns a compact YAML-style entry for one skill: its name, description,
and location, plus compatibility notes and allowed tools when present.
This is the same entry that
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
writes into its system prompt.

Compose the listing yourself by joining the entries under an "Available
skills:" heading, the way btw does it.

If the skill doesn't exist, the error lists the available skill names.

## Usage

``` r
btw_skill_prompt(skill_name)
```

## Arguments

- skill_name:

  The name of the skill, e.g. `"skill-creator"`.

## Value

A single string with the skill's YAML-style entry.

## See also

[`btw_skills_register_slash_commands()`](https://posit-dev.github.io/btw/dev/reference/btw_skills_register_slash_commands.md)
to expose skills as slash commands in a
[`shinychat::chat_server()`](https://posit-dev.github.io/shinychat/r/reference/chat_app.html)
session.

Other skills:
[`btw_skill_install_github()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_github.md),
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_package.md),
[`btw_skill_install_project()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_project.md),
[`btw_skills_register_slash_commands()`](https://posit-dev.github.io/btw/dev/reference/btw_skills_register_slash_commands.md),
[`btw_tool_skill()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_skill.md)

## Examples

``` r
cat(btw_skill_prompt("skill-creator"))
#> - skill-creator: Guide for creating effective skills. This skill should be used when users want to create a new skill (or update an existing skill) that extends Claude's capabilities with specialized knowledge, workflows, or tool integrations.
#>   location: /home/runner/work/_temp/Library/btw/skills/skill-creator/SKILL.md
```
