# btw_skills_system_prompt() works

    Code
      cat(btw_skills_system_prompt())
    Output
      ## Skills
      
      When a task matches an available skill, call `btw_tool_skill(name)` to load its instructions. Don't reload skills already loaded. Resolve relative paths in a skill against the directory containing its SKILL.md. Use file read tools for bundled references; bundled scripts are for reference or adaptation into R, not directly executable by btw.
      
      Available skills:
      - skill-creator: Guide for creating effective skills. This skill should be used when users want to create a new skill (or update an existing skill) that extends Claude's capabilities with specialized knowledge, workflows, or tool integrations.
        location: SKILL_PATH

# btw_skill_prompt() errors for unknown or invalid skills

    Code
      btw_skill_prompt("nope")
    Condition
      Error in `btw_skill_resolve()`:
      ! Skill "nope" not found.
      i Available skills: "demo-skill"
      i Call `btw_tool_skill("")` to get the full, up-to-date skill listing.

---

    Code
      btw_skill_prompt("broken-skill")
    Condition
      Error in `btw_skill_resolve()`:
      ! Skill "broken-skill" exists but has validation errors:
      ! No YAML frontmatter found.

# btw_skills_register_slash_commands() requires a chat handle

    Code
      btw_skills_register_slash_commands(list())
    Condition
      Error in `btw_skills_register_slash_commands()`:
      ! `chat` must be the chat handle returned by `shinychat::chat_server()`.
      i Skill slash commands need shinychat 0.4.0.9000 or later.

