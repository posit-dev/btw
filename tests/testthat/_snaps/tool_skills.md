# btw_skills_system_prompt() works

    Code
      cat(btw_skills_system_prompt())
    Output
      ## Skills
      
      You have access to specialized skills that provide detailed guidance for specific tasks. Skills are loaded on-demand to provide domain-specific expertise without consuming context until needed.
      
      ### Using Skills
      
      1. **Check available skills**: Review the `<available_skills>` listing below
      2. **Fetch when relevant**: Call `btw_tool_fetch_skill(skill_name)` when a task matches a skill's description
      3. **Access resources**: After fetching, use file read tools to access references or bash/code tools to run bundled scripts
      
      Skills may include bundled resources:
      - **Scripts**: Executable code (R, Python, bash) for automated tasks
      - **References**: Additional documentation to consult as needed
      - **Assets**: Templates and files for use in outputs
      
      <available_skills>
      <skill>
      <name>skill-creator</name>
      <description>Guide for creating effective skills. This skill should be used when users want to create a new skill (or update an existing skill) that extends Claude's capabilities with specialized knowledge, workflows, or tool integrations.</description>
      </skill>
      </available_skills>

