## Skills

You have access to specialized skills that provide detailed guidance for specific tasks. Skills are loaded on-demand to provide domain-specific expertise without consuming context until needed.

### Using Skills

1. **Check available skills**: Review the `<available_skills>` listing below
2. **Fetch when relevant**: Call `btw_tool_fetch_skill(skill_name)` when a task matches a skill's description
3. **Access resources**: After fetching, use file read tools to access references

Skills may include bundled resources:
- **Scripts**: Code bundled with the skill. Scripts are not directly executable by btw; read them for reference or adapt their logic into R code for use with the R code execution tool.
- **References**: Additional documentation to consult as needed
- **Assets**: Templates and files for use in outputs
