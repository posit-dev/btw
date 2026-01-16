# Create a custom agent tool from a markdown file

Creates an
[`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
from a markdown file that defines a custom agent. The tool can be
registered with a chat client to delegate tasks to a specialized
assistant with its own system prompt and tool configuration.

### Agent File Format

Agent files use YAML frontmatter to configure the agent, with the
markdown body becoming the agent's system prompt. The file should be
named `agent-{name}.md`.

#### Required Fields

- `name`: A valid R identifier (letters, numbers, underscores) that
  becomes part of the tool name: `btw_tool_agent_{name}`. The final name
  cannot conflict with any existing
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
  names.

#### Optional Fields

- `description`: Tool description shown to the LLM. Defaults to a
  generic delegation message.

- `title`: User-facing title for the tool. Defaults to title-cased name.

- `icon`: Icon specification for the agent (see **Icon Specification**
  below). Defaults to the standard agent icon.

- `client`: Model specification like
  `"anthropic/claude-sonnet-4-20250514"`. Falls back to
  `btw.subagent.client` or `btw.client` options.

- `tools`: List of tool names or groups available to this agent.
  Defaults to all non-agent tools.

#### Icon Specification

The `icon` field supports three formats:

1.  **Plain icon name**: Uses
    [`shiny::icon()`](https://rdrr.io/pkg/shiny/man/icon.html) (Font
    Awesome icons). Example: `icon: robot` or `icon: code`

2.  **Raw SVG**: Starts with `<svg` and is used literally. Example:
    `icon: '<svg viewBox="0 0 24 24">...</svg>'`

3.  **Package-prefixed icon**: Uses `pkg::icon-name` format to specify
    icons from other icon packages. Supported packages:

    |  |  |  |
    |----|----|----|
    | Package | Syntax | Function Called |
    | fontawesome | `fontawesome::home` | [`fontawesome::fa()`](https://rstudio.github.io/fontawesome/reference/fa.html) |
    | bsicons | `bsicons::house` | `bsicons::bs_icon()` |
    | phosphoricons | `phosphoricons::house` | `phosphoricons::ph()` |
    | rheroicons | `rheroicons::home` | `rheroicons::rheroicon()` |
    | tabler | `tabler::home` | `tabler::icon()` |
    | shiny | `shiny::home` | [`shiny::icon()`](https://rdrr.io/pkg/shiny/man/icon.html) |

    The specified package must be installed. If the package is missing
    or the icon name is invalid, a warning is issued and the default
    agent icon is used.

#### Example Agent File

    ---
    name: code_reviewer
    description: Reviews code for best practices and potential issues.
    title: Code Reviewer
    icon: magnifying-glass
    tools:
      - files
      - docs
    ---

    You are a code reviewer. Analyze code for:
    - Best practices and style
    - Potential bugs or issues
    - Performance considerations

    Provide specific, actionable feedback.

### Automatic Discovery

Agent files are automatically discovered by
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
when placed in the following locations (in order of priority):

- **Project level (btw)**: `.btw/agent-*.md` in your project directory

- **User level (btw)**: `~/.btw/agent-*.md` or
  `~/.config/btw/agent-*.md`

- **Project level (Claude Code)**: `.claude/agents/*.md` in your project
  directory

- **User level (Claude Code)**: `~/.claude/agents/*.md`

btw-style agents take precedence over Claude Code agents with the same
name. When duplicate agent names are found, a warning is issued.

### Claude Code Compatibility

btw supports loading agent files from Claude Code's `.claude/agents/`
directory for compatibility. However, there are some small differences
when Claude Code agents are used in btw:

- **Name normalization**: Agent names with hyphens (e.g.,
  `code-reviewer`) are automatically converted to underscores
  (`code_reviewer`) for R compatibility.

- **Ignored fields**: The following Claude Code fields are ignored (with
  a warning): `model`, `tools`, `permissionMode`, `skills`. Use btw's
  `client` field instead of `model`, and btw agents use default tools.

- **`client` argument**: Use the `client` argument to manually override
  the model for any agent file.

## Usage

``` r
btw_agent_tool(path, client = NULL)
```

## Arguments

- path:

  Path to an agent markdown file.

- client:

  Optional. A client specification to override the agent's configured
  client. Can be a string like `"anthropic/claude-sonnet-4-20250514"`,
  an [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  object, or a list with `provider` and `model` keys. If `NULL`
  (default), uses the `client` field from the agent file or falls back
  to btw's default client resolution.

## Value

An [`ellmer::ToolDef`](https://ellmer.tidyverse.org/reference/tool.html)
object that can be registered with a chat client, or `NULL` if the file
is invalid (with a warning).

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
for automatic agent discovery,
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
for creating chat clients with tools.

## Examples

``` r
# Create a btw-style agent file
withr::with_tempdir({
  dir.create(".btw")
  writeLines(
    c(
      "---",
      "name: code_reviewer",
      "description: Reviews code for best practices.",
      "---",
      "",
      "You are a code reviewer. Analyze code for best practices."
    ),
    ".btw/agent-code_reviewer.md"
  )

  tool <- btw_agent_tool(".btw/agent-code_reviewer.md")
  # Use `chat$register_tool(tool)` to register with an ellmer chat client

  tool
})
#> # <ellmer::ToolDef> btw_tool_agent_code_reviewer(prompt, session_id)
#> # @name: btw_tool_agent_code_reviewer
#> # @description: Reviews code for best practices.
#> # @convert: TRUE
#> #
#> function (prompt, session_id = NULL) 
#> {
#>     btw_tool_agent_custom_impl(prompt = prompt, session_id = session_id, 
#>         agent_config = agent_config)
#> }
#> <bytecode: 0x55dc53a593f0>
#> <environment: 0x55dc53a59000>

# Create a Claude Code-style agent file (name with hyphens)
withr::with_tempdir({
  dir.create(".claude/agents", recursive = TRUE)
  writeLines(
    c(
      "---",
      "name: test-helper",
      "description: Helps write tests.",
      "model: sonnet",
      "---",
      "",
      "You help write tests for R code."
    ),
    ".claude/agents/test-helper.md"
  )

  tool <- btw_agent_tool(".claude/agents/test-helper.md")
  # Use `chat$register_tool(tool)` to register with an ellmer chat client

  tool
})
#> # <ellmer::ToolDef> btw_tool_agent_test_helper(prompt, session_id)
#> # @name: btw_tool_agent_test_helper
#> # @description: Helps write tests.
#> # @convert: TRUE
#> #
#> function (prompt, session_id = NULL) 
#> {
#>     btw_tool_agent_custom_impl(prompt = prompt, session_id = session_id, 
#>         agent_config = agent_config)
#> }
#> <bytecode: 0x55dc53a593f0>
#> <environment: 0x55dc50f08458>
```
