# Tool: Subagent

`btw_tool_agent_subagent()` is a btw tool that enables hierarchical
agent workflows. When used by an LLM assistant (like
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md),
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md),
or third-party tools like Claude Code), this tool allows the
orchestrating agent to delegate complex tasks to specialized subagents,
each with their own isolated conversation thread and tool access.

This function is primarily intended to be called by LLM assistants via
tool use, not directly by end users.

### How Subagents Work

When an LLM calls this tool:

1.  A new chat session is created (or an existing one is resumed)

2.  The subagent receives the `prompt` and begins working with only the
    tools specified in the `tools` parameter

3.  The subagent works independently, making tool calls until it
    completes the task

4.  The function returns the subagent's final message text and a
    `session_id`

5.  The orchestrating agent can resume the session later by providing
    the `session_id`

Each subagent maintains its own conversation context, separate from the
orchestrating agent's context. Subagent sessions persist for the
duration of the R session.

### Tool Access

The orchestrating agent must specify which tools the subagent can use
via the `tools` parameter. The subagent is restricted to only these
tools - it cannot access tools from the parent session. Tools can be
specified by:

- **Specific tool names**:
  `c("btw_tool_files_read_text_file", "btw_tool_files_write_text_file")`

- **Tool groups**: `"files"` includes all file-related tools

- **NULL** (default): Uses the default tool set from options or
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)

### Configuration Options

Subagent behavior can be configured via R options:

- `btw.subagent.client`: The ellmer::Chat client or `provider/model`
  string to use for subagents. If not set, falls back to `btw.client`,
  then to the default Anthropic client.

- `btw.subagent.tools_default`: Default tools to provide to subagents
  when the orchestrating agent doesn't specify tools via the `tools`
  parameter. If not set, falls back to `btw.tools`, then all btw tools
  from
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).
  This is a convenience option for setting reasonable defaults.

- `btw.subagent.tools_allowed`: An allowlist of tools that subagents are
  allowed to use at all. When set, any tools requested (either
  explicitly via the `tools` parameter or from defaults) will be
  filtered against this list. If disallowed tools are requested, an
  error is thrown. This provides a security boundary to restrict
  subagent capabilities. If not set, all
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
  are allowed.

These options follow the precedence: function argument \>
`btw.subagent.*` option \> `btw.*` option \> default value. The
`tools_allowed` option acts as a filter on top of the resolved tools,
regardless of their source.

## Usage

``` r
btw_tool_agent_subagent(
  prompt,
  tools = NULL,
  session_id = NULL,
  `_intent` = ""
)
```

## Arguments

- prompt:

  Character string with the task description for the subagent. The
  subagent will work on this task using only the tools specified in
  `tools`. The subagent does not have access to the orchestrating
  agent's conversation history.

- tools:

  Optional character vector of tool names or tool groups that the
  subagent is allowed to use. Can be specific tool names (e.g.,
  `"btw_tool_files_read_text_file"`), tool group names (e.g.,
  `"files"`), or `NULL` to use the default tools from
  `btw.subagent.tools_default`, `btw.tools`, or
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).

- session_id:

  Optional character string with a session ID from a previous call. When
  provided, resumes the existing subagent conversation instead of
  starting a new one. Session IDs are returned in the result and have
  the format "adjective_noun" (e.g., "swift_falcon").

- \_intent:

  Optional string describing the intent of the tool call. Added
  automatically by the ellmer framework when tools are called by LLMs.

## Value

A `BtwSubagentResult` object (inherits from `BtwToolResult`) with:

- `value`: The final message text from the subagent

- `session_id`: The session identifier for resuming this conversation

## See also

[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
for available tools and tool groups

## Examples

``` r
# This tool is typically called by LLMs via tool use, not directly.
# The examples below show how to configure subagent behavior.

# Configure the client and default tools for subagents
withr::with_options(
  list(
    btw.subagent.client = "anthropic/claude-sonnet-4-20250514",
    btw.subagent.tools_default = "files"
  ),
  {
    getOption("btw.subagent.client")
  }
)
#> [1] "anthropic/claude-sonnet-4-20250514"

# Restrict subagents to only certain tools
withr::with_options(
  list(
    btw.subagent.tools_allowed = c("files", "docs"),
    btw.subagent.tools_default = "files"
  ),
  {
    getOption("btw.subagent.tools_allowed")
  }
)
#> [1] "files" "docs" 
```
