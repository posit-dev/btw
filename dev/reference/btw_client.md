# Create a btw-enhanced ellmer chat client

Creates an
[ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html) client,
enhanced with the tools from
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).
Use `btw_client()` to create the chat client for general or interactive
use at the console, or `btw_app()` to create a chat client and launch a
Shiny app for chatting with a btw-enhanced LLM in your local workspace.

### Project Context

You can keep track of project-specific rules, guidance and context by
adding a `btw.md` file, [`AGENTS.md`](https://agents.md/), or
`CLAUDE.md` in your project directory. See
[`use_btw_md()`](https://posit-dev.github.io/btw/dev/reference/use_btw_md.md)
for help creating a `btw.md` file in your project, or use `path_btw` to
tell `btw_client()` to use a specific context file. Note that
`CLAUDE.md` files will have their YAML frontmatter stripped but not used
for configuration.

`btw_client()` will also include context from an `llms.txt` file in the
system prompt, if one is found in your project directory or as specified
by the `path_llms_txt` argument.

### Client Settings with User-Level Fallback

Client settings in `client` and `tools` from a project-level `btw.md` or
`AGENTS.md` file take precedence. If a project file doesn't specify a
setting, btw will fall back to settings in a user-level `btw.md` file.
This is `~/btw.md` if present, otherwise `~/.btw/btw.md`,
`~/.config/btw/btw.md`, or `tools::R_user_dir("btw")`, in that order.
See `?btw-config` for the complete list of user-level locations.
Project-level btw tool options under the `options` key are merged with
user-level options, with project-level options taking precedence.

Project-specific instructions from both files are combined with a
divider, allowing you to maintain global guidelines in your user file
and project-specific rules in your project file.

### Client Options

The following R options are consulted when creating a new btw chat
client and take precedence over settings in a `btw.md` file:

- `btw.client`: The
  [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client or a `provider/model` string (see
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html))
  to use as the basis for new `btw_client()` or `btw_app()` chats.

- `btw.tools`: The btw tools to include by default when starting a new
  btw chat, see
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
  for details.\`

### Multiple Providers and Models

You can configure multiple client options in your `btw.md` file. When
`btw_client()` is called interactively from the console, you'll be
presented with a menu to choose which client to use. In non-interactive
contexts, the first client is used automatically.

**Array format** (unnamed list):

    client:
      - anthropic/claude-sonnet-4
      - openai/gpt-4.1
      - aws_bedrock/us.anthropic.claude-sonnet-4-20250514-v1:0

**Alias format** (named list):

    client:
      haiku: aws_bedrock/us.anthropic.claude-haiku-4-5-20251001-v1:0
      sonnet:
        provider: aws_bedrock
        model: us.anthropic.claude-sonnet-4-5-20250929-v1:0

With aliases, you can select a client by name in the interactive menu or
pass the alias directly: `btw_client(client = "sonnet")`.

## Usage

``` r
btw_client(
  ...,
  client = NULL,
  tools = NULL,
  path_btw = NULL,
  path_llms_txt = NULL
)

btw_app(
  ...,
  client = NULL,
  tools = NULL,
  path_btw = NULL,
  messages = list(),
  model_choices = c("auto", "btw_md", "provider", "none")
)
```

## Arguments

- ...:

  In `btw_app()`, additional arguments are passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html). In
  `btw_client()`, additional arguments are ignored.

- client:

  An [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client, or a `provider/model` string to be passed to
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
  to create a chat client, or an alias to a client setting in your
  `btw.md` file (see "Multiple Providers" section). Defaults to
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html).
  You can use the `btw.client` option to set a default client for new
  `btw_client()` calls, or use a `btw.md` project file for default chat
  client settings, like provider and model. We check the `client`
  argument, then the `btw.client` R option, and finally the `btw.md`
  project file (falling back to user-level `btw.md` if needed), using
  only the client definition from the first of these that is available.

- tools:

  A list of tools to include in the chat, defaults to
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).
  Join
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
  with additional tools defined by
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
  to include additional tools in the chat client. Alternatively, you can
  use a character values to refer to specific btw tools by name or by
  group. For example, use `tools = "docs"` to include only the
  documentation related tools, or `tools = c("env", "docs")` to include
  the environment and documentation tools, and so on. You can also refer
  to btw tools by name, e.g. `tools = "btw_tool_docs_help_page"` or
  alternatively in the shorter form `tools = "docs_help_page"`. Finally,
  set `tools = FALSE` to skip registering btw tools with the chat
  client.

- path_btw:

  A path to a `btw.md`, `AGENTS.md`, or `CLAUDE.md` project context
  file. If `NULL`, btw will find a project-specific context file by
  walking up the parents of the current working directory, preferring
  `btw.md`, then `AGENTS.md`, then `CLAUDE.md`, with fallback to a
  user-level `btw.md` file if no project file is found. See
  `?btw-config` for the complete list of locations. Set
  `path_btw = FALSE` to create a chat client without using a `btw.md`
  file.

- path_llms_txt:

  A path to an `llms.txt` file containing context about the current
  project. By default, btw will look for an `llms.txt` file in the your
  current working directory or its parents. Set `path_llms_txt = FALSE`
  to skip looking for an `llms.txt` file.

- messages:

  A list of initial messages to show in the chat, passed to
  [`shinychat::chat_mod_ui()`](https://posit-dev.github.io/shinychat/r/reference/chat_app.html).

- model_choices:

  Can be one of `"btw_md"` (model choices from your `path_btw`
  configuration), `"provider"` (models from the provider API), `"auto"`
  (uses `path_btw` if `client` comes from `path_btw`, otherwise falling
  back to provider), or `"none"` (don't show model choices).

## Value

Returns an
[ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html) object
with additional tools registered from
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).
`btw_app()` returns the chat object invisibly, and the chat object with
the messages added during the chat session.

## Functions

- `btw_client()`: Create a btw-enhanced
  [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client

- `btw_app()`: Create a btw-enhanced client and launch a Shiny app to
  chat

## User (global) locations

btw's user-level home is the `~/.btw/` directory. Skills and btw-style
agents live there (and in two additional directories), while the
user-level `btw.md` configuration file may also live directly in your
home directory.

The user-level directories, in decreasing order of priority, are:

1.  `~/.btw/` (recommended)

2.  `~/.config/btw/`

3.  The platform-specific R user config directory,
    `tools::R_user_dir("btw")`

Skills are discovered in the `skills/` subdirectory of each (e.g.
`~/.btw/skills/`), and btw-style agents are discovered both as
`agent-*.md` files directly inside each (e.g.
`~/.btw/agent-my_agent.md`) and as `*.md` files in an `agents/`
subdirectory of each (e.g. `~/.btw/agents/my_agent.md`).

The user-level `btw.md` **configuration file** is searched for in a
slightly different order: a `btw.md` directly in your home directory
(`~/btw.md`) takes precedence, followed by `btw.md` in each of the
directories above (`~/.btw/btw.md`, `~/.config/btw/btw.md`,
`tools::R_user_dir("btw")/btw.md`). `~/btw.md` remains a first-class
location; `use_btw_md("user")` creates new configuration in the
recommended `~/.btw/` directory. If more than one user-level `btw.md`
exists, the highest-priority one is used and btw warns once per session.

On Windows, R's notion of your home directory
([`fs::path_home_r()`](https://fs.r-lib.org/reference/path_expand.html),
typically your `Documents` folder) can differ from your user profile
directory
([`fs::path_home()`](https://fs.r-lib.org/reference/path_expand.html));
btw searches both.

## Project locations

Project-level configuration is discovered by walking up from the current
working directory to the project root, where the project root is
identified by the presence of a `DESCRIPTION`, `.git`, `.vscode`,
`.here`, or `*.Rproj` file.

- **Config file**: the nearest `btw.md` file, or if none is found, the
  nearest `AGENTS.md` file, or if neither is found, the nearest
  `CLAUDE.md` file. (`btw.md` \> `AGENTS.md` \> `CLAUDE.md`)

- **btw-style agents**: `.btw/agents/*.md` or `.btw/agent-*.md`

- **Skills**: `.btw/skills/` or `.agents/skills/`

## Examples

``` r
if (FALSE) { # rlang::is_interactive()
withr::local_options(list(
  btw.client = ellmer::chat_ollama(model="llama3.1:8b")
))

chat <- btw_client()
chat$chat(
  "How can I replace `stop()` calls with functions from the cli package?"
)
}
```
