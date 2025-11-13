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
adding a `btw.md` file or [`AGENTS.md`](https://agents.md/) in your
project directory. See
[`use_btw_md()`](https://posit-dev.github.io/btw/dev/reference/use_btw_md.md)
for help creating a `btw.md` file in your project, or use `path_btw` to
tell `btw_client()` to use a specific context file.

`btw_client()` will also include context from an `llms.txt` file in the
system prompt, if one is found in your project directory or as specified
by the `path_llms_txt` argument.

### Client Settings with User-Level Fallback

Client settings in `client` and `tools` from a project-level `btw.md` or
`AGENTS.md` file take precedence. If a project file doesn't specify a
setting, btw will fall back to settings in a user-level `btw.md` file
(typically in `~/btw.md` or `~/.config/btw/btw.md`). Project-level btw
tool options under the `options` key are merged with user-level options,
with project-level options taking precedence.

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

## Usage

``` r
btw_client(
  ...,
  client = NULL,
  tools = NULL,
  path_btw = NULL,
  path_llms_txt = NULL
)

btw_app(..., client = NULL, tools = NULL, path_btw = NULL, messages = list())
```

## Arguments

- ...:

  In `btw_app()`, additional arguments are passed to
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html). In
  `btw_client()`, additional arguments are ignored.

- client:

  An [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client or a `provider/model` string to be passed to
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
  to create a chat client. Defaults to
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

  A path to a `btw.md` or `AGENTS.md` project context file. If `NULL`,
  btw will find a project-specific `btw.md` or `AGENTS.md` file in the
  parents of the current working directory, with fallback to user-level
  `btw.md` if no project file is found. Set `path_btw = FALSE` to create
  a chat client without using a `btw.md` file.

- path_llms_txt:

  A path to an `llms.txt` file containing context about the current
  project. By default, btw will look for an `llms.txt` file in the your
  current working directory or its parents. Set `path_llms_txt = FALSE`
  to skip looking for an `llms.txt` file.

- messages:

  A list of initial messages to show in the chat, passed to
  [`shinychat::chat_mod_ui()`](https://rdrr.io/pkg/shinychat/man/chat_app.html).

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
