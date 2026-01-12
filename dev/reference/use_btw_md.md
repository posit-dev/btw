# Create or edit a btw.md context file

Create or edit a `btw.md` or `AGENTS.md` context file for your project
or user-level configuration. These functions help you set up the context
files that
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
and
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
use to configure chat clients.

`use_btw_md()` creates a new context file with a default template. If
the file already exists, it will not overwrite it, but will still ensure
the file is added to `.Rbuildignore` if you're in an R package.

`edit_btw_md()` opens an existing context file for editing. Without
arguments, it opens the same file that
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
would use by default.

## Usage

``` r
use_btw_md(scope = "project")

edit_btw_md(scope = NULL)
```

## Arguments

- scope:

  The scope of the context file. Can be:

  - `"project"` (default): Creates/opens `btw.md` (by default) or
    `AGENTS.md` in the project root

  - `"user"`: Creates/opens `btw.md` in your home directory

  - A directory path: Creates/opens `btw.md` in that directory

  - A file path: Creates/opens that specific file

  For `edit_btw_md()`, `scope = NULL` (default) will find and open the
  context file that
  [`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  would use, searching first for `btw.md` and then `AGENTS.md` in the
  project directory and then for `btw.md` in your home directory.

## Value

`use_btw_md()` returns the path to the context file, invisibly.
`edit_btw_md()` is called for its side effect of opening the file.

## Functions

- `use_btw_md()`: Create a new `btw.md` or `AGENTS.md` context file in
  the current directory, the project directory or your home directory.

- `edit_btw_md()`: Open an existing `btw.md` or `AGENTS.md` context file
  for editing.

## Additional Examples

    # Create a project-level btw.md
    use_btw_md()

    # Create a user-level btw.md
    use_btw_md("user")

    # Create an AGENTS.md file
    use_btw_md("AGENTS.md")

    # Edit the context file that btw_client() would use
    edit_btw_md()

    # Edit a specific context file
    edit_btw_md("user")

## Project Context

You can use a `btw.md` or [`AGENTS.md`](https://agents.md) file to keep
track of project-specific rules, guidance and context in your project.
Either file name will work, so we'll refer primarily to `btw.md`. These
files are used automatically by
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
and
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md):
they look first for `btw.md` and then for `AGENTS.md`. If both files are
present, only the `btw.md` file will be used.

Any time you start a chat client with
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
or launch a chat session with
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md),
btw will automatically find and include the contents of the `btw.md` or
`AGENTS.md` file in the system prompt of your chat. This helps maintain
context and consistency across chat sessions.

Use `btw.md` to inform the LLM of your preferred code style, to provide
domain-specific terminology or definitions, to establish project
documentation, goals and constraints, to include reference materials
such or technical specifications, or more. Storing this kind of
information in `btw.md` may help you avoid repeating yourself and can be
used to maintain coherence across many chat sessions.

Write in markdown and structure the file in any way you wish, or use
[`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_btw_md.md)
to help you create a project context file for an existing project with
the help of an AI agent.

## Chat Settings

You can also use the `btw.md` file to choose default chat settings for
your project in a YAML front matter block at the top of the file. In
this YAML block you can choose settings for the default ellmer chat
`client`, e.g. `provider`, `model`, as well as choose which
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
to use in
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
or
[`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md).

**Chat client settings**

Use the `client` field to set options for the chat client. This can be a
single string in `provider` or `provider/model` format – as used by
[`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
– or a list of client options with `provider` and `model` fields, as
well as any other options supported by the underlying `ellmer::chat_*()`
function you choose. Note that `provider` maps to the `ellmer::chat_*()`
function, while `model` maps to the `model` argument of that function.

- Using ellmer's default model for a provider:

      ---
      client: openai
      ---

      ---
      client:
        provider: openai
      ---

- Using a specific model:

      ---
      client: anthropic/claude-4-5-sonnet-latest
      ---

      ---
      client:
        provider: anthropic
        model: claude-4-5-sonnet-latest
      ---

- Using additional client options:

      ---
      client:
        provider: ollama
        model: "gpt-oss:20b"
        echo: output
        base_url: "http://my-company.example.com:11434"
      ---

**Tool Settings**

The top-level `tools` field is used to specify which btw tools are
included in the chat. This should be a list of tool groups or tool names
(with or without the `btw_tool_` prefix). See
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
for a list of available tools and tool groups.

Here's an example `btw.md` file:

    ---
    client: claude/claude-4-5-sonnet-latest
    tools: [docs, env, files, git, ide, search, session, web]
    ---

    Follow these important style rules when writing R code:

    * Prefer solutions that use {tidyverse}
    * Always use `<-` for assignment
    * Always use the native base-R pipe `|>` for piped expressions

## Selective Context

One use-case for `btw.md` is to provide stable context for an on-going
task that might span multiple chat sessions. In this case, you can use
`btw.md` to hold the complete project plan, with background information,
requirements, and specific tasks to be completed. This can help maintain
continuity across chat sessions, especially if you update the `btw.md`
file as the project progresses.

In this use case, however, you might want to hide parts of the project
plan from the system prompt, for example to hide completed or future
tasks when their description would distract the LLM from the current
task.

You can hide parts of the `btw.md` file from the system prompt by
wrapping them in HTML `<!-- HIDE -->` and `<!-- /HIDE -->` comment tags.
A single `<!-- HIDE -->` comment tag will hide all content after it
until the next `<!-- /HIDE -->` tag, or the end of the file. This is
particularly useful when your system prompt contains notes to yourself
or future tasks that you do not want to be included in the system
prompt.

## Project or User Scope

For project-specific configuration, store your `btw.md` file in the root
of your project directory. You can even have multiple `btw.md` files in
your project, in which case the one closest to your current working
directory will be used. This makes it easy to have different `btw.md`
files for different sub-projects or sub-directories within a larger
project.

For global configuration, you can maintain a `btw.md` file in your home
directory (at `btw.md` or `.config/btw/btw.md` in your home directory,
using
[`fs::path_home()`](https://fs.r-lib.org/reference/path_expand.html)).
This file will be used by default when a project-specific `btw.md` file
is not found. Note that btw only looks for `btw.md` in your home
directory if no project-specific `btw.md` or `AGENTS.md` file is
present. It also does not look for `AGENTS.md` in your home directory.

## Interactive Setup

For an interactive guided setup, consider using
[`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_btw_md.md)
to use an LLM to help you create a `btw.md` file for your project.

## See also

Project context files are discovered automatically and included in the
system prompt by
[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md).
See
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
for a list of available tools.

## Examples

``` r
# See additional examples in the sections above

withr::with_tempdir({
  withr::with_tempfile("btw_md_tmp", fileext = ".md", {
    use_btw_md(btw_md_tmp)
  })
})
#> ✔ Created /tmp/RtmpBZEoi0/file226c75d022b2.md
#> ℹ See `?btw::btw_client()` for format details
#> ℹ See `?btw::btw_tools()` for available tools
#> ℹ Call `btw::btw_task_create_btw_md()` to use an LLM to help you initialize the
#>   project context.
```
