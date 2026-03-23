# Task: Create a Skill

Create a new skill for your project using interactive guidance. If
launched in app or console mode, this task will start an interactive
chat session to guide you through the process of creating a skill that
extends Claude's capabilities with specialized knowledge, workflows, or
tool integrations.

## Usage

``` r
btw_task_create_skill(
  ...,
  name = NULL,
  client = NULL,
  mode = c("app", "console", "client", "tool"),
  tools = "docs"
)
```

## Arguments

- ...:

  Additional context to provide to the AI. This can be any text or R
  objects that can be converted to text using
  [`btw()`](https://posit-dev.github.io/btw/reference/btw.md).

- name:

  Optional skill name. If provided, the AI will skip the naming step and
  use this name directly.

- client:

  An [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client, or a `provider/model` string to be passed to
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
  to create a chat client, or an alias to a client setting in your
  `btw.md` file (see "Multiple Providers" section). Defaults to
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html).
  You can use the `btw.client` option to set a default client for new
  [`btw_client()`](https://posit-dev.github.io/btw/reference/btw_client.md)
  calls, or use a `btw.md` project file for default chat client
  settings, like provider and model. We check the `client` argument,
  then the `btw.client` R option, and finally the `btw.md` project file
  (falling back to user-level `btw.md` if needed), using only the client
  definition from the first of these that is available.

- mode:

  The mode to run the task in, which affects what is returned from this
  function. `"app"` and `"console"` modes launch interactive sessions,
  while `"client"` and `"tool"` modes return objects for programmatic
  use.

- tools:

  Optional list or character vector of tools to allow the task to use
  when creating the skill. By default documentation tools are included
  to allow the task to help create package-based skills. You can include
  additional tools as needed.

  Because the task requires file tools to create skills with resources,
  tools for listing, reading and writing files are always included.

## Value

When `mode` is `"app"` or `"console"`, this function launches an
interactive session in the browser or the R console, respectively. The
ellmer chat object with the conversation history is returned invisibly
when the session ends.

When `mode` is `"client"`, this function returns the configured ellmer
chat client object. When `mode` is `"tool"`, this function returns an
ellmer tool object that can be used in other chat instances.

## See also

Other task and agent functions:
[`btw_task()`](https://posit-dev.github.io/btw/reference/btw_task.md),
[`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/reference/btw_task_create_btw_md.md),
[`btw_task_create_readme()`](https://posit-dev.github.io/btw/reference/btw_task_create_readme.md)

## Examples

``` r
withr::with_envvar(list(ANTHROPIC_API_KEY = "example"), {
  btw_task_create_skill(mode = "tool", client = "anthropic")
})
#> Using model = "claude-sonnet-4-5-20250929".
#> # <ellmer::ToolDef> btw_task_create_skill(prompt, name)
#> # @name: btw_task_create_skill
#> # @description: Create a new skill for your project with interactive guidance.
#> # @convert: TRUE
#> #
#> function (prompt, name = NULL) 
#> btw_task_create_skill_tool(prompt, name)
#> <bytecode: 0x55c8ef7a9c08>
#> <environment: 0x55c8ef7a65f0>
```
