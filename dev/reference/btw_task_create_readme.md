# Task: Create a Polished README

Create a compelling, user-focused README file for your project. If
launched in app or console mode, this task will start an interactive
chat session to guide you through the process of creating a polished
README that clearly communicates value and helps potential users make
informed decisions.

This task focuses on creating READMEs for END USERS, not developers,
with emphasis on clarity, accessibility, and authentic communication of
value. The process involves exploring your project files, understanding
your target audience and goals, proposing a structure, and then
iteratively drafting each section with your input.

## Usage

``` r
btw_task_create_readme(
  ...,
  client = NULL,
  mode = c("app", "console", "client", "tool")
)
```

## Arguments

- ...:

  Additional context to provide to the AI. This can be any text or R
  objects that can be converted to text using
  [`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md).

- client:

  An [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client, or a `provider/model` string to be passed to
  [`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat-any.html)
  to create a chat client, or an alias to a client setting in your
  `btw.md` file (see "Multiple Providers" section). Defaults to
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html).
  You can use the `btw.client` option to set a default client for new
  [`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
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
[`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_btw_md.md)

## Examples

``` r
withr::with_envvar(list(ANTHROPIC_API_KEY = "example"), {
  btw_task_create_readme(mode = "tool", client = "anthropic")
})
#> Using model = "claude-sonnet-4-5-20250929".
#> # <ellmer::ToolDef> btw_task_create_readme(prompt)
#> # @name: btw_task_create_readme
#> # @description: Create a polished, user-focused README file for your project.
#> # @convert: TRUE
#> #
#> function (prompt) 
#> btw_task_create_readme_tool(prompt)
#> <bytecode: 0x55f26cef5418>
#> <environment: 0x55f26cef4df8>
```
