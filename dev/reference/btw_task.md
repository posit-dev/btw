# Run a pre-formatted btw task

Runs a btw task defined in a file with YAML frontmatter configuration
and a markdown body containing the task prompt. The task file format is
similar to `btw.md` files, with client and tool configuration in the
frontmatter and the task instructions in the body.

### Task File Format

Task files use the same format as `btw.md` files:

    ---
    client:
      provider: anthropic
      model: claude-sonnet-4
    tools: [docs, files]
    ---

    Your task prompt here with {{ variable }} interpolation...

### Template Variables

The task prompt body supports template variable interpolation using
`{{ variable }}` syntax via
[`ellmer::interpolate()`](https://ellmer.tidyverse.org/reference/interpolate.html).
Pass named arguments to provide values for template variables:

    btw_task("my-task.md", package_name = "dplyr", version = "1.1.0")

### Additional Context

Unnamed arguments are treated as additional context and converted to
text using
[`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md). This
context is appended to the system prompt:

    btw_task("analyze.md", dataset_name = "mtcars", mtcars, my_function)
    #                      ^-- template var        ^-- additional context

## Usage

``` r
btw_task(
  path,
  ...,
  client = NULL,
  mode = c("app", "console", "client", "tool")
)
```

## Arguments

- path:

  Path to the task file containing YAML configuration and prompt.

- ...:

  Named arguments become template variables for interpolation in the
  task prompt. Unnamed arguments are treated as additional context
  objects and converted to text via
  [`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md).

- client:

  An [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
  client to override the task file's client configuration. If `NULL`,
  uses the client specified in the task file's YAML frontmatter, falling
  back to the default client resolution of
  [`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md).

- mode:

  The execution mode for the task:

  - `"app"`: Launch interactive Shiny app (default)

  - `"console"`: Interactive console chat with
    [`ellmer::live_console()`](https://ellmer.tidyverse.org/reference/live_console.html)

  - `"client"`: Return configured
    [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
    client without running

  - `"tool"`: Return an
    [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
    object for programmatic use

## Value

Depending on `mode`:

- `"app"`: Returns the chat client invisibly after launching the app

- `"console"`: Returns the chat client after console interaction

- `"client"`: Returns the configured chat client

- `"tool"`: Returns an
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)
  object

## See also

Other task and agent functions:
[`btw_task_create_btw_md()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_btw_md.md),
[`btw_task_create_readme()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_readme.md),
[`btw_task_create_skill()`](https://posit-dev.github.io/btw/dev/reference/btw_task_create_skill.md)

## Examples

``` r
# Create a simple task file
tmp_task_file <- tempfile(fileext = ".md")

cat(file = tmp_task_file, '---
client: anthropic/claude-sonnet-4-6
tools: [docs, files]
---

Analyze the {{ package_name }} package and create a summary.
')

# Task with template interpolation
btw_task(tmp_task_file, package_name = "dplyr", mode = "tool")
#> # <ellmer::ToolDef> btw_task_file2507943e925(prompt)
#> # @name: btw_task_file2507943e925
#> # @description: Analyze the dplyr package and create a summary.
#> # @convert: TRUE
#> #
#> function (prompt = "") 
#> {
#>     this_client <- chat_client$clone()
#>     sys_prompt <- paste0(this_client$get_system_prompt(), "\n\n---\n\n", 
#>         "YOU ARE NOW OPERATING IN TOOL MODE. ", "The user cannot respond directly to you. ", 
#>         "Because you cannot talk to the user, you will need to make ", 
#>         "your own decisions using the information available to you ", 
#>         "and the best of your abilities. ", "You may do additional exploration if needed.")
#>     this_client$set_system_prompt(sys_prompt)
#>     if (nzchar(prompt)) {
#>         this_client$chat(paste0("Additional instructions: ", 
#>             prompt))
#>     }
#>     else {
#>         this_client$chat("Please complete the task as instructed.")
#>     }
#> }
#> <bytecode: 0x56248f8803f0>
#> <environment: 0x56248f882268>

# Include additional context
btw_task(
  tmp_task_file,
  package_name = "ggplot2",
  mtcars,  # Additional context
  mode = "tool"
)
#> # <ellmer::ToolDef> btw_task_file2507943e925(prompt)
#> # @name: btw_task_file2507943e925
#> # @description: Analyze the ggplot2 package and create a summary.
#> # @convert: TRUE
#> #
#> function (prompt = "") 
#> {
#>     this_client <- chat_client$clone()
#>     sys_prompt <- paste0(this_client$get_system_prompt(), "\n\n---\n\n", 
#>         "YOU ARE NOW OPERATING IN TOOL MODE. ", "The user cannot respond directly to you. ", 
#>         "Because you cannot talk to the user, you will need to make ", 
#>         "your own decisions using the information available to you ", 
#>         "and the best of your abilities. ", "You may do additional exploration if needed.")
#>     this_client$set_system_prompt(sys_prompt)
#>     if (nzchar(prompt)) {
#>         this_client$chat(paste0("Additional instructions: ", 
#>             prompt))
#>     }
#>     else {
#>         this_client$chat("Please complete the task as instructed.")
#>     }
#> }
#> <bytecode: 0x56248f8803f0>
#> <environment: 0x56249151a128>
```
