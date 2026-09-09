# Register skill slash commands in a chat

Registers a slash command for every skill discovered by the skill tool,
using the chat handle returned by `shinychat::chat_server()`. Requires
shinychat 0.4.0.9000 or later.

Each skill is registered under its own name, e.g. `/skill-creator`. When
the user submits the command, btw sends the skill's full instructions to
the model, followed by the text the user typed after the command. The
chat UI shows `/skill-creator <user text>`; the model receives the
skill's instructions and the user text separated by a blank line.

If the command fails, btw restores the original slash text to the chat
input and shows a toast with the error.

Skill commands can't run while a response is streaming; in that case the
command fails the same way, restoring the input and showing an error
toast.

Slash command names may only contain letters, numbers, underscores, and
hyphens. Skills with other names are skipped with a warning, as are
skills whose names match btw's own `/btw-*` slash commands and any names
passed to `reserved`.

Because `shinychat::chat_server()` must be called from within a Shiny
app, this function can't be demonstrated with a runnable example. Inside
your app's server function:

    server <- shinychat::chat_server("chat", client = ellmer::chat_openai())
    btw_skills_register_slash_commands(server)

## Usage

``` r
btw_skills_register_slash_commands(chat, reserved = character())
```

## Arguments

- chat:

  The chat handle returned by `shinychat::chat_server()`.

- reserved:

  Skill names to skip, as a character vector. Pass the names of slash
  commands you registered yourself so a skill can't take their place.
  btw_app(), for example, passes its own `/new` and `/clear` commands.

## Value

`chat`, invisibly.

## See also

[`btw_tool_skill()`](https://posit-dev.github.io/btw/reference/btw_tool_skill.md)
for a skill's full instructions and
[btw-config](https://posit-dev.github.io/btw/reference/btw-config.md)
for the skill discovery locations.

Other skills:
[`btw_skill_install_github()`](https://posit-dev.github.io/btw/reference/btw_skill_install_github.md),
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/reference/btw_skill_install_package.md),
[`btw_skill_install_project()`](https://posit-dev.github.io/btw/reference/btw_skill_install_project.md),
[`btw_skill_prompt()`](https://posit-dev.github.io/btw/reference/btw_skill_prompt.md),
[`btw_tool_skill()`](https://posit-dev.github.io/btw/reference/btw_tool_skill.md)
