# use_btw_md() creates btw.md in project scope

    Code
      path <- use_btw_md("project")
    Message
      v Created 'btw.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_create_btw_md()` to use an LLM to help you initialize the project context.

# use_btw_md() creates btw.md in user scope

    Code
      path <- use_btw_md("user")
    Message
      v Created '~/.btw/btw.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_create_btw_md()` to use an LLM to help you initialize the project context.

# use_btw_md('user') uses an existing ~/.btw/btw.md

    Code
      path <- use_btw_md("user")
    Message
      v '~/.btw/btw.md' already exists
      i Call `btw::edit_btw_md("user")` to edit it

# use_btw_md('user') keeps a loose ~/btw.md when non-interactive

    Code
      path <- use_btw_md("user")
    Message
      ! Your user config is at '~/btw.md'.
      i The recommended location is now '~/.btw/btw.md'.
      i Move it there when convenient; btw reads '~/btw.md' for now.
      i Call `btw::edit_btw_md("user")` to edit it

# use_btw_md('user') migrates a loose ~/btw.md when confirmed

    Code
      path <- use_btw_md("user")
    Message
      ! Your user config is at '~/btw.md'.
      i The recommended location is now '~/.btw/btw.md'.
      Move it to the recommended location now?
      v Moved to '~/.btw/btw.md'.
      i Call `btw::edit_btw_md("user")` to edit it

# use_btw_md('user') migrates a config from a non-loose location

    Code
      path <- use_btw_md("user")
    Message
      ! Your user config is at '~/.config/btw/btw.md'.
      i The recommended location is now '~/.btw/btw.md'.
      Move it to the recommended location now?
      v Moved to '~/.btw/btw.md'.
      i Call `btw::edit_btw_md("user")` to edit it

# use_btw_md('user') does not clobber a shadowed ~/.btw/btw.md

    Code
      path <- use_btw_md("user")
    Message
      ! Your user config is at '~/btw.md'.
      i The recommended location is now '~/.btw/btw.md'.
      i A config also exists at '~/.btw/btw.md', shadowed by '~/btw.md'.
      i Call `btw::edit_btw_md("user")` to edit it

# use_btw_md() creates btw.md in sub-directory path

    Code
      path <- use_btw_md(subdir)
    Message
      v Created 'subdir/btw.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_create_btw_md()` to use an LLM to help you initialize the project context.

# use_btw_md() creates AGENTS.md with correct template

    Code
      path <- use_btw_md("AGENTS.md")
    Message
      v Created 'AGENTS.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_create_btw_md()` to use an LLM to help you initialize the project context.

# use_btw_md() does not overwrite existing file

    Code
      path <- use_btw_md("btw.md")
    Message
      v 'btw.md' already exists
      i Call `btw::edit_btw_md("btw.md")` to edit it

# edit_btw_md('user') prompts to pick among multiple configs

    Code
      path <- edit_btw_md("user")
    Message
      ! You have more than one user-level 'btw.md' config file.
      i `btw::btw_client()` and `btw::btw_app()` read the highest-priority one, marked below.
      i Consider consolidating your config into '~/.btw/btw.md'.
      v Opening '~/.btw/btw.md'

