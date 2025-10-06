# use_btw_md() creates btw.md in project scope

    Code
      path <- use_btw_md("project")
    Message
      v Created 'btw.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_btw_init()` to use an LLM to help you initialize the project context.

# use_btw_md() creates btw.md in user scope

    Code
      path <- use_btw_md("user")
    Message
      v Created '~/btw.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_btw_init()` to use an LLM to help you initialize the project context.

# use_btw_md() creates btw.md in sub-directory path

    Code
      path <- use_btw_md(subdir)
    Message
      v Created 'subdir/btw.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_btw_init()` to use an LLM to help you initialize the project context.

# use_btw_md() creates AGENTS.md with correct template

    Code
      path <- use_btw_md("AGENTS.md")
    Message
      v Created 'AGENTS.md'
      i See `?btw::btw_client()` for format details
      i See `?btw::btw_tools()` for available tools
      i Call `btw::btw_task_btw_init()` to use an LLM to help you initialize the project context.

# use_btw_md() does not overwrite existing file

    Code
      path <- use_btw_md("btw.md")
    Message
      v 'btw.md' already exists
      i Call `btw::edit_btw_md("btw.md")` to edit it

