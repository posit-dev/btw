# btw_client() works with `btw.client` option

    Code
      print(chat)
    Output
      <Chat Anthropic/claude-sonnet-4-20250514 turns=1 tokens=0/0 $0.00>
      -- system [0] ------------------------------------------------------------------
      # System and Session Context
      Please account for the following R session and system settings in all responses.
      
      <system_info>
      R_VERSION: R VERSION
      OS: OPERATING SYSTEM
      SYSTEM: SYSTEM VERSION
      UI: RStudio
      LANGUAGE: es
      LOCALE: C
      ENCODING: LC_CTYPE
      TIMEZONE: Europe/Madrid
      DATE: DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)
      </system_info>
      
      # Tools
      
      You have access to tools that help you interact with the user's R session and workspace. Use these tools when they are helpful and appropriate to complete the user's request. These tools are available to augment your ability to help the user, but you are smart and capable and can answer many things on your own. It is okay to answer the user without relying on these tools.
      
      ---
      
      I like to have my own system prompt.

# btw_client() adds `btw.md` context file to system prompt

    Code
      print(chat)
    Output
      <Chat Anthropic/claude-sonnet-4-20250514 turns=1 tokens=0/0 $0.00>
      -- system [0] ------------------------------------------------------------------
      # System and Session Context
      Please account for the following R session and system settings in all responses.
      
      <system_info>
      R_VERSION: R VERSION
      OS: OPERATING SYSTEM
      SYSTEM: SYSTEM VERSION
      UI: RStudio
      LANGUAGE: es
      LOCALE: C
      ENCODING: LC_CTYPE
      TIMEZONE: Europe/Madrid
      DATE: DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)
      </system_info>
      
      # Tools
      
      You have access to tools that help you interact with the user's R session and workspace. Use these tools when they are helpful and appropriate to complete the user's request. These tools are available to augment your ability to help the user, but you are smart and capable and can answer many things on your own. It is okay to answer the user without relying on these tools.
      
      # Project Context
      
      * Prefer solutions that use {tidyverse}
      * Always use `=` for assignment
      * Always use the native base-R pipe `|>` for piped expressions
      
      ---
      
      I like to have my own system prompt.

# btw_client() uses `btw.md` context file for client settings

    Code
      print(chat)
    Output
      <Chat OpenAI/gpt-4o turns=1 tokens=0/0 $0.00>
      -- system [0] ------------------------------------------------------------------
      # System and Session Context
      Please account for the following R session and system settings in all responses.
      
      <system_info>
      R_VERSION: R VERSION
      OS: OPERATING SYSTEM
      SYSTEM: SYSTEM VERSION
      UI: RStudio
      LANGUAGE: es
      LOCALE: C
      ENCODING: LC_CTYPE
      TIMEZONE: Europe/Madrid
      DATE: DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)
      </system_info>
      
      # Tools
      
      You have access to tools that help you interact with the user's R session and workspace. Use these tools when they are helpful and appropriate to complete the user's request. These tools are available to augment your ability to help the user, but you are smart and capable and can answer many things on your own. It is okay to answer the user without relying on these tools.
      
      # Project Context
      
      * Prefer solutions that use {tidyverse}
      * Always use `=` for assignment
      * Always use the native base-R pipe `|>` for piped expressions
      
      ---
      
      I like to have my own system prompt

