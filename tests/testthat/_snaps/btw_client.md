# btw_client() works with `btw.chat_client` option

    Code
      print(chat)
    Output
      <Chat turns=1 tokens=0/0>
      -- system ----------------------------------------------------------------------
      # User System
      
      The user is running R VERSION on a OPERATING SYSTEM system 
      (SYSTEM VERSION). The user interface being used is RStudio. The system's 
      language setting is es, with locale collation rules set to 'es_ES.UTF-8' and 
      character encoding set to 'es_ES.UTF-8'. The system is configured to use the 
      'Europe/Madrid' timezone. The current date is DAY OF WEEK, MONTH DAY, YEAR 
      (YYYY-MM-DD). When processing date-related queries or timezone-specific 
      information, please account for these system settings.
      
      # Tools
      
      You have access to tools that help you interact with the user's R session and 
      workspace. Use these tools when they are helpful and appropriate to complete 
      the user's request. These tools are available to augment your ability to help 
      the user, but you are smart and capable and can answer many things on your own.
      It is okay to answer the user without relying on these tools.
      
      ---
      
      I like to have my own system prompt.

# btw_client() adds `.btw` context file to system prompt

    Code
      print(chat)
    Output
      <Chat turns=1 tokens=0/0>
      -- system ----------------------------------------------------------------------
      # User System
      
      The user is running R VERSION on a OPERATING SYSTEM system 
      (SYSTEM VERSION). The user interface being used is RStudio. The system's 
      language setting is es, with locale collation rules set to 'es_ES.UTF-8' and 
      character encoding set to 'es_ES.UTF-8'. The system is configured to use the 
      'Europe/Madrid' timezone. The current date is DAY OF WEEK, MONTH DAY, YEAR 
      (YYYY-MM-DD). When processing date-related queries or timezone-specific 
      information, please account for these system settings.
      
      # Tools
      
      You have access to tools that help you interact with the user's R session and 
      workspace. Use these tools when they are helpful and appropriate to complete 
      the user's request. These tools are available to augment your ability to help 
      the user, but you are smart and capable and can answer many things on your own.
      It is okay to answer the user without relying on these tools.
      
      # Project Context
      
      * Prefer solutions that use {tidyverse}
      * Always use `=` for assignment
      * Always use the native base-R pipe `|>` for piped expressions
      
      ---
      
      I like to have my own system prompt.

# btw_client() uses `.btw` context file for client settings

    Code
      print(chat)
    Output
      <Chat turns=1 tokens=0/0>
      -- system ----------------------------------------------------------------------
      # User System
      
      The user is running R VERSION on a OPERATING SYSTEM system 
      (SYSTEM VERSION). The user interface being used is RStudio. The system's 
      language setting is es, with locale collation rules set to 'es_ES.UTF-8' and 
      character encoding set to 'es_ES.UTF-8'. The system is configured to use the 
      'Europe/Madrid' timezone. The current date is DAY OF WEEK, MONTH DAY, YEAR 
      (YYYY-MM-DD). When processing date-related queries or timezone-specific 
      information, please account for these system settings.
      
      # Tools
      
      You have access to tools that help you interact with the user's R session and 
      workspace. Use these tools when they are helpful and appropriate to complete 
      the user's request. These tools are available to augment your ability to help 
      the user, but you are smart and capable and can answer many things on your own.
      It is okay to answer the user without relying on these tools.
      
      # Project Context
      
      * Prefer solutions that use {tidyverse}
      * Always use `=` for assignment
      * Always use the native base-R pipe `|>` for piped expressions
      
      ---
      
      I like to have my own system prompt

