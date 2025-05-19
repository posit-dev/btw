# btw_tool_ide_read_current_editor() has informative errors

    Code
      btw_tool_ide_read_current_editor()
    Condition
      Error in `btw_tool_ide_read_current_editor()`:
      ! Please ask the user for consent before reading from the editor.

---

    Code
      btw_tool_ide_read_current_editor(consent = TRUE)
    Condition
      Error in `btw_tool_ide_read_current_editor()`:
      ! @current_file only works in an IDE where the rstudioapi is available.

# @current_file

    Code
      cli::cat_line(btw("@current_file"))
    Output
      ## Context
      
      FILE: `fixtures/test.R`
      ```R
      library(dplyr)
      
      mtcars %>%
        group_by(am) %>%
        summarize(average_mpg = mean(mpg))
      
      ```

# @current_selection

    Code
      cli::cat_line(btw("@current_selection"))
    Output
      ## Context
      
      FILE: fixtures/test.scss:L1C1-L1C21
      ```scss
      .one { color: red; }
      ```
      
      FILE: fixtures/test.scss:L3C1-L3C5
      ```scss
      .two
      ```
      
      FILE: fixtures/test.scss:L5C10-L5C23
      ```scss
      color: green;
      ```

