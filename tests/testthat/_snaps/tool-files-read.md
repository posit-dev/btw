# btw_tool_files_read() works

    Code
      btw_tool_files_read("mtcars.rds")
    Condition
      Error in `btw_tool_files_read()`:
      ! Path 'mtcars.rds' appears to be a binary file or cannot be read as text.

---

    Code
      btw_tool_files_read("../mtcars.rds")
    Condition
      Error in `btw_tool_files_read()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

