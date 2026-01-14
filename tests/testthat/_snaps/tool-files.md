# btw_tool_files_list() works

    Code
      writeLines(btw_tool_files_list()@value)
    Output
      | path | type | size | modification_time |
      |------|------|------|-------------------|
      | test.R | file | 5 | MODIFIED TIME |
      | test.csv | file | 5 | MODIFIED TIME |

---

    Code
      btw_tool_files_list("/")
    Condition
      Error in `btw_tool_files_list()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

---

    Code
      btw_tool_files_list("../")
    Condition
      Error in `btw_tool_files_list()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

# btw_tool_files_read() works

    Code
      btw_tool_files_read("mtcars.rds")
    Condition
      Error in `btw_tool_files_read()`:
      ! Path 'mtcars.rds' is not a path to a text file.

---

    Code
      btw_tool_files_read("../mtcars.rds")
    Condition
      Error in `btw_tool_files_read()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

# btw_tool_files_write() works

    Code
      btw_tool_files_write("../test.txt", "content")
    Condition
      Error in `btw_tool_files_write()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

