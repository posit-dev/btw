# btw_tool_list_files() works

    Code
      writeLines(btw_tool_list_files())
    Output
      | path | type | size | modification_time |
      |------|------|------|-------------------|
      | test.R | file | 5 | MODIFIED TIME |
      | test.csv | file | 5 | MODIFIED TIME |

---

    Code
      btw_tool_list_files("/")
    Condition
      Error in `check_path_within_current_wd()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

---

    Code
      btw_tool_list_files("../")
    Condition
      Error in `check_path_within_current_wd()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

# btw_tool_read_text_file() works

    Code
      btw_tool_read_text_file("mtcars.rds")
    Condition
      Error in `btw_tool_read_text_file()`:
      ! Path 'mtcars.rds' is not a path to a text file.

---

    Code
      btw_tool_read_text_file("../mtcars.rds")
    Condition
      Error in `check_path_within_current_wd()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

