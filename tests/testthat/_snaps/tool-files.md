# btw_tool_files_list_files() works

    Code
      writeLines(btw_tool_files_list_files()@value)
    Condition
      Warning:
      `btw_tool_files_list_files()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_files_list()` instead.
    Output
      | path | type | size | modification_time |
      |------|------|------|-------------------|
      | test.R | file | 5 | MODIFIED TIME |
      | test.csv | file | 5 | MODIFIED TIME |

---

    Code
      btw_tool_files_list_files("/")
    Condition
      Warning:
      `btw_tool_files_list_files()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_files_list()` instead.
      Error in `btw_tool_files_list_files()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

---

    Code
      btw_tool_files_list_files("../")
    Condition
      Warning:
      `btw_tool_files_list_files()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_files_list()` instead.
      Error in `btw_tool_files_list_files()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

# btw_tool_files_read_text_file() works

    Code
      btw_tool_files_read_text_file("mtcars.rds")
    Condition
      Warning:
      `btw_tool_files_read_text_file()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_files_read()` instead.
      Error in `btw_tool_files_read_text_file()`:
      ! Path 'mtcars.rds' is not a path to a text file.

---

    Code
      btw_tool_files_read_text_file("../mtcars.rds")
    Condition
      Warning:
      `btw_tool_files_read_text_file()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_files_read()` instead.
      Error in `btw_tool_files_read_text_file()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

# btw_tool_files_write_text_file() works

    Code
      btw_tool_files_write_text_file("../test.txt", "content")
    Condition
      Warning:
      `btw_tool_files_write_text_file()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_files_write()` instead.
      Error in `btw_tool_files_write_impl()`:
      ! You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.

