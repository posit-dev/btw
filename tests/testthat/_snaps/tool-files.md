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
      ! Path 'mtcars.rds' appears to be a binary file or cannot be read as text.

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

# edit response: multiple 1:1 replaces merged, no shift hint

    Code
      writeLines(edit_result@value)
    Output
      Applied 2 edit(s) to test.txt (5 lines).
      
      1:262|AAA
      2:1ba|bbb
      3:62d|CCC
      4:f8c|ddd

# edit response: insert adds shift hint

    Code
      writeLines(val)
    Output
      Applied 1 edit(s) to test.txt (now 7 lines, previously 5).
      
      2:1ba|bbb
      3:426|new1
      4:1b3|new2
      5:73c|ccc
      
      Content below line 5 was not modified.
      Cached hashes are still valid — update line numbers by +2 (old line 3 → new line 5).

# edit response: delete adds negative shift hint

    Code
      writeLines(val)
    Output
      Applied 1 edit(s) to test.txt (now 3 lines, previously 5).
      
      1:066|aaa
      2:f8c|ddd
      
      Content below line 2 was not modified.
      Cached hashes are still valid — update line numbers by -2 (old line 4 → new line 2).

# edit response: nearby edits merge into single region

    Code
      writeLines(val)
    Output
      Applied 2 edit(s) to test.txt (30 lines).
      
      4:6cc|line04
      5:493|FIVE
      6:a37|line06
      7:735|line07
      8:4b8|line08
      9:2af|line09
      10:1b4|TEN
      11:f00|line11

# edit response: distant edits produce multiple regions

    Code
      writeLines(val)
    Output
      Applied 2 edit(s) to test.txt (now 51 lines, previously 50).
      
      2:dec|line02
      3:bf2|THREE-A
      4:0ac|THREE-B
      5:6cc|line04
      
      Content between here and the next edit region was not modified.
      Cached hashes are still valid — update line numbers by +1 (old line 4 → new line 5).
      
      40:3d9|line39
      41:958|FORTY
      42:778|line41
      
      Content below line 42 was not modified.
      Cached hashes are still valid — update line numbers by +1 (old line 41 → new line 42).

# edit response: distant edits with cumulative deltas

    Code
      writeLines(val)
    Output
      Applied 2 edit(s) to test.txt (now 53 lines, previously 50).
      
      2:dec|line02
      3:bf2|THREE-A
      4:0ac|THREE-B
      5:6cc|line04
      
      Content between here and the next edit region was not modified.
      Cached hashes are still valid — update line numbers by +1 (old line 4 → new line 5).
      
      40:3d9|line39
      41:582|FORTY-A
      42:e77|FORTY-B
      43:136|FORTY-C
      44:778|line41
      
      Content below line 44 was not modified.
      Cached hashes are still valid — update line numbers by +3 (old line 41 → new line 44).

# edit response: insert at top of file

    Code
      writeLines(val)
    Output
      Applied 1 edit(s) to test.txt (now 5 lines, previously 3).
      
      1:ae1|header1
      2:c40|header2
      3:066|aaa
      
      Content below line 3 was not modified.
      Cached hashes are still valid — update line numbers by +2 (old line 1 → new line 3).

