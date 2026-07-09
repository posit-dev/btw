# edit response: multiple 1:1 replaces merged, no shift hint

    Code
      writeLines(edit_result@value)
    Output
      Applied 2 edit(s) to test.txt (5 lines).
      
      1:1bf|AAA
      2:9bf|bbb
      3:70f|CCC
      4:37b|ddd

# edit response: insert adds shift hint

    Code
      writeLines(val)
    Output
      Applied 1 edit(s) to test.txt (now 7 lines, previously 5).
      
      2:9bf|bbb
      3:6fa|new1
      4:0b0|new2
      5:675|ccc
      
      Content below line 5 was not modified.
      Cached hashes are still valid — update line numbers by +2 (old line 3 → new line 5).

# edit response: delete adds negative shift hint

    Code
      writeLines(val)
    Output
      Applied 1 edit(s) to test.txt (now 3 lines, previously 5).
      
      1:40a|aaa
      2:37b|ddd
      
      Content below line 2 was not modified.
      Cached hashes are still valid — update line numbers by -2 (old line 4 → new line 2).

# edit response: nearby edits merge into single region

    Code
      writeLines(val)
    Output
      Applied 2 edit(s) to test.txt (30 lines).
      
      4:17c|line04
      5:566|FIVE
      6:948|line06
      7:c30|line07
      8:729|line08
      9:f13|line09
      10:848|TEN
      11:de6|line11

# edit response: distant edits produce multiple regions

    Code
      writeLines(val)
    Output
      Applied 2 edit(s) to test.txt (now 51 lines, previously 50).
      
      2:2f9|line02
      3:f47|THREE-A
      4:52e|THREE-B
      5:17c|line04
      
      Content between here and the next edit region was not modified.
      Cached hashes are still valid — update line numbers by +1 (old line 4 → new line 5).
      
      40:ebd|line39
      41:51b|FORTY
      42:bb7|line41
      
      Content below line 42 was not modified.
      Cached hashes are still valid — update line numbers by +1 (old line 41 → new line 42).

# edit response: distant edits with cumulative deltas

    Code
      writeLines(val)
    Output
      Applied 2 edit(s) to test.txt (now 53 lines, previously 50).
      
      2:2f9|line02
      3:f47|THREE-A
      4:52e|THREE-B
      5:17c|line04
      
      Content between here and the next edit region was not modified.
      Cached hashes are still valid — update line numbers by +1 (old line 4 → new line 5).
      
      40:ebd|line39
      41:569|FORTY-A
      42:77b|FORTY-B
      43:f05|FORTY-C
      44:bb7|line41
      
      Content below line 44 was not modified.
      Cached hashes are still valid — update line numbers by +3 (old line 41 → new line 44).

# edit response: insert at top of file

    Code
      writeLines(val)
    Output
      Applied 1 edit(s) to test.txt (now 5 lines, previously 3).
      
      1:53f|header1
      2:294|header2
      3:40a|aaa
      
      Content below line 3 was not modified.
      Cached hashes are still valid — update line numbers by +2 (old line 1 → new line 3).

