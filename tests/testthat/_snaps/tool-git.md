# btw_tool_git_status()

    Code
      cli::cat_line(result@value)
    Output
      | file | status | staged |
      |------|--------|--------|
      | test.txt | new | FALSE |

---

    Code
      cli::cat_line(result_staged_empty@value)
    Output
      No changes to report

---

    Code
      cli::cat_line(result_staged@value)
    Output
      | file | status | staged |
      |------|--------|--------|
      | test.txt | new | TRUE |

# btw_tool_git_diff()

    Code
      cli::cat_line(result@value)
    Output
      ```diff
      diff --git a/test.txt b/test.txt
      index abcd123..abcd123 100644
      --- a/test.txt
      +++ b/test.txt
      @@ -1 +1,2 @@
       line 1
      +line 2
      
      ```

# btw_tool_git_log()

    Code
      cli::cat_line(result@value)
    Output
      | commit | author | time | message |
      |--------|--------|------|---------|
      | abcd123 | Test User <test@example.com> | 2025-10-11 12:13:14 | Initial commit |

# btw_tool_git_commit()

    Code
      cli::cat_line(result@value)
    Output
      Created commit: abcd123
      Message: Add test file

# btw_tool_git_branch_list()

    Code
      cli::cat_line(result@value)
    Output
      | name | ref | upstream | updated |
      |------|-----|----------|---------|
      | main | refs/heads/main | NA | 2025-10-11 12:13:14 |

# btw_tool_git_branch_create()

    Code
      cli::cat_line(result@value)
    Output
      Created branch 'feature-branch' from 'HEAD'

# btw_tool_git_branch_checkout()

    Code
      cli::cat_line(result@value)
    Output
      Checked out branch 'feature'

