# btw_tool_git_status()

    Code
      cli::cat_line(result@value)
    Output
      test.txt [new] -unstaged

---

    Code
      cli::cat_line(result_staged_empty@value)
    Output
      No changes to report

---

    Code
      cli::cat_line(result_staged@value)
    Output
      test.txt [new] +staged

---

    Code
      cli::cat_line(btw_tool_git_status()@value)
    Output
      test.txt [new] +staged

---

    Code
      cli::cat_line(btw_tool_git_status(staged = FALSE)@value)
    Output
      test.txt [modified] -unstaged

---

    Code
      cli::cat_line(btw_tool_git_status()@value)
    Output
      test.txt [modified] -unstaged

---

    Code
      cli::cat_line(btw_tool_git_status()@value)
    Output
      test.txt [modified] +staged

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
      message: Initial commit
      author: Test User <test@example.com>
      time: 2025-10-11 12:13:14
      n_files: 1
      commit: abcd123

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
      main [2025-10-11 12:13:14] 

# btw_tool_git_branch_create()

    Code
      cli::cat_line(result@value)
    Output
      Created branch `feature-branch` from `HEAD`.

# btw_tool_git_branch_checkout()

    Code
      cli::cat_line(result@value)
    Output
      Checked out branch 'feature'

# git tools work together

    Code
      cli::cat_line(diff_unstaged@value)
    Output
      ```diff
      diff --git a/file1.txt b/file1.txt
      index abcd123..abcd123 100644
      --- a/file1.txt
      +++ b/file1.txt
      @@ -1 +1,2 @@
       Initial content
      +Added line
      
      ```

---

    Code
      cli::cat_line(diff_staged@value)
    Output
      ```diff
      diff --git a/file1.txt b/file1.txt
      new file mode 100644
      index abcd123..abcd123
      --- /dev/null
      +++ b/file1.txt
      @@ -0,0 +1 @@
      +Initial content
      
      ```

---

    Code
      cli::cat_line(log_all@value)
    Output
      message: Update file1
      author: Test User <test@example.com>
      time: 2025-10-11 12:13:14
      n_files: 1
      commit: abcd123
      
      message: Add file1
      author: Test User <test@example.com>
      time: 2025-10-11 12:13:14
      n_files: 1
      commit: abcd123

---

    Code
      cli::cat_line(status_final@value)
    Output
      file3.txt [new] -unstaged

---

    Code
      cli::cat_line(log_final@value)
    Output
      message: Add file2
      author: Test User <test@example.com>
      time: 2025-10-11 12:13:14
      n_files: 1
      commit: abcd123
      
      message: Update file1
      author: Test User <test@example.com>
      time: 2025-10-11 12:13:14
      n_files: 1
      commit: abcd123
      
      message: Add file1
      author: Test User <test@example.com>
      time: 2025-10-11 12:13:14
      n_files: 1
      commit: abcd123

