# btw_this.function()

    Code
      cli::cat_line(btw_this(dplyr::mutate))
    Output
      ```r
      function (.data, ...) 
      {
          UseMethod("mutate")
      }
      ```

# btw_this('@last_error')

    Code
      cat(btw_this("@last_error"))
    Output
      <error/rlang_error> Error: ! That didn't work.

# @issue detects current repo when only number provided

    Code
      cli::cat_line(result)
    Output
      <github-issue owner="posit-dev" repo="btw" number="65">
      <metadata>
      title: Test Issue
      url: https://github.com/posit-dev/btw/issues/65
      type: Issue
      state: open
      author: testuser
      created: 2025-01-01T00:00:00Z
      updated: 2025-01-01T00:00:00Z
      </metadata>
      <body>
      Test body
      </body>
      </github-issue>

# @pr marks pull requests correctly

    Code
      cli::cat_line(result)
    Output
      <github-pull-request owner="posit-dev" repo="btw" number="64">
      <metadata>
      title: Test PR
      url: https://github.com/posit-dev/btw/pull/64
      type: Pull Request
      state: closed
      author: testuser
      created: 2025-01-01T00:00:00Z
      updated: 2025-01-01T00:00:00Z
      closed: 2025-01-02T00:00:00Z
      merged: 2025-01-02T00:00:00Z
      labels: bug, urgent
      milestone: v1.0
      </metadata>
      <body>
      Test PR body
      </body>
      </github-pull-request>

