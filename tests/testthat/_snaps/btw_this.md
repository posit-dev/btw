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

