#' Run a testthat test with automatic retry on failure
#'
#' @param desc Test description (same as test_that)
#' @param code Test code to execute
#' @param times Number of times to retry (default: 3)
#' @param verbose Whether to print retry messages (default: TRUE)
#'
#' @noRd
test_that_with_retry <- function(desc, code, times = 3, verbose = TRUE) {
  code <- substitute(code)
  attempt <- 0
  last_error <- NULL
  caller_env <- parent.frame()

  testthat::test_that(desc, {
    for (i in 1:times) {
      attempt <<- attempt + 1

      if (verbose && attempt > 1) {
        cli::cli_inform("\u00a0\u00a0[Retry {attempt - 1}/{times - 1}] {desc}")
      }

      # Try to evaluate the code, catching both errors and expectation failures
      result <- tryCatch(
        {
          eval(code, envir = caller_env)
          list(success = TRUE, error = NULL)
        },
        skip = function(e) {
          # Skips are successes
          list(success = TRUE, error = NULL)
        },
        expectation_failure = function(e) {
          list(success = FALSE, error = e)
        },
        error = function(e) {
          list(success = FALSE, error = e)
        }
      )

      # If successful, we're done
      if (result$success) {
        if (verbose && attempt > 1) {
          cli::cli_inform(
            "\u00a0\u00a0[{cli::col_green('Success')}] {desc} (passed on attempt {attempt})"
          )
        }
        return(invisible(NULL))
      }

      # Store the error for potential re-throw
      last_error <<- result$error

      # If this is the last attempt, re-throw the error
      if (i == times) {
        if (verbose) {
          cli::cli_inform(
            "\u00a0\u00a0[{cli::col_red('Failed')}] {desc} (failed after {attempt} attempt{?s})"
          )
        }
        rlang::cnd_signal(last_error)
      }
    }
  })
}
