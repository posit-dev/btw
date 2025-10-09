#' Execute code with automatic retry on failure
#'
#' @param code Code to execute
#' @param times Number of times to retry (default: 3)
#' @param verbose Whether to print retry messages (default: TRUE)
#'
#' @noRd
with_retry <- function(
  code,
  times = 3,
  verbose = TRUE,
  .envir = parent.frame(),
  .desc = NULL
) {
  code <- substitute(code)
  attempt <- 0
  last_error <- NULL
  times <- max(1, times)

  test_that_env <- get_test_that_env()
  if (!is.null(test_that_env)) {
    .desc <- get0("frame", test_that_env)$desc
  } else if (is.null(.envir)) {
    .envir <- parent.frame()
  }

  .desc <- .desc %||% "code with retry"

  for (i in seq_len(times)) {
    attempt <- i

    if (verbose && attempt > 1) {
      cli::cli_inform("\u00a0\u00a0[Retry {attempt}/{times}] { .desc}")
    }

    if (attempt == times) {
      # Last attempt, run without catching errors
      return(eval(code, envir = .envir))
    }

    # Try to evaluate the code, catching both errors and expectation failures
    res <- tryCatch(
      {
        eval(code, envir = .envir)
        list(success = TRUE, error = NULL)
      },
      skip = function(e) {
        # Skips should propagate immediately
        rlang::cnd_signal(e)
      },
      expectation_failure = function(e) {
        list(success = FALSE, error = e)
      },
      error = function(e) {
        list(success = FALSE, error = e)
      }
    )

    if (res$success) {
      return(invisible())
    }
  }
}

get_test_that_env <- function() {
  frames <- sys.frames()
  calls <- sys.calls()
  n <- length(calls)

  for (i in rev(seq_len(n))) {
    call <- calls[[i]]
    if (!is.call(call)) {
      next
    }

    fn <- call[[1L]]
    if (!is_test_that_call(fn)) {
      next
    }

    # Return the frame “inside” test_that() where its arguments are bound
    inner_idx <- if (i < n) i + 1L else i
    return(frames[[inner_idx]])
  }

  NULL
}

is_test_that_call <- function(fn) {
  # Direct call: test_that(...)
  if (is.name(fn) && identical(as.character(fn), "test_that")) {
    return(TRUE)
  }

  # Namespaced call: testthat::test_that(...) or testthat:::test_that(...)
  if (
    is.call(fn) &&
      length(fn) == 3L &&
      as.character(fn[[1L]]) %in% c("::", ":::") &&
      identical(as.character(fn[[3L]]), "test_that")
  ) {
    return(TRUE)
  }

  FALSE
}
