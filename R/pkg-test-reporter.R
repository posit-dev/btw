# Compact testthat reporter used by btw pkg test and btw_tool_pkg_test().

btw_test_name <- function(file) {
  sub("\\.[rR]$", "", sub("^test-", "", basename(file)))
}

btw_test_duration <- function(seconds) {
  decimals <- if (seconds < 10) 2L else if (seconds < 100) 1L else 0L
  if (round(seconds, decimals) >= 10 && decimals == 2L) {
    decimals <- 1L
  }
  if (round(seconds, decimals) >= 100 && decimals == 1L) {
    decimals <- 0L
  }
  sprintf(paste0("%.", decimals, "fs"), seconds)
}

btw_test_location <- function(result) {
  ref <- result$srcref
  if (!inherits(ref, "srcref")) {
    return("")
  }
  filename <- attr(ref, "srcfile")$filename
  if (is.null(filename)) {
    return("")
  }
  sprintf("%s:%s:%s", filename, ref[[1]], ref[[2]])
}

btw_test_type <- function(result) {
  sub("^expectation_", "", class(result)[[1]])
}

btw_compact_reporter <- function() {
  rlang::check_installed("R6")

  R6::R6Class("BtwCompactReporter", inherit = testthat::Reporter, public = list(
    name = NULL,
    file_id = NULL,
    files = NULL,
    color = NULL,
    counts = NULL,
    failures = NULL,
    warnings = NULL,

    initialize = function() {
      super$initialize()
      self$capabilities$parallel_support <- TRUE
      self$capabilities$parallel_updates <- TRUE
      self$files <- list()
      self$color <- identical(self$out, stdout()) &&
        sink.number() == 0L && cli::num_ansi_colors() > 1L
      self$counts <- c(P = 0L, F = 0L, S = 0L, W = 0L)
      self$failures <- list()
      self$warnings <- list()
    },
    colorize = function(text, style) {
      if (!self$color) {
        return(text)
      }
      switch(
        style,
        muted = cli::col_grey(text),
        pass = cli::col_green(text),
        fail = cli::col_red(text),
        warn = cli::col_yellow(text)
      )
    },
    start_file = function(name) {
      self$file_id <- name
      self$name <- btw_test_name(name)
      # In testthat's parallel mode, start_file() is repeated for every
      # event. Keep each running file's timer and counts across events.
      if (!is.null(self$files[[self$file_id]])) {
        return(invisible(NULL))
      }
      self$files[[self$file_id]] <- list(
        started = proc.time()[[3L]],
        counts = c(P = 0L, F = 0L, S = 0L, W = 0L)
      )
      self$cat_line(self$colorize(paste0("@ ", self$name), "muted"))
      if (identical(self$out, stdout())) flush(stdout())
    },
    add_result = function(context, test, result) {
      type <- btw_test_type(result)
      key <- if (type %in% c("failure", "error")) {
        self$failures <- c(self$failures, list(result))
        "F"
      } else if (type == "skip") {
        "S"
      } else if (type == "warning") {
        self$warnings <- c(self$warnings, list(result))
        "W"
      } else {
        "P"
      }
      file <- self$files[[self$file_id]]
      file$counts[[key]] <- file$counts[[key]] + 1L
      self$files[[self$file_id]] <- file
      self$counts[[key]] <- self$counts[[key]] + 1L
    },
    end_file = function() {
      file <- self$files[[self$file_id]]
      if (is.null(file)) {
        return(invisible(NULL))
      }
      elapsed <- proc.time()[[3L]] - file$started
      counts <- file$counts
      status <- if (counts[["F"]] > 0L) {
        "✗"
      } else if (counts[["W"]] > 0L) {
        "!"
      } else {
        "✓"
      }
      counts <- counts[counts > 0L]
      if (!length(counts)) {
        counts <- c(P = 0L)
      }
      summary <- paste(vapply(names(counts), function(key) {
        style <- switch(key, P = "pass", F = "fail", S = "muted", W = "warn")
        self$colorize(paste0(key, ":", counts[[key]]), style)
      }, ""), collapse = " ")
      styled_status <- self$colorize(
        status, switch(status, "✓" = "pass", "✗" = "fail", "warn")
      )
      styled_time <- self$colorize(btw_test_duration(elapsed), "muted")
      self$cat_line(sprintf(
        "%s %s  %s  %s",
        styled_status, self$name, styled_time, summary
      ))
      if (identical(self$out, stdout())) flush(stdout())
      self$files[[self$file_id]] <- NULL
    },
    end_reporter = function() {
      if (length(self$warnings)) {
        self$cat_line()
        self$cat_line(self$colorize("======== WARNINGS ========", "warn"))
        for (warning in self$warnings) {
          self$cat_line(
            self$colorize("WARN", "warn"), ": ",
            btw_test_location(warning)
          )
          self$cat_line(format(warning))
          self$cat_line()
        }
      }
      if (length(self$failures)) {
        self$cat_line()
        self$cat_line(self$colorize("======== FAILURES ========", "fail"))
        for (failure in self$failures) {
          self$cat_line(
            self$colorize(toupper(btw_test_type(failure)), "fail"),
            ": ", btw_test_location(failure)
          )
          self$cat_line(format(failure))
          self$cat_line()
        }
      }
      self$cat_line(sprintf(
        "[ FAIL %d | WARN %d | SKIP %d | PASS %d ]",
        self$counts[["F"]], self$counts[["W"]], self$counts[["S"]], self$counts[["P"]]
      ))
      if (identical(self$out, stdout())) flush(stdout())
    }
  ))$new()
}
