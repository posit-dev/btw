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

btw_test_color <- function(text, style, enabled) {
  if (!enabled) {
    return(text)
  }
  switch(
    style,
    muted = cli::col_grey(text),
    pass = cli::col_green(text),
    fail = cli::col_red(text),
    warn = cli::col_yellow(text)
  )
}

btw_compact_reporter <- function(pkg = ".", filter = NULL) {
  rlang::check_installed("R6")
  test_dir <- file.path(pkg, "tests", "testthat")
  files <- if (dir.exists(test_dir)) {
    testthat::find_test_scripts(test_dir, filter = filter, full.names = FALSE)
  } else {
    character()
  }
  width <- if (length(files)) {
    max(nchar(btw_test_name(files), type = "width"))
  } else {
    0L
  }

  R6::R6Class("BtwCompactReporter", inherit = testthat::Reporter, public = list(
    name = NULL,
    file_id = NULL,
    files = NULL,
    name_width = NULL,
    color = NULL,
    counts = NULL,
    failures = NULL,
    warnings = NULL,

    initialize = function(width) {
      super$initialize()
      self$capabilities$parallel_support <- TRUE
      self$capabilities$parallel_updates <- TRUE
      self$name_width <- width
      self$files <- list()
      self$color <- identical(self$out, stdout()) &&
        sink.number() == 0L && cli::num_ansi_colors() > 1L
      self$counts <- c(P = 0L, F = 0L, S = 0L, W = 0L)
      self$failures <- list()
      self$warnings <- list()
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
        btw_test_color(paste0(key, ":", counts[[key]]), style, self$color)
      }, ""), collapse = " ")
      styled_status <- btw_test_color(
        status, switch(status, "✓" = "pass", "✗" = "fail", "warn"), self$color
      )
      styled_time <- btw_test_color(btw_test_duration(elapsed), "muted", self$color)
      self$cat_line(sprintf(
        "%s %-*s  %s  %s",
        styled_status, max(self$name_width, nchar(self$name, type = "width")),
        self$name, styled_time, summary
      ))
      if (identical(self$out, stdout())) flush(stdout())
      self$files[[self$file_id]] <- NULL
    },
    end_reporter = function() {
      if (length(self$warnings)) {
        self$cat_line()
        self$cat_line(btw_test_color("======== WARNINGS ========", "warn", self$color))
        for (warning in self$warnings) {
          self$cat_line(
            btw_test_color("WARN", "warn", self$color), ": ",
            btw_test_location(warning)
          )
          self$cat_line(format(warning))
          self$cat_line()
        }
      }
      if (length(self$failures)) {
        self$cat_line()
        self$cat_line(btw_test_color("======== FAILURES ========", "fail", self$color))
        for (failure in self$failures) {
          self$cat_line(
            btw_test_color(toupper(btw_test_type(failure)), "fail", self$color),
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
  ))$new(width = width)
}
