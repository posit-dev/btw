#' Tool: Run R code
#'
#' This tool runs R code and returns results as ellmer Content objects.
#' It captures text output, plots, messages, warnings, and errors.
#' Code execution stops on the first error, returning all results up to that point.
#'
#' @param code A character string containing R code to run.
#' @param _intent Intent description (automatically added by ellmer).
#'
#' @returns A list of ellmer Content objects:
#'   - `ContentText`: visible return values and text output
#'   - `ContentMessage`: messages from `message()`
#'   - `ContentWarning`: warnings from `warning()`
#'   - `ContentError`: errors from `stop()`
#'   - `ContentImageInline`: plots created during execution
#'
#' @examples
#' \dontrun{
#' # Simple calculation
#' btw_tool_run_r("2 + 2")
#'
#' # Code with plot
#' btw_tool_run_r("hist(rnorm(100))")
#'
#' # Code with warning
#' btw_tool_run_r("mean(c(1, 2, NA))")
#' }
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @export
btw_tool_run_r <- function(code, `_intent`) {}

btw_tool_run_r_impl <- function(code, .envir = global_env()) {
  check_string(code)
  check_installed("evaluate", "to run R code.")

  last_value <- NULL # Store the last value for potential use
  had_error <- FALSE # Track if an error occurred

  # Content results from evaluating the R code
  contents <- list()
  append_content <- function(x) contents <<- c(contents, list(x))

  last_plot <- NULL
  append_last_plot <- function() {
    if (is.null(last_plot)) {
      return()
    }

    path_plot <- withr::local_tempfile(fileext = ".png")
    run_r_plot_device(filename = path_plot, width = 768, height = 768)
    tryCatch(
      grDevices::replayPlot(last_plot),
      finally = {
        grDevices::dev.off()
      }
    )

    append_content(ellmer::content_image_file(path_plot, resize = "none"))
    last_plot <<- NULL
  }

  local_reproducible_output(disable_ansi_features = !is_installed("fansi"))

  # Create output handler that converts to Content types as outputs are generated
  handler <- evaluate::new_output_handler(
    source = function(src, expr) {
      # Skip source code echoing by returning NULL
      NULL
    },
    text = function(text) {
      append_last_plot()
      # Text output (from print, cat, etc.)
      append_content(ContentCode(text = text))
      text
    },
    graphics = function(plot) {
      if (!is.null(last_plot)) {
        if (!last_plot %is_plot_prefix_of% plot) {
          # New plot is not an extension of the last plot, so add the last plot
          append_last_plot()
        }
      }

      last_plot <<- plot
      plot
    },
    message = function(msg) {
      append_last_plot()
      msg_text <- conditionMessage(msg)
      # Remove trailing newline that message() adds
      msg_text <- sub("\n$", "", msg_text)
      append_content(ContentMessage(text = msg_text))
      msg
    },
    warning = function(warn) {
      append_last_plot()
      append_content(ContentWarning(conditionMessage(warn)))
      warn
    },
    error = function(err) {
      append_last_plot()
      had_error <<- TRUE
      append_content(ContentError(conditionMessage(err)))
      err
    },
    value = function(value, visible) {
      # Store the actual value when it's visible (meaningful output)
      # Invisible values include assignments and side-effect returns
      if (visible) {
        last_value <<- value
        # Also add as code content
        value_text <- paste(
          utils::capture.output(print(value)),
          collapse = "\n"
        )
        append_content(ContentCode(text = value_text))
      }

      if (visible) value
    }
  )

  # Evaluate the R code, collecting results along the way
  evaluate::evaluate(
    code,
    envir = .envir,
    stop_on_error = 1,
    new_device = TRUE,
    output_handler = handler
  )

  # Ensure last plot is added if not caught by other handlers
  append_last_plot()

  # Merge adjacent content of the same type
  contents <- merge_adjacent_content(contents)

  # For `value`, remove all ANSI codes
  value <- map(contents, run_r_content_handle_ansi)

  BtwRunToolResult(
    value = value,
    extra = list(
      data = last_value,
      code = code,
      contents = contents,
      # We always return contents up to the error as `value` because `error`
      # cannot handle rich output. We'll show status separately in the UI.
      status = if (had_error) "error" else "success"
    )
  )
}

`%is_plot_prefix_of%` <- function(x, y) {
  # See https://github.com/r-lib/evaluate/blob/20333c/R/graphics.R#L87-L88

  stopifnot(inherits(x, "recordedplot"))
  stopifnot(inherits(y, "recordedplot"))

  x <- x[[1]]
  y <- y[[1]]

  if (length(x) > length(y)) {
    return(FALSE)
  }

  identical(x[], y[seq_along(x)])
}

run_r_plot_device <- function(...) {
  dev_fn <- getOption("btw.run_r.graphics_device", default = NULL)
  if (!is.null(dev_fn)) {
    check_function(dev_fn)
    return(dev_fn(...))
  }

  if (rlang::is_installed("ragg")) {
    return(ragg::agg_png(...))
  }

  grDevices::png(...)
}

btw_can_register_run_r_tool <- function() {
  rlang::is_installed("evaluate")
}

run_r_content_handle_ansi <- function(x, plain = TRUE) {
  if (!S7::S7_inherits(x, ellmer::ContentText)) {
    return(x)
  }

  text <-
    if (isTRUE(plain)) {
      htmltools::htmlEscape(strip_ansi(x@text))
    } else {
      fansi_to_html(x@text)
    }

  S7::set_props(x, text = text)
}

#' Convert ANSI text to HTML with btw CSS classes
#'
#' Wrapper around fansi::to_html() that uses btw's CSS classes for ANSI colors.
#' Supports all 16 ANSI colors (basic + bright) with Bootstrap 5 theme integration.
#'
#' @param text Character string with ANSI escape codes
#' @returns Character string with HTML span elements using btw ANSI CSS classes
#' @noRd
fansi_to_html <- function(text) {
  # Define 32 class names for all ANSI 16 colors (foreground + background).
  # Order must alternate fg/bg for each color: black, red, green, yellow, blue,
  # magenta, cyan, white, then bright versions of each

  # Color names for basic (0-7) and bright (8-15) colors
  colors_basic <- c(
    "black",
    "red",
    "green",
    "yellow",
    "blue",
    "magenta",
    "cyan",
    "white"
  )
  colors_bright <- paste("bright", colors_basic, sep = "-")
  colors_all <- c(colors_basic, colors_bright)

  # Generate class names: for each color, create fg and bg class
  classes_32 <- paste0(
    "btw-ansi-",
    c(rbind(paste0("fg-", colors_all), paste0("bg-", colors_all)))
  )

  fansi::to_html(fansi::html_esc(text), classes = classes_32)
}

.btw_add_to_tools(
  name = "btw_tool_run_r",
  group = "run",
  tool = function() {
    ellmer::tool(
      function(code) {
        btw_tool_run_r_impl(code)
      },
      name = "btw_tool_run_r",
      description = "Run R code and return results as Content objects. Captures text output, plots, messages, warnings, and errors. Stops on first error.",
      annotations = ellmer::tool_annotations(
        title = "Run R Code",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        btw_can_register = btw_can_register_run_r_tool
      ),
      arguments = list(
        code = ellmer::type_string("R code to run as a string.")
      )
    )
  }
)

# ---- Content Types ----
ContentCode <- S7::new_class(
  "ContentCode",
  parent = ellmer::ContentText
)

ContentMessage <- S7::new_class(
  "ContentMessage",
  parent = ellmer::ContentText
)

ContentWarning <- S7::new_class(
  "ContentWarning",
  parent = ellmer::ContentText
)

ContentError <- S7::new_class(
  "ContentError",
  parent = ellmer::ContentText
)

BtwRunToolResult <- S7::new_class(
  "BtwRunToolResult",
  parent = ellmer::ContentToolResult
)

contents_html <- S7::new_external_generic(
  package = "ellmer",
  name = "contents_html",
  dispatch_args = "content"
)

S7::method(contents_html, ContentCode) <- function(content, ...) {
  sprintf(
    '<pre><code class="nohighlight">%s</code></pre>',
    trimws(content@text)
  )
}

S7::method(contents_html, ContentMessage) <- function(content, ...) {
  sprintf(
    '<pre class="btw-output-message"><code class="nohighlight">%s</code></pre>',
    trimws(content@text)
  )
}

S7::method(contents_html, ContentWarning) <- function(content, ...) {
  sprintf(
    '<pre class="btw-output-warning"><code class="nohighlight">%s</code></pre>',
    trimws(content@text)
  )
}

S7::method(contents_html, ContentError) <- function(content, ...) {
  sprintf(
    '<pre class="btw-output-error"><code class="nohighlight">%s</code></pre>',
    trimws(content@text)
  )
}

contents_shinychat <- S7::new_external_generic(
  package = "shinychat",
  name = "contents_shinychat",
  dispatch_args = "content"
)

S7::method(contents_shinychat, BtwRunToolResult) <- function(content) {
  code <- content@extra$code

  # Render all content objects to HTML
  contents <- content@extra$contents
  # ---- Deal with ANSI codes in content objects
  contents <- map(contents, function(x) {
    run_r_content_handle_ansi(x, plain = !is_installed("fansi"))
  })
  output_html <- map_chr(contents, ellmer::contents_html)
  output_html <- paste(output_html, collapse = "\n")

  status <- content@extra$status
  request_id <- NULL
  tool_title <- NULL

  if (!is.null(content@request)) {
    request_id <- content@request@id

    tool_title <- NULL
    tool <- content@request@tool
    if (!is.null(tool)) {
      tool_title <- tool@annotations$title
    }
  }

  dep <- htmltools::htmlDependency(
    name = "btw-run-r",
    version = utils::packageVersion("btw"),
    package = "btw",
    src = "js/run-r",
    script = list(list(src = "btw-run-r.js", type = "module")),
    stylesheet = "btw-run-r.css",
    all_files = FALSE
  )

  htmltools::tag(
    "btw-run-r-result",
    list(
      `request-id` = request_id,
      code = code,
      status = status,
      `tool-title` = tool_title,
      htmltools::HTML(output_html),
      dep
    )
  )
}

is_mergeable_content <- function(x, y) {
  mergeable_content_types <- list(
    ContentCode,
    ContentMessage,
    ContentWarning,
    ContentError
  )

  for (cls in mergeable_content_types) {
    if (S7::S7_inherits(x, cls) && S7::S7_inherits(y, cls)) {
      return(TRUE)
    }
  }

  FALSE
}

#' Merge adjacent content of the same type
#'
#' Reduces a list of Content objects by concatenating adjacent elements
#' of the same mergeable type (ContentCode, ContentMessage, ContentWarning,
#' ContentError) into single elements.
#'
#' @param contents List of Content objects
#' @returns List of Content objects with adjacent same-type elements merged
#' @keywords internal
merge_adjacent_content <- function(contents) {
  if (length(contents) <= 1) {
    return(contents)
  }

  reduce(
    contents,
    function(acc, item) {
      if (length(acc) == 0) {
        return(list(item))
      }

      last <- acc[[length(acc)]]

      if (is_mergeable_content(last, item)) {
        # Merge by concatenating text with newline
        merged_text <- paste(last@text, item@text, sep = "\n")
        S7::prop(acc[[length(acc)]], "text") <- merged_text
        acc
      } else {
        append(acc, list(item))
      }
    },
    .init = list()
  )
}
