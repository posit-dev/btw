#' Tool: Run R code
#'
#' This tool runs R code and returns results as ellmer Content objects.
#' It captures text output, plots, messages, warnings, and errors.
#' Code execution stops on the first error, returning all results up to that point.
#'
#' @param code A character string containing R code to run.
#' @param `_intent` Intent description (automatically added by ellmer).
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

btw_tool_run_r_impl <- function(code) {
  check_string(code)
  check_installed("evaluate", "to run R code.")

  # Initialize list to store Content objects
  contents <- list()
  append_content <- function(x) contents <<- c(contents, list(x))

  # Store the last value for potential use
  last_value <- NULL
  # Track if an error occurred
  had_error <- FALSE

  # Create output handler that converts to Content types as outputs are generated
  handler <- evaluate::new_output_handler(
    source = function(src, expr) {
      # Skip source code echoing by returning NULL
      NULL
    },
    text = function(text) {
      # Text output (from print, cat, etc.)
      append_content(ContentCode(text = text))
      text
    },
    graphics = function(plot) {
      # Save plot to temporary file
      path_plot <- withr::local_tempfile(fileext = ".png")
      grDevices::png(path_plot, width = 768, height = 768)
      grDevices::replayPlot(plot)
      grDevices::dev.off()

      append_content(ellmer::content_image_file(path_plot))
      plot
    },
    message = function(msg) {
      # Message output
      msg_text <- conditionMessage(msg)
      # Remove trailing newline that message() adds
      msg_text <- sub("\n$", "", msg_text)
      append_content(ContentMessage(text = msg_text))
      msg
    },
    warning = function(warn) {
      # Warning message
      append_content(ContentWarning(conditionMessage(warn)))
      warn
    },
    error = function(err) {
      # Error message
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

  # Evaluate the code with our custom handler
  evaluate::evaluate(
    code,
    envir = global_env(),
    stop_on_error = 1,
    new_device = TRUE,
    output_handler = handler
  )

  # Merge adjacent content of the same type
  contents <- merge_adjacent_content(contents)

  # Render all content objects to HTML
  output_html <- vapply(
    contents,
    function(content) ellmer::contents_html(content),
    character(1)
  )
  output_html <- paste(output_html, collapse = "\n")

  # Return as BtwRunToolResult
  BtwRunToolResult(
    value = contents,
    error = if (had_error) contents[[length(contents)]]@text else NULL,
    extra = list(
      data = last_value,
      code = code,
      output_html = output_html
    )
  )
}

btw_can_register_run_r_tool <- function() {
  rlang::is_installed("evaluate")
}

.btw_add_to_tools(
  name = "btw_tool_run_r",
  group = "run",
  tool = function() {
    ellmer::tool(
      btw_tool_run_r_impl,
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
  text <- htmltools::htmlEscape(content@text)
  sprintf('<pre><code>%s</code></pre>', trimws(text))
}

S7::method(contents_html, ContentMessage) <- function(content, ...) {
  text <- htmltools::htmlEscape(content@text)

  sprintf(
    '<pre class="btw-output-message"><code>%s</code></pre>',
    trimws(text)
  )
}

S7::method(contents_html, ContentWarning) <- function(content, ...) {
  text <- htmltools::htmlEscape(content@text)
  sprintf(
    '<pre class="btw-output-warning"><code>%s</code></pre>',
    trimws(text)
  )
}

S7::method(contents_html, ContentError) <- function(content, ...) {
  text <- htmltools::htmlEscape(content@text)
  sprintf(
    '<pre class="btw-output-error"><code>%s</code></pre>',
    trimws(text)
  )
}

contents_shinychat <- S7::new_external_generic(
  package = "shinychat",
  name = "contents_shinychat",
  dispatch_args = "content"
)

S7::method(contents_shinychat, BtwRunToolResult) <- function(content) {
  code <- content@extra$code
  output_html <- content@extra$output_html
  request_id <- content@request@id
  status <- if (!is.null(content@error)) "error" else "success"

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
