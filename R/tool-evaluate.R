#' S7 Content Types for Code Evaluation Results
#'
#' These S7 classes extend ellmer's ContentText to provide semantic meaning
#' to different types of text output from code evaluation.
#'
#' @name ContentTypes
#' @keywords internal
NULL

#' @rdname ContentTypes
ContentMessage <- S7::new_class(
  "ContentMessage",
  parent = ellmer::ContentText
)

#' @rdname ContentTypes
ContentWarning <- S7::new_class(
  "ContentWarning",
  parent = ellmer::ContentText
)

#' @rdname ContentTypes
ContentError <- S7::new_class(
  "ContentError",
  parent = ellmer::ContentText
)

#' Tool: Evaluate R code
#'
#' This tool evaluates R code and returns results as ellmer Content objects.
#' It captures text output, plots, messages, warnings, and errors.
#' Code evaluation stops on the first error, returning all results up to that point.
#'
#' @param code A character string containing R code to evaluate.
#' @param `_intent` Intent description (automatically added by ellmer).
#'
#' @returns A list of ellmer Content objects:
#'   - `ContentText`: visible return values and text output
#'   - `ContentMessage`: messages from `message()`
#'   - `ContentWarning`: warnings from `warning()`
#'   - `ContentError`: errors from `stop()`
#'   - `ContentImageInline`: plots created during evaluation
#'
#' @examples
#' \dontrun{
#' # Simple calculation
#' btw_tool_evaluate("2 + 2")
#'
#' # Code with plot
#' btw_tool_evaluate("hist(rnorm(100))")
#'
#' # Code with warning
#' btw_tool_evaluate("mean(c(1, 2, NA))")
#' }
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @export
btw_tool_evaluate <- function(code, `_intent`) {}

btw_tool_evaluate_impl <- function(code, ...) {
  check_dots_empty()
  check_string(code)

  # Check if evaluate package is available
  check_installed("evaluate", "to evaluate R code.")

  # Initialize list to store Content objects
  contents <- list()
  # Store the last value for potential use
  last_value <- NULL

  # Create output handler that converts to Content types as outputs are generated
  handler <- evaluate::new_output_handler(
    source = function(src, expr) {
      # Skip source code echoing by returning NULL
      NULL
    },
    text = function(text) {
      # Text output (from print, cat, etc.)
      contents <<- append(contents, list(ellmer::ContentText(text)))
      text
    },
    graphics = function(plot) {
      # Save plot to temporary file
      tmp <- withr::local_tempfile(fileext = ".png")
      grDevices::png(tmp, width = 768, height = 768)
      grDevices::replayPlot(plot)
      grDevices::dev.off()

      # Read and encode as base64
      img_data <- base64enc::base64encode(tmp)
      contents <<- append(
        contents,
        list(
          ellmer::ContentImageInline(type = "image/png", data = img_data)
        )
      )

      plot
    },
    message = function(msg) {
      # Message output
      msg_text <- conditionMessage(msg)
      # Remove trailing newline that message() adds
      msg_text <- sub("\n$", "", msg_text)
      contents <<- append(
        contents,
        list(
          ContentMessage(text = msg_text)
        )
      )
      msg
    },
    warning = function(warn) {
      # Warning message
      warn_text <- conditionMessage(warn)
      contents <<- append(
        contents,
        list(
          ContentWarning(text = warn_text)
        )
      )
      warn
    },
    error = function(err) {
      # Error message
      err_text <- conditionMessage(err)
      contents <<- append(
        contents,
        list(
          ContentError(text = err_text)
        )
      )
      err
    },
    value = function(value, visible) {
      # Store the actual value when it's visible (meaningful output)
      # Invisible values include assignments and side-effect returns
      if (visible) {
        last_value <<- value
        # Also add as text content
        value_text <- paste(
          utils::capture.output(print(value)),
          collapse = "\n"
        )
        contents <<- append(
          contents,
          list(
            ellmer::ContentText(value_text)
          )
        )
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

  # Return as ContentToolResult with contents in extra
  # The value is stored but the contents list is what gets displayed
  BtwToolResult(
    contents,
    extra = list(
      data = last_value,
      code = code
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_evaluate",
  group = "env",
  tool = function() {
    ellmer::tool(
      function(code) {
        btw_tool_evaluate_impl(code = code)
      },
      name = "btw_tool_evaluate",
      description = "Execute R code and return results as Content objects. Captures text output, plots, messages, warnings, and errors. Stops on first error.",
      annotations = ellmer::tool_annotations(
        title = "Evaluate R Code",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        code = ellmer::type_string(
          "R code to evaluate as a string."
        )
      )
    )
  }
)
