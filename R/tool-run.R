#' Tool: Run R code
#'
#' @description
#' `r lifecycle::badge("experimental")`
#' This tool runs R code and returns results as a list of [ellmer::Content()]
#' objects. It captures text output, plots, messages, warnings, and errors. Code
#' execution stops on the first error, returning all results up to that point.
#'
#' @section Security Considerations:
#' Executing arbitrary R code can pose significant security risks, especially
#' in shared or multi-user environments. Furthermore, neither \pkg{shinychat}
#' (as of v0.4.0) or nor \pkg{ellmer} (as of v0.4.0) provide a mechanism to
#' review and reject the code before execution. Even more, the code is executed
#' in the global environment and does not have any sandboxing or R code
#' limitations applied.
#'
#' It is your responsibility to ensure that you are taking appropriate measures
#' to reduce the risk of the LLM writing arbitrary code. Most often, this means
#' not prompting the model to take large or potentially destructive actions.
#' At this time, we do not recommend that you enable this tool in a publicly-
#' available environment without strong safeguards in place.
#'
#' That said, this tool is very powerful and can greatly enhance the
#' capabilities of your btw chatbots. Please use it responsibly! If you'd like
#' to enable the tool, please read the instructions below.
#'
#' @section Enabling this tool:
#' This tool is not enabled by default in [btw_tools()], [btw_app()] or
#' [btw_client()]. To enable the function, you have a few options:
#'
#' 1. Set the `btw.run_r.enabled` option to `TRUE` in your R session, or in your
#'    `.Rprofile` file to enable it globally.
#' 2. Set the `BTW_RUN_R_ENABLED` environment variable to `true` in your
#'    `.Renviron` file or your system environment.
#' 3. Explicitly include the tool when calling `btw_tools("run")` (unless the
#'    above options disable it).
#'
#' In your [btw.md file][use_btw_md], you can explicitly enable the tool by
#' naming it in the tools option
#'
#' ```md
#' ---
#' tools:
#'   - run_r
#' ---
#' ```
#'
#' or you can enable the tool by setting the `btw.run_r.enabled` option from the
#' `options` list in `btw.md` (this approach is useful if you've globally
#' disabled the tool but want to enable it for a specific btw chat):
#'
#' ```md
#' ---
#' options:
#'   run_r:
#'     enabled: true
#' ---
#' ```
#'
#' @details
#' ## Configuration Options
#'
#' The behavior of the `btw_tool_run_r` tool can be customized using the
#' following R options:
#'
#' * `btw.run_r.graphics_device`: A function that creates a graphics device used
#'   for rendering plots. By default, it uses `ragg::agg_png()` if the `ragg`
#'   package is installed, otherwise it falls back to `grDevices::png()`.
#' * `btw.run_r.plot_aspect_ratio`: Aspect ratio for plots created during code
#'   execution. Can be a character string of the form `"w:h"` (e.g., `"16:9"`)
#'   or a numeric value representing width/height (e.g., `16/9`). Default is
#'   `"3:2"`.
#' * `btw.run_r.plot_size`: Integer pixel size for the longest side of plots.
#'   Default is `768L`. This image size was selected to match [OpenAI's image
#'   resizing rules](https://platform.openai.com/docs/guides/images-vision?api-mode=responses),
#'   where images are resized such that the largest size is 768px. Another
#'   common choice is 512px. Larger images may be used but will result in
#'   increased token sizes.
#' * `btw.run_r.enabled`: Logical flag to enable or disable the tool globally.
#'
#' These values can be set using [options()] in your R session or `.Rprofile` or
#' in a [btw.md file][use_btw_md] under the `options` section.
#'
#' ```md
#' ---
#' options:
#'  run_r:
#'    enabled: true
#'    plot_aspect_ratio: "16:9"
#'    plot_size: 512
#' ---
#' ```
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
#' @family run tools
#' @export
btw_tool_run_r <- function(code, `_intent`) {}

btw_tool_run_r_impl <- function(
  code,
  .envir = global_env(),
  show_last_value = TRUE
) {
  check_string(code)
  check_bool(show_last_value)
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

    dims <- btw_run_r_plot_dimensions(
      ratio = getOption("btw.run_r.plot_aspect_ratio", "3:2"),
      longest_side = getOption("btw.run_r.plot_size", 768L)
    )

    path_plot <- withr::local_tempfile(fileext = ".png")
    run_r_plot_device(
      filename = path_plot,
      width = dims$width,
      height = dims$height
    )
    tryCatch(
      grDevices::replayPlot(last_plot),
      finally = {
        grDevices::dev.off()
      }
    )

    append_content(ellmer::content_image_file(path_plot, resize = "none"))
    last_plot <<- NULL
  }

  # Ensure working directory, options, envvar are restored after execution
  withr::local_dir(getwd())
  withr::local_options()
  withr::local_envvar()

  local_reproducible_output(disable_ansi_features = !is_installed("fansi"))

  # Create output handler that converts to Content types as outputs are generated
  handler <- evaluate::new_output_handler(
    source = function(src, expr) {
      # Skip source code echoing by returning NULL
      src_code <- sub("\n$", "", src$src)
      append_content(ContentSource(text = src_code))
    },
    text = function(text) {
      append_last_plot()
      # Text output (from print, cat, etc.)
      append_content(ContentOutput(text = text))
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
      last_value <<- value

      if (visible && show_last_value) {
        # Also add as code content
        value_text <- paste(
          utils::capture.output(print(value)),
          collapse = "\n"
        )
        append_content(ContentOutput(text = value_text))
      }
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

  # For `value`, drop source code blocks and remove all ANSI codes
  value <- keep(contents, function(x) !S7::S7_inherits(x, ContentSource))
  value <- map(value, run_r_content_handle_ansi)

  if (length(value) == 0) {
    value <- if (had_error) {
      "(The code encountered an error but did not produce any output.)"
    } else {
      "(The code ran successfully but did not produce any output.)"
    }
  } else if (every(value, S7::S7_inherits, ellmer::ContentText)) {
    # Flatten text-only output into a single string
    value <- paste(
      map_chr(value, ellmer::contents_text),
      collapse = "\n"
    )
  }

  BtwRunToolResult(
    value = value,
    extra = list(
      data = last_value,
      code = code,
      contents = contents,
      # We always return contents up to the error as `value` because `error`
      # cannot handle rich output. We'll show status separately in the UI.
      status = if (had_error) "error" else "success",
      display = list(open = TRUE, copy_code = TRUE)
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
    return(ragg::agg_png(..., scaling = 1.5))
  }

  grDevices::png(...)
}

btw_can_register_run_r_tool <- function() {
  rlang::is_installed("evaluate") &&
    btw_run_r_tool_is_enabled()
}

btw_run_r_tool_is_enabled <- function() {
  opt <- getOption("btw.run_r.enabled", default = NULL)
  if (!is.null(opt)) {
    return(isTRUE(opt))
  }

  envvar <- Sys.getenv("BTW_RUN_R_ENABLED", unset = "")
  if (nzchar(envvar)) {
    return(tolower(trimws(envvar)) %in% c("true", "1"))
  }

  switch(
    getOption(".btw_tools.match_mode", default = "default"),
    "explicit" = TRUE,
    FALSE
  )
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
      description = r"---(Run R code.

Executes R code and captures printed values, text output, plots, messages, warnings, and errors.

## CORE RULES (FOLLOW STRICTLY)
- MUST work incrementally: each call should do one small, well-defined task
- MUST create no more than one rendered figure per tool call. Use separate calls for multiple figures.
- MUST NOT use this tool to "talk to the user". Explanations and interpretation belong in the assistant message
- MUST read any error messages carefully
- MUST NOT make more than 2 attempts to fix an error
    - After 2 failed attempts: stop, summarize what you tried, include the error(s), and propose the next change without executing it.

## SAFETY REQUIREMENTS (MUST FOLLOW)
- This code runs in a global environment. Write code that is safe, reversible, and non-destructive
- MUST NOT perform any of the following UNLESS the user explicitly requests it and you first show the code and target paths/URLs:
    - File writes or modifications (persistent output, overwriting, deleting)
    - System/shell execution (system, system2, pipe, shell)
    - Network requests
    - Package installation or updates
- SHOULD NOT change global state (options, environment variables, working directory, etc.)
    - Working directory, options and environment variables are reset between tool calls
- MUST use temporary files for any ephemeral storage needs (`tempfile()`)

## CODE AND OUTPUT STYLE
- ALWAYS write clear, concise, and idiomatic R code, preferring packages and functions from the tidyverse ecosystem when available
- PREFER less than 50 lines of code per tool call
- SHOULD use code comments to explain only the non-obvious parts of the code
    - AVOID using comments to literally describe the code
- DO return results implicitly (`x`, not `print(x)`)
- DO make the last expression the object you want to show (e.g. a data frame, tibble, list or scalar)
- AVOID `print()` and `cat()` unless necessary. If `cat()` is unavoidable, you MUST use a SINGLE `cat()` call and keep it concise
- PREFER returning structured objects (tibbles, data frames, lists) and brief summaries (`head()`, `str()`, `summary()`)
- AVOID extremely large outputs; show summaries and return key results
      )---",
      annotations = ellmer::tool_annotations(
        title = "Run R Code",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        btw_can_register = btw_can_register_run_r_tool
      ),
      arguments = list(
        code = ellmer::type_string("The R code to run")
      )
    )
  }
)

# ---- Content Types ----
ContentSource <- S7::new_class(
  "ContentSource",
  parent = ellmer::ContentText
)

ContentOutput <- S7::new_class(
  "ContentOutput",
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

trim_outer_nl <- function(x) {
  x <- sub("^\r?\n", "", x)
  sub("\r?\n$", "", x)
}

btw_pre_output <- function(text, pre_class, code_class = "nohighlight") {
  text <- trim_outer_nl(text)
  if (!nzchar(text)) {
    return("")
  }

  sprintf(
    '<pre class="btw-output-%s"><code class="%s">%s</code></pre>',
    pre_class,
    code_class,
    text
  )
}

S7::method(contents_html, ContentSource) <- function(content, ...) {
  btw_pre_output(content@text, pre_class = "source", code_class = "language-r")
}

S7::method(contents_html, ContentOutput) <- function(content, ...) {
  btw_pre_output(content@text, pre_class = "output")
}

S7::method(contents_html, ContentMessage) <- function(content, ...) {
  btw_pre_output(content@text, pre_class = "message")
}

S7::method(contents_html, ContentWarning) <- function(content, ...) {
  btw_pre_output(content@text, pre_class = "warning")
}

S7::method(contents_html, ContentError) <- function(content, ...) {
  btw_pre_output(content@text, pre_class = "error")
}

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

  display <- content@extra$display %||% list()
  annotations <- list()
  intent <- ""

  if (!is.null(content@request)) {
    request_id <- content@request@id

    tool_title <- NULL
    tool <- content@request@tool
    annotations <- tool@annotations
    if (!is.null(content@request@arguments$`_intent`)) {
      intent <- content@request@arguments$`_intent`
    }
  }

  htmltools::tag(
    "btw-run-r-result",
    list(
      `request-id` = request_id,
      code = code,
      status = status,
      intent = intent,
      `tool-title` = display$title %||% annotations$title %||% "Run R Code",
      icon = display$icon %||% annotations$icon,
      expanded = if (isTRUE(display$open)) NA,
      `copy-code` = if (isTRUE(display$copy_code)) NA,
      htmltools::HTML(output_html),
      btw_run_tool_card_dep()
    )
  )
}

btw_run_tool_card_dep <- function() {
  htmltools::htmlDependency(
    name = "btw-run-r",
    version = utils::packageVersion("btw"),
    package = "btw",
    src = "js/run-r",
    script = list(
      list(src = "btw-icons.js", type = "module"),
      list(src = "btw-run-r.js", type = "module")
    ),
    stylesheet = "btw-run-r.css",
    all_files = FALSE
  )
}

is_mergeable_content <- function(x, y) {
  mergeable_content_types <- list(
    ContentSource,
    ContentOutput,
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
#' of the same mergeable type (ContentOutput, ContentMessage, ContentWarning,
#' ContentError) into single elements.
#'
#' @param contents List of Content objects
#' @returns List of Content objects with adjacent same-type elements merged
#' @noRd
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

#' Compute plot dimensions from aspect ratio
#'
#' @param ratio Either:
#'   - character of the form "w:h" (e.g. "16:9", "5:9"), or
#'   - numeric giving width/height (e.g. 16/9, 1.777...).
#' @param longest_side Integer pixel size for the longest side (default 768).
#'
#' @return Named list with `width` and `height` in pixels, where
#'   max(width, height) == longest_side.
#' @noRd
btw_run_r_plot_dimensions <- function(ratio, longest_side = 768L) {
  r <- parse_ratio(ratio)

  if (r >= 1) {
    # Width is longer
    width <- longest_side
    height <- longest_side / r
  } else {
    # Height is longer
    height <- longest_side
    width <- longest_side * r
  }

  list(
    width = as.integer(round(width)),
    height = as.integer(round(height))
  )
}

#' Parse an aspect ratio specification
#'
#' @param ratio Either:
#'   - character of the form "w:h" (e.g. "16:9", "5:9"), or
#'   - numeric giving width/height (e.g. 16/9, 1.777...).
#'
#' @return Numeric scalar giving width/height.
#' @noRd
parse_ratio <- function(ratio) {
  if (is.character(ratio)) {
    parts <- strsplit(ratio, ":", fixed = TRUE)[[1]]
    if (length(parts) != 2L) {
      cli::cli_abort(
        "Invalid ratio string '{ratio}'. Use the form 'w:h', e.g. '16:9'.",
        call = caller_env(n = 2)
      )
    }
    nums <- suppressWarnings(as.numeric(parts))
    if (any(is.na(nums)) || any(nums <= 0)) {
      cli::cli_abort(
        "Both sides of the ratio must be positive numbers, e.g. '16:9'.",
        call = caller_env(n = 2)
      )
    }
    return(nums[1] / nums[2])
  }

  check_number_decimal(ratio, allow_infinite = FALSE, min = 0)
  ratio
}
