#' @include tool-result.R
NULL

#' Tool: Read a Web Page as Markdown
#'
#' @details
#' You can control the maximum time to wait for the page to load by setting
#' the `btw.max_wait_for_page_load_s` option globally in your R session.
#'
#' @examplesIf rlang::is_installed("chromote") && rlang::is_interactive()
#' btw_tool_web_read_url("https://www.r-project.org/")
#' btw_tool_web_read_url(
#'   "https://posit.co/blog/easy-tool-calls-with-ellmer-and-chatlas/"
#' )
#'
#' @param url The URL of the web page to read.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a `BtwWebPageResult` object that inherits from
#'   [ellmer::ContentToolResult] containing the markdown content of the web
#'   page.
#'
#' @family web tools
#' @export
btw_tool_web_read_url <- function(url, `_intent`) {}

btw_tool_web_read_url_impl <- function(
  url,
  ...,
  max_wait_for_page_load_s = getOption("btw.max_wait_for_page_load_s", 10)
) {
  rlang::check_installed("chromote")
  html <- read_url_main_content(url, timeout = max_wait_for_page_load_s)

  if (is.null(html) || !nzchar(html)) {
    cli::cli_abort(
      "Failed to read web page at {.url {url}}. Please check the URL."
    )
  }

  md <- paste(pandoc_html_simplify(html), collapse = "\n")
  res <- glue_('<web_page_content url="{{url}}">\n{{md}}\n</web_page_content>')

  BtwWebPageResult(res)
}

BtwWebPageResult <- S7::new_class(
  "BtwWebPageResult",
  parent = BtwToolResult
)

has_chromote <- function() {
  is_installed("chromote")
}

.btw_add_to_tools(
  name = "btw_tool_web_read_url",
  group = "web",
  tool = function() {
    ellmer::tool(
      function(url) {
        btw_tool_web_read_url_impl(url = url) # nocov
      },
      name = "btw_tool_web_read_url",
      description = r'---(Read a web page and convert it to Markdown format.

This tool fetches the content of a web page and returns it as a simplified Markdown representation.

WHEN TO USE: Use this tool when you need to access and analyze the content of a web page, e.g. when the user asks you to read the contents of a webpage.)---',
      annotations = ellmer::tool_annotations(
        title = "Read Web Page",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        btw_can_register = function() {
          if (has_chromote()) {
            return(TRUE)
          }
          cli::cli_warn(
            "The `chromote` package is required to use the web reading tool."
          )
          FALSE
        }
      ),
      arguments = list(
        url = ellmer::type_string(
          "The URL of the web page to read."
        )
      )
    )
  }
)

read_url_main_content <- function(url, timeout = 10) {
  b <- chromote::ChromoteSession$new()
  withr::defer(b$close())

  p <- b$Page$loadEventFired(wait_ = FALSE, timeout_ = timeout)
  b$Page$navigate(url, wait_ = FALSE)
  b$wait_for(p)

  wait_for_network_idle(b, timeout = timeout)

  conversion_script <- paste(
    readLines(system.file("js", "clean-url.js", package = "btw")),
    collapse = "\n"
  )

  result <- b$Runtime$evaluate(conversion_script, returnByValue = TRUE)

  result$result$value
}


#' Wait for Network Idle in Chromote Session
#'
#' @param session A chromote session object
#' @param idle_time Time in seconds to wait for no network activity
#' @param timeout Maximum time in seconds to wait
#' @param max_requests Maximum number of active requests to consider "idle"
#' @return List with success status and final metrics
#'
#' @noRd
wait_for_network_idle <- function(
  session,
  idle_time = 1,
  timeout = 10,
  max_requests = 0
) {
  # Enable required domains
  session$Network$enable()
  session$Page$enable()

  # Initialize tracking variables
  active_requests <- new.env(hash = TRUE)
  last_activity_time <- Sys.time()

  # Helper functions
  should_ignore_request <- function(url) {
    ignored_patterns <- c(
      "google-analytics",
      "doubleclick",
      "wistia",
      "facebook\\.com/tr",
      "googletagmanager",
      "\\.(gif|mp4|m4v|webp|jpg|jpeg|mov|avi|mp3)(\\?|$)",
      "analytics",
      "^data:"
    )
    any(grepl(paste(ignored_patterns, collapse = "|"), url, ignore.case = TRUE))
  }

  get_pending_count <- function() {
    length(ls(active_requests))
  }

  get_idle_time <- function() {
    as.numeric(difftime(Sys.time(), last_activity_time, units = "secs"))
  }

  # Set up event listeners
  session$Network$requestWillBeSent(function(params) {
    if (!should_ignore_request(params$request$url)) {
      active_requests[[params$requestId]] <- list(
        url = params$request$url,
        timestamp = Sys.time()
      )
      # Uncomment to debug requests that block loading
      # cli::cli_inform("Requesting: {params$request$url}")
      last_activity_time <<- Sys.time()
    }
  })

  session$Network$loadingFinished(function(params) {
    if (exists(params$requestId, envir = active_requests)) {
      rm(list = params$requestId, envir = active_requests)
    }
    last_activity_time <<- Sys.time()
  })

  session$Network$loadingFailed(function(params) {
    # nocov start
    if (exists(params$requestId, envir = active_requests)) {
      rm(list = params$requestId, envir = active_requests)
    }
    last_activity_time <<- Sys.time()
    # nocov end
  })

  session$Network$responseReceived(function(params) {
    last_activity_time <<- Sys.time()
  })

  # Wait for network idle
  start_time <- Sys.time()

  repeat {
    # Check timeout
    elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    if (elapsed_time > timeout) {
      warning(sprintf(
        "Timeout waiting for network idle after %.1f seconds",
        timeout
      ))
      return(list(
        success = FALSE,
        reason = "timeout",
        elapsed_time = elapsed_time,
        final_request_count = get_pending_count()
      ))
    }

    # Check network idle conditions
    current_requests <- get_pending_count()
    current_idle_time <- get_idle_time()

    if (current_requests <= max_requests && current_idle_time >= idle_time) {
      return(list(
        success = TRUE,
        elapsed_time = elapsed_time,
        final_request_count = current_requests,
        idle_time_achieved = current_idle_time
      ))
    }

    # Small delay before next check
    Sys.sleep(0.1)
  }
}
