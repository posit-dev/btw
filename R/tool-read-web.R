#' @include tool-result.R
NULL

#' Tool: Read a Web Page as Markdown
#'
#' @examplesIf rlang::is_installed("chromote") && rlang::is_interactive()
#' btw_tool_web_read_url("https://www.r-project.org/")
#'
#' @param url The URL of the web page to read.
#' @param ... Ignored, for future features.
#' @param max_wait_for_page_load_s Maximum time to wait for the page to load, in
#'   seconds. Can be set globally using the `btw.max_wait_for_page_load_s`
#'   option.
#'
#' @family Tools
#' @export
btw_tool_web_read_url <- function(
  url,
  ...,
  max_wait_for_page_load_s = getOption("btw.max_wait_for_page_load_s", 10)
) {
  html <- read_url_main_content(url)
  md <- paste(pandoc_html_simplify(html), collapse = "\n")

  BtwWebPageResult(md)
}

BtwWebPageResult <- S7::new_class(
  "BtwWebPageResult",
  parent = BtwToolResult
)

.btw_add_to_tools(
  name = "btw_tool_web_read_url",
  group = "web",
  tool = function() {
    ok <- tryCatch(
      {
        rlang::check_installed("chromote", version = "0.5.1.9000")
        TRUE
      },
      error = function(e) {
        cli::cli_warn(
          "The `chromote` package is required to use the web reading tool."
        )
        FALSE
      }
    )

    if (!ok) {
      return(NULL)
    }

    ellmer::tool(
      btw_tool_web_read_url,
      .description = 'Read a web page and convert it to Markdown format.

This tool fetches the content of a web page and returns it as a simplified Markdown representation.

WHEN TO USE: Use this tool when you need to access and analyze the content of a web page, e.g. when the user asks you to read the contents of a webpage.',
      .annotations = ellmer::tool_annotations(
        title = "Read Web Page",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE
      ),
      url = ellmer::type_string(
        "The URL of the web page to read."
      )
    )
  }
)

read_url_main_content <- function(url, timeout = 30) {
  b <- chromote::ChromoteSession$new()
  b$go_to(url)
  withr::defer(b$close())

  wait_for_network_idle(b)

  # JavaScript to find main content and convert to markdown
  conversion_script <- r"---(
(function() {
  // Function to find the main content container
  function findMainContent() {
    // List of selectors to try, in order of preference
    const selectors = [
      "main",
      "[role=\"main\"]",
      "#main",
      ".main",
      "#content",
      ".content",
      "#main-content",
      ".main-content",
      ".container.page",
      "article",
      ".article",
      "#article",
      ".post",
      "#post",
      ".entry",
      "#entry",
      ".container",
      "#container"
    ];

    for (const selector of selectors) {
      const element = document.querySelector(selector);
      if (element && element.innerText.trim().length > 100) {
        return element;
      }
    }

    // If no specific container found, try to find the largest content block
    const candidates = document.querySelectorAll("div, section, article");
    let bestCandidate = null;
    let maxTextLength = 0;

    for (const candidate of candidates) {
      const textLength = candidate.innerText.trim().length;

      // Skip if too small or if it contains the entire body content
      if (textLength > 200 && textLength < document.body.innerText.length * 0.9) {
        if (textLength > maxTextLength) {
          maxTextLength = textLength;
          bestCandidate = candidate;
        }
      }
    }

    // Fallback to body if nothing else works
    return bestCandidate || document.body;
  }

  // Find the main content element
  const mainContentElement = findMainContent();

  // Remove all svg, style, script elements
  mainContentElement.querySelectorAll("svg, style, script").forEach(el => el.remove());

  // Simplify `<code>` elements
  mainContentElement.querySelectorAll('code').forEach(codeElement => {
    // 1. Remove all span wrappers while preserving text content
    const spans = codeElement.querySelectorAll('span');
    spans.forEach(span => {
      // Replace span with its text content
      const textNode = document.createTextNode(span.textContent);
      span.parentNode.replaceChild(textNode, span);
    });

    // Normalize text nodes (merge adjacent text nodes)
    codeElement.normalize();

    // 2. Check if code is multi-line and parent isn't <pre>
    const isMultiLine = codeElement.textContent.includes('\n');
    const parentIsPre = codeElement.parentElement.tagName === 'PRE';

    if (isMultiLine && !parentIsPre) {
      // 3. Wrap the code element in a pre element
      const preElement = document.createElement('pre');

      // Insert the pre element before the code element
      codeElement.parentNode.insertBefore(preElement, codeElement);

      // Move the code element inside the pre element
      preElement.appendChild(codeElement);
    }
  });

  // Ensure all links are absolute
  mainContentElement.querySelectorAll("a").forEach(el => {
    if (el.href && !el.getAttribute("href").startsWith("http")) {
      el.setAttribute('href', new URL(el.href, document.location.href).href);
    }
  });

  return mainContentElement.innerHTML;
})();
)---"

  result <- b$Runtime$evaluate(conversion_script, returnByValue = TRUE)

  result$result$value
}


#' Wait for Network Idle in Chromote Session
#'
#' @param session A chromote session object
#' @param idle_time Time in seconds to wait for no network activity (default: 0.5)
#' @param timeout Maximum time in seconds to wait (default: 10)
#' @param max_requests Maximum number of active requests to consider "idle" (default: 0)
#' @return List with success status and final metrics
#' @noRd
wait_for_network_idle <- function(
  session,
  idle_time = 0.5,
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
      "facebook\\.com/tr",
      "googletagmanager",
      "\\.gif(\\?|$)",
      "analytics"
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
    if (exists(params$requestId, envir = active_requests)) {
      rm(list = params$requestId, envir = active_requests)
    }
    last_activity_time <<- Sys.time()
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
