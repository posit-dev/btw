test_that("with_retry() works", {
  third_try <- local({
    i <- 3
    function() {
      i <<- i - 1
      if (i > 0) {
        expect_true(FALSE)
      }
      expect_true(TRUE)
    }
  })

  with_retry(
    times = 5,
    {
      third_try()
    },
    verbose = FALSE
  )
})

test_that("ci: prep chromote", {
  with_retry({
    # Try to warm up chromote. IDK why it fails on older versions of R.
    skip_on_cran()
    skip_if_not_installed("chromote")

    on_ci <- isTRUE(as.logical(Sys.getenv("CI")))
    skip_if(!on_ci, "Not on CI")

    # Wrap in a `try()` as the test doesn't matter
    # Only the action of trying to open chromote matters
    try({
      chromote <- utils::getFromNamespace(
        "default_chromote_object",
        "chromote"
      )()
      chromote$new_session()
    })

    expect_true(TRUE)
  })
})

test_that("has_chromote() detects chromote availability", {
  # When chromote is installed
  local_mocked_bindings(
    check_installed = function(...) TRUE,
    .package = "rlang"
  )
  expect_true(has_chromote())

  # When chromote is not installed (error thrown)
  local_mocked_bindings(
    check_installed = function(...) {
      stop("chromote is not installed")
    },
    .package = "rlang"
  )
  expect_false(has_chromote())
})

test_that("BtwWebPageResult is properly defined", {
  expect_true(S7::S7_inherits(BtwWebPageResult))

  instance <- BtwWebPageResult("test")
  expect_s7_class(instance, BtwWebPageResult)
  expect_s7_class(instance, BtwToolResult)
})

test_that("btw_tool_web_read_url_impl() returns BtwWebPageResult", {
  with_retry({
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("chromote")

    result <- btw_tool_web_read_url_impl("https://www.r-project.org/")

    expect_s7_class(result, BtwWebPageResult)
    expect_s7_class(result, BtwToolResult)
    expect_s7_class(result, ellmer::ContentToolResult)
    expect_type(result@value, "character")
    expect_match(
      result@value,
      '<web_page_content url="https://www.r-project.org/',
      fixed = TRUE
    )
    expect_match(result@value, "www.r-project.org")
  })
})

test_that("btw_tool_web_read_url_impl() handles empty content", {
  # Mock read_url_main_content to return empty string
  local_mocked_bindings(
    read_url_main_content = function(...) ""
  )

  expect_error(
    btw_tool_web_read_url_impl("https://example.com"),
    "Failed to read web page"
  )
})

test_that("btw_tool_web_read_url_impl() handles NULL content", {
  # Mock read_url_main_content to return NULL
  local_mocked_bindings(
    read_url_main_content = function(...) NULL
  )

  expect_error(
    btw_tool_web_read_url_impl("https://example.com"),
    "Failed to read web page"
  )
})

test_that("btw_tool_web_read_url_impl() respects max_wait_for_page_load_s option", {
  with_retry({
    skip_if_not_installed("chromote")
    skip_on_cran()

    # Mock to capture the timeout parameter
    timeout_captured <- NULL
    local_mocked_bindings(
      read_url_main_content = function(url, timeout = 10) {
        timeout_captured <<- timeout
        "<html><body>Test content</body></html>"
      }
    )

    # Test with option set
    withr::local_options(btw.max_wait_for_page_load_s = 15)
    btw_tool_web_read_url_impl("https://example.com")
    expect_equal(timeout_captured, 15)
  })
})

test_that("btw_tool_web_read_url_impl() throws if pageload timeout reached", {
  with_retry({
    skip_on_cran()
    skip_if_not_installed("chromote")

    tmp_html <- withr::local_tempfile(
      fileext = ".html",
      lines = '<!DOCTYPE html>
<html>
<head>
    <title>Test Page</title>
    <script>
        const start = Date.now();
        while (Date.now() - start < 1500) {
            // Block for 1.5 seconds
        }
    </script>
</head>
<body>
    <h1>Page Loaded</h1>
</body>
</html>
'
    )

    expect_error(
      btw_tool_web_read_url_impl(
        paste0("file:///", tmp_html),
        max_wait_for_page_load_s = 1
      ),
      "Page.loadEventFired",
      fixed = TRUE
    )
  })
})

test_that("btw_tool_web_read_url_impl() waits for network idle", {
  with_retry({
    skip_on_cran()
    skip_if_not_installed("chromote")

    tmp_html <- withr::local_tempfile(
      fileext = ".html",
      lines = "<!DOCTYPE html>
<html>
<head>
    <title>Test Page</title>
</head>
<body>
    <h1>Page Loaded</h1>
    <script>
        fetch('data:text/plain;base64,' + 'A'.repeat(1000000), {
            method: 'GET'
        });

        const start = Date.now();
        const interval = setInterval(() => {
            fetch('data:text/plain,keepalive');
            if (Date.now() - start >= 4000) {
                clearInterval(interval);
            }
        }, 100);
    </script>
</body>
</html>
"
    )

    expect_warning(
      btw_tool_web_read_url_impl(
        paste0("file:///", tmp_html),
        max_wait_for_page_load_s = 1,
      ),
      "network idle",
      fixed = TRUE
    )
  })
})

test_that("btw_tool_web_read_url_impl() formats output correctly", {
  # Mock read_url_main_content and pandoc_html_simplify
  local_mocked_bindings(
    read_url_main_content = function(...) {
      "<html><body><h1>Test Page</h1><p>Content</p></body></html>"
    },
    pandoc_html_simplify = function(html) {
      c("# Test Page", "", "Content")
    }
  )

  result <- btw_tool_web_read_url_impl("https://example.com/test")

  expect_snapshot(
    cli::cat_line(result@value)
  )
})

test_that("read_url_main_content() works with real chromote", {
  with_retry({
    skip_on_cran()
    skip_if_offline()
    skip_if_not_installed("chromote")

    # Test with a simple, stable URL
    html <- read_url_main_content("https://www.r-project.org/", timeout = 10)

    # TODO: Update read_url_main_content() to better capture r-project.org
    expect_type(html, "character")
    expect_true(nzchar(html))
    # Should contain some HTML-related content
    expect_true(grepl("<(div|span|a|p)", html))
  })
})

test_that("btw_tool_web_read_url() wrapper exists", {
  # Test that the exported wrapper function exists
  expect_true(exists("btw_tool_web_read_url"))
  expect_type(btw_tool_web_read_url, "closure")

  # Check formals include _intent parameter
  expect_true("_intent" %in% names(formals(btw_tool_web_read_url)))
})

test_that("web tool is registered correctly", {
  # Check that the tool is registered in the tools registry
  tools <- btw_tools("web")

  expect_length(tools, 1)
  expect_s3_class(tools[[1]], "ellmer::ToolDef")
  expect_in(map_chr(tools, S7::prop, "name"), "btw_tool_web_read_url")
})

test_that("web tool btw_can_register checks for chromote", {
  tools <- btw_tools("web")
  tool <- tools[[1]]

  # Mock chromote as available
  local_mocked_bindings(
    has_chromote = function() TRUE
  )
  expect_true(tool@annotations$btw_can_register())

  # Mock chromote as unavailable
  local_mocked_bindings(
    has_chromote = function() FALSE
  )
  expect_warning(
    result <- tool@annotations$btw_can_register(),
    "chromote.*required"
  )
  expect_false(result)
})
