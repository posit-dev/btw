test_that("btw_tool_run_r() returns simple calculations", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_run_r_impl("2 + 2")
  expect_s7_class(res, BtwRunToolResult)
  expect_type(res@value, "list")
  # The actual value is stored in extra$data
  expect_equal(res@extra$data, 4)
  # The visible output is captured as ContentOutput
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentOutput)
  expect_match(res@value[[1]]@text, "4")
  # The contents in extra should match value (except source blocks)
  expect_equal(
    res@value,
    keep(res@extra$contents, Negate(S7::S7_inherits), ContentSource)
  )
})

test_that("btw_tool_run_r() captures messages", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_run_r_impl('message("hello")')
  expect_s7_class(res, BtwRunToolResult)
  expect_type(res@value, "list")
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentMessage)
  expect_equal(res@value[[1]]@text, "hello")
})

test_that("btw_tool_run_r() captures warnings", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_run_r_impl('warning("beware")')
  expect_s7_class(res, BtwRunToolResult)
  expect_type(res@value, "list")
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentWarning)
  expect_match(res@value[[1]]@text, "beware")
})

test_that("btw_tool_run_r() captures errors and stops", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_run_r_impl('x <- 1; stop("error"); y <- 2')
  expect_s7_class(res, BtwRunToolResult)
  expect_type(res@value, "list")
  # Should have the error content
  has_error <- any(vapply(
    res@value,
    function(x) S7::S7_inherits(x, ContentError),
    logical(1)
  ))
  expect_true(has_error)
  # y should not be assigned (code stopped at error)
  expect_false(exists("y", envir = globalenv()))
  # Error should be set on result
  expect_equal(res@extra$status, "error")
})

test_that("btw_tool_run_r() captures plots", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_run_r_impl('plot(1:10)')
  expect_s7_class(res, BtwRunToolResult)
  expect_type(res@value, "list")
  has_plot <- any(vapply(
    res@value,
    function(x) S7::S7_inherits(x, ellmer::ContentImage),
    logical(1)
  ))
  expect_true(has_plot)
})

test_that("btw_tool_run_r() handles multiple outputs", {
  skip_if_not_installed("evaluate")

  code <- '
    message("starting")
    x <- 1:10
    mean(x)
    warning("careful")
  '
  res <- btw_tool_run_r_impl(code)
  expect_s7_class(res, BtwRunToolResult)
  expect_type(res@value, "list")
  expect_gte(length(res@value), 3)

  # Check we have message, code output, and warning
  has_message <- any(vapply(
    res@value,
    function(x) S7::S7_inherits(x, ContentMessage),
    logical(1)
  ))
  has_code <- any(vapply(
    res@value,
    function(x) S7::S7_inherits(x, ContentOutput),
    logical(1)
  ))
  has_warning <- any(vapply(
    res@value,
    function(x) S7::S7_inherits(x, ContentWarning),
    logical(1)
  ))

  expect_true(has_message)
  expect_true(has_code)
  expect_true(has_warning)
})

test_that("btw_tool_run_r() requires string input", {
  skip_if_not_installed("evaluate")

  expect_error(btw_tool_run_r_impl(123), class = "rlang_error")
  expect_error(btw_tool_run_r_impl(NULL), class = "rlang_error")
})

test_that("ContentOutput, ContentMessage, ContentWarning, ContentError inherit from ContentText", {
  code <- ContentOutput(text = "output")
  msg <- ContentMessage(text = "hello")
  warn <- ContentWarning(text = "warning")
  err <- ContentError(text = "error")

  expect_s7_class(code, ellmer::ContentText)
  expect_s7_class(msg, ellmer::ContentText)
  expect_s7_class(warn, ellmer::ContentText)
  expect_s7_class(err, ellmer::ContentText)

  expect_equal(code@text, "output")
  expect_equal(msg@text, "hello")
  expect_equal(warn@text, "warning")
  expect_equal(err@text, "error")
})

test_that("contents_html() renders Content types correctly", {
  code <- ContentOutput(text = "[1] 42")
  msg <- ContentMessage(text = "info message")
  warn <- ContentWarning(text = "warning message")
  err <- ContentError(text = "error message")

  code_html <- ellmer::contents_html(code)
  msg_html <- ellmer::contents_html(msg)
  warn_html <- ellmer::contents_html(warn)
  err_html <- ellmer::contents_html(err)

  expect_match(code_html, 'code class="nohighlight"')
  expect_match(code_html, 'pre class="btw-output-output"')
  expect_match(msg_html, 'class="btw-output-message"')
  expect_match(warn_html, 'class="btw-output-warning"')
  expect_match(err_html, 'class="btw-output-error"')
})

test_that("adjacent content of same type is merged", {
  skip_if_not_installed("evaluate")

  # Multiple messages should be merged
  res <- btw_tool_run_r_impl('message("a"); message("b")')
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentMessage)
  expect_match(res@value[[1]]@text, "a\nb")

  # Multiple code outputs should be merged
  res <- btw_tool_run_r_impl('1 + 1; 2 + 2')
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentOutput)

  # Different types should not be merged
  res <- btw_tool_run_r_impl('message("a"); 1 + 1; warning("b")')
  expect_length(res@value, 3)
  expect_s7_class(res@value[[1]], ContentMessage)
  expect_s7_class(res@value[[2]], ContentOutput)
  expect_s7_class(res@value[[3]], ContentWarning)
})

test_that("intermediate plots are dropped", {
  skip_if_not_installed("evaluate")

  code <- "
plot(1:3)
text(1, 1, 'x')
text(1, 1, 'y')"

  res <- btw_tool_run_r_impl(code)
  expect_s7_class(res, BtwRunToolResult)

  expect_type(res@value, "list")
  plot_contents <- keep(res@value, S7::S7_inherits, ellmer::ContentImage)
  expect_length(plot_contents, 1)

  expect_type(res@extra$contents, "list")
  plot_contents_all <- keep(
    res@extra$contents,
    S7::S7_inherits,
    ellmer::ContentImage
  )
  expect_length(plot_contents_all, 1)
})

test_that("btw_tool_run_r() is not included in btw_tools() by default", {
  local_mocked_bindings(
    is_installed = function(...) TRUE,
    btw_can_register_gh_tool = function() FALSE
  )
  withr::local_envvar(BTW_RUN_R_ENABLED = NULL)
  withr::local_options(btw.run_r.enabled = NULL)

  tools <- btw_tools()
  tool_names <- map_chr(tools, function(x) x@name)
  expect_false("btw_tool_run_r" %in% tool_names)
})

test_that("btw_tool_run_r() is included in btw_tools() when requested", {
  local_mocked_bindings(is_installed = function(...) TRUE)
  withr::local_envvar(BTW_RUN_R_ENABLED = NULL)
  withr::local_options(btw.run_r.enabled = NULL)

  tools <- btw_tools("run")
  tool_names <- map_chr(tools, function(x) x@name)
  expect_true("btw_tool_run_r" %in% tool_names)

  tools <- btw_tools("btw_tool_run_r")
  tool_names <- map_chr(tools, function(x) x@name)
  expect_true("btw_tool_run_r" %in% tool_names)
})

describe("btw_tool_run_r() in btw_tools()", {
  local_mocked_bindings(is_installed = function(...) TRUE)

  it("can be enabled via option", {
    withr::local_options(btw.run_r.enabled = TRUE)
    tools <- btw_tools()
    tool_names <- map_chr(tools, function(x) x@name)
    expect_true("btw_tool_run_r" %in% tool_names)
  })

  it("can be enabled via environment variable", {
    withr::local_envvar(BTW_RUN_R_ENABLED = "TRUE")
    tools <- btw_tools()
    expect_true("btw_tool_run_r" %in% names(tools))
  })

  it("can be enabled via btw.md", {
    path_btw <- withr::local_tempfile(
      lines = c(
        "---",
        "options:",
        "  run_r:",
        "    enabled: true",
        "---"
      )
    )

    withr::local_envvar(ANTHROPIC_API_KEY = "boop")
    client <- btw_client(path_btw = path_btw)

    tools <- client$get_tools()
    expect_true("btw_tool_run_r" %in% names(tools))
  })

  it("is not included if explicitly disabled", {
    path_btw <- withr::local_tempfile(
      lines = c(
        "---",
        "tools: ['run']",
        "options:",
        "  run_r:",
        "    enabled: false",
        "---"
      )
    )

    withr::local_envvar(ANTHROPIC_API_KEY = "boop")
    client <- btw_client(path_btw = path_btw)

    tools <- client$get_tools()
    expect_false("btw_tool_run_r" %in% names(tools))
  })

  it("is included if explicitly mentioned", {
    path_btw <- withr::local_tempfile(
      lines = c(
        "---",
        "tools: ['run']",
        "---"
      )
    )

    withr::local_envvar(ANTHROPIC_API_KEY = "boop")
    client <- btw_client(path_btw = path_btw)

    tools <- client$get_tools()
    expect_true("btw_tool_run_r" %in% names(tools))
  })

  it("is not included if explicitly mentioned but disabled", {
    path_btw <- withr::local_tempfile(
      lines = c(
        "---",
        "tools: ['run']",
        "---"
      )
    )

    withr::local_envvar(BTW_RUN_R_ENABLED = "false")
    withr::local_envvar(ANTHROPIC_API_KEY = "boop")
    client <- btw_client(path_btw = path_btw)

    tools <- client$get_tools()
    expect_false("btw_tool_run_r" %in% names(tools))
  })

  it("is included if mentioned and enabled, even if globally disabled", {
    path_btw <- withr::local_tempfile(
      lines = c(
        "---",
        "tools: ['run']",
        "options:",
        "  run_r:",
        "    enabled: true",
        "---"
      )
    )

    withr::local_options(btw.run_r.enabled = FALSE)
    withr::local_envvar(ANTHROPIC_API_KEY = "boop")
    client <- btw_client(path_btw = path_btw)

    expect_equal(getOption("btw.run_r.enabled"), FALSE)

    tools <- client$get_tools()
    expect_true("btw_tool_run_r" %in% names(tools))
  })
})


test_that("parse_ratio correctly parses 'w:h' strings", {
  expect_equal(parse_ratio("16:9"), 16 / 9)
  expect_equal(parse_ratio("5:9"), 5 / 9)
})

test_that("parse_ratio accepts numeric ratios", {
  expect_equal(parse_ratio(16 / 9), 16 / 9)
})

test_that("btw_run_r_plot_dimensions computes correct dimensions for landscape ratio", {
  dims <- btw_run_r_plot_dimensions("16:9")
  exp_width <- 768L
  exp_height <- as.integer(round(768 / (16 / 9)))

  expect_equal(dims$width, !!exp_width)
  expect_equal(dims$height, !!exp_height)
  expect_equal(max(unlist(dims)), 768L)
})

test_that("btw_run_r_plot_dimensions computes correct dimensions for portrait ratio", {
  dims <- btw_run_r_plot_dimensions("5:9")
  expect_equal(dims$height, 768L)
  expect_equal(dims$width, as.integer(round(768 * (5 / 9))))
  expect_equal(max(unlist(dims)), 768L)
})

test_that("btw_run_r_plot_dimensions works with numeric ratio input", {
  dims <- btw_run_r_plot_dimensions(16 / 9)
  expect_equal(dims$width, 768L)
  expect_equal(dims$height, as.integer(round(768 / (16 / 9))))
})
