test_that("btw_tool_evaluate() returns simple calculations", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_evaluate_impl("2 + 2")
  expect_s7_class(res, BtwToolResult)
  expect_type(res@value, "list")
  # The value is captured as text output for visible values
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ellmer::ContentText)
  # Check that the output contains "4"
  expect_match(res@value[[1]]@text, "4")
})

test_that("btw_tool_evaluate() captures messages", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_evaluate_impl('message("hello")')
  expect_s7_class(res, BtwToolResult)
  expect_type(res@value, "list")
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentMessage)
  expect_equal(res@value[[1]]@text, "hello")
})

test_that("btw_tool_evaluate() captures warnings", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_evaluate_impl('warning("beware")')
  expect_s7_class(res, BtwToolResult)
  expect_type(res@value, "list")
  expect_length(res@value, 1)
  expect_s7_class(res@value[[1]], ContentWarning)
  expect_match(res@value[[1]]@text, "beware")
})

test_that("btw_tool_evaluate() captures errors and stops", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_evaluate_impl('x <- 1; stop("error"); y <- 2')
  expect_s7_class(res, BtwToolResult)
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
})

test_that("btw_tool_evaluate() captures plots", {
  skip_if_not_installed("evaluate")

  res <- btw_tool_evaluate_impl('plot(1:10)')
  expect_s7_class(res, BtwToolResult)
  expect_type(res@value, "list")
  # Should have at least one ContentImageInline
  has_plot <- any(vapply(
    res@value,
    function(x) S7::S7_inherits(x, ellmer::ContentImageInline),
    logical(1)
  ))
  expect_true(has_plot)
})

test_that("btw_tool_evaluate() handles multiple outputs", {
  skip_if_not_installed("evaluate")

  code <- '
    message("starting")
    x <- 1:10
    mean(x)
    warning("careful")
  '
  res <- btw_tool_evaluate_impl(code)
  expect_s7_class(res, BtwToolResult)
  expect_type(res@value, "list")
  expect_gte(length(res@value), 3)

  # Check we have message, text output, and warning
  has_message <- any(vapply(res@value, function(x) S7::S7_inherits(x, ContentMessage), logical(1)))
  has_text <- any(vapply(res@value, function(x) S7::S7_inherits(x, ellmer::ContentText), logical(1)))
  has_warning <- any(vapply(res@value, function(x) S7::S7_inherits(x, ContentWarning), logical(1)))

  expect_true(has_message)
  expect_true(has_text)
  expect_true(has_warning)
})

test_that("btw_tool_evaluate() requires string input", {
  skip_if_not_installed("evaluate")

  expect_error(btw_tool_evaluate_impl(123), class = "rlang_error")
  expect_error(btw_tool_evaluate_impl(NULL), class = "rlang_error")
})

test_that("ContentMessage, ContentWarning, ContentError inherit from ContentText", {
  msg <- ContentMessage(text = "hello")
  warn <- ContentWarning(text = "warning")
  err <- ContentError(text = "error")

  expect_s7_class(msg, ellmer::ContentText)
  expect_s7_class(warn, ellmer::ContentText)
  expect_s7_class(err, ellmer::ContentText)

  expect_equal(msg@text, "hello")
  expect_equal(warn@text, "warning")
  expect_equal(err@text, "error")
})
