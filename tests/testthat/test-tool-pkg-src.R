# Test btw_tool_pkg_src_list_impl ----------------------------------------------

test_that("btw_tool_pkg_src_list_impl returns exported objects by default", {
  result <- btw_tool_pkg_src_list_impl("tools")

  expect_s7_class(result, btw:::BtwToolResult)

  data <- S7::prop(result, "extra")$data
  expect_s3_class(data, "data.frame")
  expect_named(data, c("name", "type", "path", "line"))
  expect_true(nrow(data) > 0)

  exported <- getNamespaceExports(asNamespace("tools"))
  expect_setequal(data$name, exported)
})

test_that("btw_tool_pkg_src_list_impl includes internal objects with all = TRUE", {
  exported <- btw_tool_pkg_src_list_impl("tools", all = FALSE)
  full <- btw_tool_pkg_src_list_impl("tools", all = TRUE)

  n_exported <- nrow(S7::prop(exported, "extra")$data)
  n_full <- nrow(S7::prop(full, "extra")$data)

  expect_true(n_full > n_exported)
})

test_that("btw_tool_pkg_src_list_impl classifies functions and reports missing srcref", {
  result <- btw_tool_pkg_src_list_impl("tools")
  data <- S7::prop(result, "extra")$data

  expect_true(all(data$type %in% c(
    "function",
    "S4generic",
    "S4class",
    "R6generator",
    "data",
    "other"
  )))

  # tools is installed as a binary package without srcref, so paths/lines
  # should be unknown for all objects.
  expect_true(all(is.na(data$path)))
  expect_true(all(is.na(data$line)))
})

test_that("btw_tool_pkg_src_list_impl validates arguments", {
  expect_error(btw_tool_pkg_src_list_impl(123))
  expect_error(btw_tool_pkg_src_list_impl(c("a", "b")))
  expect_error(btw_tool_pkg_src_list_impl("tools", all = "yes"))
})

# Test btw_tool_pkg_src_path_impl ------------------------------------------------

test_that("btw_tool_pkg_src_path_impl reports install path and source availability", {
  result <- btw_tool_pkg_src_path_impl("tools")

  expect_s7_class(result, btw:::BtwToolResult)

  data <- S7::prop(result, "extra")$data
  expect_s3_class(data, "data.frame")
  expect_named(data, c("package", "path", "source_available"))
  expect_equal(nrow(data), 1)
  expect_equal(data$package, "tools")
  expect_true(dir.exists(data$path))
  expect_false(data$source_available)
})

test_that("btw_tool_pkg_src_path_impl handles multiple packages", {
  result <- btw_tool_pkg_src_path_impl(c("tools", "stats"))
  data <- S7::prop(result, "extra")$data

  expect_equal(nrow(data), 2)
  expect_setequal(data$package, c("tools", "stats"))
})

test_that("btw_tool_pkg_src_path_impl validates arguments", {
  expect_error(btw_tool_pkg_src_path_impl(123))
})
