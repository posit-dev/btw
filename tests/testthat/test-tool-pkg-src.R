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

  expect_true(all(
    data$type %in%
      c(
        "function",
        "S4generic",
        "S4class",
        "R6generator",
        "data",
        "other"
      )
  ))

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

test_that("btw_pkg_src_describe degrades objects that error when forced", {
  ns <- new.env()
  assign("ok", function() 1, ns)
  makeActiveBinding("boom", function() stop("cannot force me"), ns)

  ok <- btw:::btw_pkg_src_describe("ok", ns)
  expect_equal(ok$type, "function")

  # A binding that errors on force degrades to an `other` row rather than
  # aborting.
  boom <- btw:::btw_pkg_src_describe("boom", ns)
  expect_equal(boom$type, "other")
  expect_true(is.na(boom$path))
  expect_true(is.na(boom$line))
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
  expect_error(btw_tool_pkg_src_path_impl(character()))
})

# Test btw_tool_pkg_src_get_impl --------------------------------------------

test_that("btw_tool_pkg_src_get_impl returns deparsed source for functions without srcref", {
  result <- btw_tool_pkg_src_get_impl("tools", "toRd")

  expect_s7_class(result, btw:::BtwToolResult)

  value <- S7::prop(result, "value")
  expect_match(value, "toRd", fixed = TRUE)
  expect_match(value, "```r", fixed = TRUE)

  data <- S7::prop(result, "extra")$data
  expect_s3_class(data, "data.frame")
  expect_equal(nrow(data), 1)
  expect_equal(data$type, "function")
  expect_true(nzchar(data$source))
})

test_that("btw_tool_pkg_src_get_impl reaches internal (non-exported) objects", {
  exported <- getNamespaceExports(asNamespace("tools"))
  expect_false(".arg_names_from_call" %in% exported)

  result <- btw_tool_pkg_src_get_impl("tools", ".arg_names_from_call")
  data <- S7::prop(result, "extra")$data

  expect_equal(data$name, ".arg_names_from_call")
  expect_equal(data$type, "function")
  expect_true(nzchar(data$source))
})

test_that("btw_tool_pkg_src_get_impl summarizes non-function objects with str()", {
  result <- btw_tool_pkg_src_get_impl("tools", "IANA_URI_scheme_db")
  data <- S7::prop(result, "extra")$data

  expect_equal(data$type, "data")
  expect_match(data$source, "data.frame", fixed = TRUE)
  # Should be a short structure summary, not a full data dump.
  expect_true(nrow(data) == 1)
  expect_lt(length(strsplit(data$source, "\n")[[1]]), 20)
})

test_that("btw_tool_pkg_src_get_impl reports missing objects without erroring", {
  result <- btw_tool_pkg_src_get_impl("tools", "no_such_object_xyz")

  value <- S7::prop(result, "value")
  expect_match(value, "not found", ignore.case = TRUE)

  data <- S7::prop(result, "extra")$data
  expect_true(is.na(data$type))
  expect_match(data$source, "not found", ignore.case = TRUE)
})

test_that("btw_tool_pkg_src_get_impl handles multiple objects, including a mix of found/missing", {
  result <- btw_tool_pkg_src_get_impl(
    "tools",
    c("toRd", "IANA_URI_scheme_db", "no_such_object_xyz")
  )
  data <- S7::prop(result, "extra")$data

  expect_equal(nrow(data), 3)
  expect_equal(data$name, c("toRd", "IANA_URI_scheme_db", "no_such_object_xyz"))
})

test_that("btw_tool_pkg_src_get_impl validates arguments", {
  expect_error(btw_tool_pkg_src_get_impl(123, "toRd"))
  expect_error(btw_tool_pkg_src_get_impl("tools", character()))
})

# Test btw_pkg_src_materialize_dir -------------------------------------------

test_that("btw_pkg_src_materialize_dir writes deparsed sources to a temp dir", {
  dir <- btw:::btw_pkg_src_materialize_dir("tools")

  expect_true(dir.exists(dir))
  files <- list.files(dir, pattern = "\\.R$")
  expect_true(length(files) > 0)
  expect_true("toRd.R" %in% files)

  content <- readLines(file.path(dir, "toRd.R"))
  expect_true(any(grepl("function", content, fixed = TRUE)))
})

# Test btw_tool_pkg_src_search_impl ------------------------------------------

test_that("btw_tool_pkg_src_search_impl validates arguments", {
  expect_error(btw_tool_pkg_src_search_impl(123, "file.path"))
  expect_error(btw_tool_pkg_src_search_impl("tools", character()))
})

test_that("btw_tool_pkg_src_search_impl searches materialized source for binary-installed packages", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  withr::local_envvar(TESTTHAT = NA)

  result <- btw_tool_pkg_src_search_impl("tools", "file.path")

  expect_s7_class(result, btw:::BtwToolResult)

  data <- S7::prop(result, "extra")$data
  expect_s3_class(data, "data.frame")
  expect_named(
    data,
    c("filename", "size", "last_modified", "content", "line", "term")
  )
  expect_true(nrow(data) > 0)
  expect_true(all(data$term == "file.path"))
  expect_true(all(grepl("file.path", data$content, fixed = TRUE)))
})

test_that("btw_tool_pkg_src_search_impl combines results across multiple terms", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  withr::local_envvar(TESTTHAT = NA)

  result <- btw_tool_pkg_src_search_impl("tools", c("file.path", "toRd"))
  data <- S7::prop(result, "extra")$data

  expect_setequal(unique(data$term), c("file.path", "toRd"))
})
