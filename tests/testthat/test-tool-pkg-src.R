# Test btw_tool_pkg_src_list_impl ----------------------------------------------

test_that("btw_tool_pkg_src_list_impl returns exported objects by default", {
  result <- btw_tool_pkg_src_list_impl("tools")

  expect_s7_class(result, btw:::BtwToolResult)

  data <- S7::prop(result, "extra")$data
  expect_s3_class(data, "data.frame")
  # tools is a binary install with no srcref, so the all-NA path/line columns
  # are dropped, leaving name/type.
  expect_named(data, c("name", "type"))
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

  # tools is installed as a binary package without srcref, so the path/line
  # columns are unknown for every object and dropped entirely.
  expect_false("path" %in% names(data))
  expect_false("line" %in% names(data))
})

test_that("btw_pkg_src_drop_na_columns drops only fully-NA columns", {
  df <- data.frame(
    name = c("a", "b"),
    type = c("function", "data"),
    path = c(NA_character_, NA_character_),
    line = c(NA_integer_, 12L),
    stringsAsFactors = FALSE
  )

  dropped <- btw:::btw_pkg_src_drop_na_columns(df)
  # `path` is fully NA and dropped; `line` is partially populated and kept.
  expect_named(dropped, c("name", "type", "line"))

  # An empty frame keeps its full schema.
  empty <- df[0, ]
  expect_named(btw:::btw_pkg_src_drop_na_columns(empty), names(df))
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

# Test btw_tool_pkg_src_methods_impl -----------------------------------------

test_that("btw_tool_pkg_src_methods_impl enumerates S3 methods of a generic", {
  result <- btw_tool_pkg_src_methods_impl("stats", "predict")

  expect_s7_class(result, btw:::BtwToolResult)

  data <- S7::prop(result, "extra")$data
  expect_s3_class(data, "data.frame")
  expect_true(all(c("generic", "method", "class", "type") %in% names(data)))
  expect_true(nrow(data) > 0)
  expect_true(all(data$generic == "predict"))
  expect_true(all(data$type == "S3method"))
  expect_false("source" %in% names(data))

  # `method` is the get-able object name; `class` is the dispatch class.
  expect_true("predict.lm" %in% data$method)
  expect_equal(data$class[data$method == "predict.lm"], "lm")

  # The listed method name feeds straight back into `get`.
  got <- btw_tool_pkg_src_get_impl("stats", "predict.lm")
  expect_equal(S7::prop(got, "extra")$data$type, "function")
})

test_that("btw_tool_pkg_src_methods_impl enumerates S4 methods with signatures", {
  result <- btw_tool_pkg_src_methods_impl("stats4", "coef")
  data <- S7::prop(result, "extra")$data

  expect_true(any(data$type == "S4method"))
  # S4 methods carry the signature in `class` and are not get-able by name.
  expect_true("mle" %in% data$class)
  expect_true(all(is.na(data$method[data$type == "S4method"])))
})

test_that("btw_tool_pkg_src_methods_impl finds external-generic S3 registrations", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("vctrs")

  result <- btw_tool_pkg_src_methods_impl("tibble", "vec_ptype_abbr")
  data <- S7::prop(result, "extra")$data

  row <- data[data$method == "vec_ptype_abbr.tbl_df", , drop = FALSE]
  expect_equal(nrow(row), 1)
  expect_equal(row$class, "tbl_df")
  expect_equal(row$type, "S3method")

  got <- btw_tool_pkg_src_get_impl("tibble", row$method)
  expect_equal(S7::prop(got, "extra")$data$type, "function")
})

test_that("btw_tool_pkg_src_methods_impl handles function-valued S3 registrations", {
  skip_if_not_installed("vctrs")
  skip_if_not_installed("fs")

  loadNamespace("fs")

  result <- btw_tool_pkg_src_methods_impl("vctrs", "vec_ptype2")
  data <- S7::prop(result, "extra")$data

  expect_true(nrow(data) > 0)
  expect_false("fs_path.fs_path" %in% data$class)
  expect_true("vec_ptype2.AsIs" %in% data$method)

  with_source <- btw_tool_pkg_src_methods_impl(
    "vctrs",
    "vec_ptype2",
    source = TRUE
  )
  source_data <- S7::prop(with_source, "extra")$data
  source_row <- source_data[
    !is.na(source_data$method) &
      source_data$method == "vec_ptype2.AsIs",
    ,
    drop = FALSE
  ]

  expect_equal(nrow(source_row), 1)
  expect_match(source_row$source, "function", fixed = TRUE)
})

test_that("btw_tool_pkg_src_methods_impl renders S3 and S4 method source", {
  skip_if_not_installed("tibble")
  skip_if_not_installed("vctrs")

  s3 <- btw_tool_pkg_src_methods_impl(
    "tibble",
    "vec_ptype_abbr",
    source = TRUE
  )
  s3_data <- S7::prop(s3, "extra")$data
  s3_row <- s3_data[s3_data$method == "vec_ptype_abbr.tbl_df", , drop = FALSE]

  expect_true("source" %in% names(s3_data))
  expect_match(s3_row$source, "function", fixed = TRUE)
  expect_match(S7::prop(s3, "value"), "```r", fixed = TRUE)

  s4 <- btw_tool_pkg_src_methods_impl("stats4", "coef", source = TRUE)
  s4_data <- S7::prop(s4, "extra")$data
  s4_row <- s4_data[s4_data$class == "mle", , drop = FALSE]

  expect_true("source" %in% names(s4_data))
  expect_match(s4_row$source, "function", fixed = TRUE)
  expect_match(S7::prop(s4, "value"), "```r", fixed = TRUE)
})

test_that("btw_tool_pkg_src_methods_impl handles multiple generics", {
  result <- btw_tool_pkg_src_methods_impl("stats", c("predict", "residuals"))
  data <- S7::prop(result, "extra")$data

  expect_setequal(unique(data$generic), c("predict", "residuals"))
})

test_that("btw_tool_pkg_src_methods_impl reports nothing for a non-generic", {
  default <- btw_tool_pkg_src_methods_impl("stats", "no_such_generic_xyz")

  expect_match(S7::prop(default, "value"), "No methods found", fixed = TRUE)
  default_data <- S7::prop(default, "extra")$data
  expect_equal(nrow(default_data), 0)
  expect_false("source" %in% names(default_data))

  with_source <- btw_tool_pkg_src_methods_impl(
    "stats",
    "no_such_generic_xyz",
    source = TRUE
  )

  expect_match(
    S7::prop(with_source, "value"),
    "No methods found.",
    fixed = TRUE
  )
  source_data <- S7::prop(with_source, "extra")$data
  expect_equal(nrow(source_data), 0)
  expect_named(
    source_data,
    c("generic", "method", "class", "type", "path", "line", "source")
  )
})

test_that("btw_tool_pkg_src_methods_impl validates arguments", {
  expect_error(btw_tool_pkg_src_methods_impl(123, "predict"))
  expect_error(btw_tool_pkg_src_methods_impl("stats", character()))
  expect_error(btw_tool_pkg_src_methods_impl("stats", "predict", source = 1))
})

# Test btw_pkg_src_materialize_dir -------------------------------------------

test_that("btw_pkg_src_materialize_dir writes deparsed sources to a temp dir", {
  ns <- btw:::btw_pkg_src_resolve_ns("tools")$ns
  materialized <- btw:::btw_pkg_src_materialize_dir(ns)

  expect_true(dir.exists(materialized$dir))
  files <- list.files(materialized$dir, pattern = "\\.R$")
  expect_true(length(files) > 0)
  expect_true(all(files %in% materialized$mapping$filename))

  to_rd <- materialized$mapping[
    materialized$mapping$name == "toRd",
    "filename"
  ]
  expect_length(to_rd, 1)
  content <- readLines(file.path(materialized$dir, to_rd))
  expect_true(any(grepl("function", content, fixed = TRUE)))
})

test_that("btw_pkg_src_materialize_dir preserves colliding object names", {
  ns <- new.env(parent = emptyenv())
  assign("[<-.vctrs_vctr", function(x, value) x, ns)
  assign("*.vctrs_vctr", function(e1, e2) e1, ns)
  assign("/.vctrs_vctr", function(e1, e2) e1, ns)

  materialized <- btw:::btw_pkg_src_materialize_dir(ns)
  mapping <- materialized$mapping

  expect_setequal(
    mapping$name,
    c("[<-.vctrs_vctr", "*.vctrs_vctr", "/.vctrs_vctr")
  )
  expect_equal(length(unique(mapping$filename)), nrow(mapping))
  expect_true(all(file.exists(file.path(materialized$dir, mapping$filename))))
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

  # Materialized source lives in a temp dir that is gone by now; results
  # surface the bare object name, not a transient (and unreadable) temp path.
  expect_false(any(grepl(.Platform$file.sep, data$filename, fixed = TRUE)))
  expect_false(any(grepl("\\.R$", data$filename)))
  ns <- btw:::btw_pkg_src_resolve_ns("tools")$ns
  expect_true(all(vapply(
    data$filename,
    exists,
    logical(1),
    envir = ns,
    inherits = FALSE
  )))
})

test_that("btw_tool_pkg_src_search_impl preserves exact operator names", {
  skip_if_not_installed("vctrs")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  withr::local_envvar(TESTTHAT = NA)

  result <- btw_tool_pkg_src_search_impl("vctrs", "vec_cast(value, x)")
  data <- S7::prop(result, "extra")$data

  expect_true("[<-.vctrs_vctr" %in% data$filename)

  got <- btw_tool_pkg_src_get_impl("vctrs", "[<-.vctrs_vctr")
  got_data <- S7::prop(got, "extra")$data
  expect_equal(got_data$name, "[<-.vctrs_vctr")
  expect_equal(got_data$type, "function")
})

test_that("btw_tool_pkg_src_search_impl combines results across multiple terms", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  withr::local_envvar(TESTTHAT = NA)

  result <- btw_tool_pkg_src_search_impl("tools", c("file.path", "toRd"))
  data <- S7::prop(result, "extra")$data

  expect_setequal(unique(data$term), c("file.path", "toRd"))
})
