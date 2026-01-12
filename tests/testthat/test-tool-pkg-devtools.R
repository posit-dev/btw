# Test btw_tool_pkg_document --------------------------------------------------

test_that("btw_tool_pkg_document constructs correct code", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Documentation updated")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_document_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::document")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, "roclets = NULL")
  expect_match(result@extra$code, "quiet = FALSE")
})

test_that("btw_tool_pkg_document handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Documentation updated")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_document_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
})

test_that("btw_tool_pkg_document rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_document_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_document_impl("../outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_document_impl("/etc/passwd"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_document validates pkg argument", {
  expect_error(btw_tool_pkg_document_impl(123))
  expect_error(btw_tool_pkg_document_impl(c("a", "b")))
  expect_error(btw_tool_pkg_document_impl(NULL))
})

# Test btw_tool_pkg_check ------------------------------------------------------

test_that("btw_tool_pkg_check constructs correct code", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "R CMD check output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_check_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::check")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, "remote = TRUE")
  expect_match(result@extra$code, "cran = TRUE")
  expect_match(result@extra$code, "manual = FALSE")
  expect_match(result@extra$code, "quiet = FALSE")
  expect_match(result@extra$code, 'error_on = "never"')
})

test_that("btw_tool_pkg_check handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "R CMD check output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_check_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
})

test_that("btw_tool_pkg_check rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_check_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_check_impl("../outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_check_impl("/etc/passwd"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_check validates pkg argument", {
  expect_error(btw_tool_pkg_check_impl(123))
  expect_error(btw_tool_pkg_check_impl(c("a", "b")))
  expect_error(btw_tool_pkg_check_impl(NULL))
})

# Test btw_tool_pkg_test -------------------------------------------------------

test_that("btw_tool_pkg_test constructs correct code without filter", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::test")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, "stop_on_failure = FALSE")
  expect_match(result@extra$code, "export_all = TRUE")
  # Should NOT have filter argument
  expect_false(grepl("filter", result@extra$code))
})

test_that("btw_tool_pkg_test constructs correct code with filter", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl(".", filter = "helper")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "devtools::test")
  expect_match(result@extra$code, 'pkg = "."')
  expect_match(result@extra$code, 'filter = "helper"')
  expect_match(result@extra$code, "stop_on_failure = FALSE")
  expect_match(result@extra$code, "export_all = TRUE")
})

test_that("btw_tool_pkg_test handles different filter patterns", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  # Test with regex pattern
  result1 <- btw_tool_pkg_test_impl(".", filter = "tool-.*")
  expect_match(result1@extra$code, 'filter = "tool-.*"')

  # Test with simple string
  result2 <- btw_tool_pkg_test_impl(".", filter = "pkg")
  expect_match(result2@extra$code, 'filter = "pkg"')
})

test_that("btw_tool_pkg_test handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
})

test_that("btw_tool_pkg_test handles subdirectory paths with filter", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl("subdir/pkg", filter = "helper")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, 'pkg = "subdir/pkg"')
  expect_match(result@extra$code, 'filter = "helper"')
})

test_that("btw_tool_pkg_test rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_test_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_test_impl("../outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_test_impl("/etc/passwd"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_test validates pkg argument", {
  expect_error(btw_tool_pkg_test_impl(123))
  expect_error(btw_tool_pkg_test_impl(c("a", "b")))
  expect_error(btw_tool_pkg_test_impl(NULL))
})

test_that("btw_tool_pkg_test validates filter argument", {
  expect_error(btw_tool_pkg_test_impl(".", filter = 123))
  expect_error(btw_tool_pkg_test_impl(".", filter = c("a", "b")))
  # NULL should be allowed (no filter)
  expect_no_error({
    local_mocked_bindings(
      btw_tool_run_r_impl = function(code) {
        BtwRunToolResult(
          value = list(ContentOutput(text = "Test output")),
          extra = list(
            code = code,
            status = "success",
            data = NULL,
            contents = list()
          )
        )
      }
    )
    btw_tool_pkg_test_impl(".", filter = NULL)
  })
})

# Test code construction with special characters -------------------------------

test_that("btw_tool_pkg_check handles paths with spaces correctly", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    },
    check_path_within_current_wd = function(path) invisible(path)
  )

  result <- btw_tool_pkg_check_impl("path with spaces")
  expect_s7_class(result, BtwRunToolResult)
  # deparse() should properly quote the path
  expect_match(result@extra$code, 'pkg = "path with spaces"')
})

test_that("btw_tool_pkg_test handles filter with special regex chars", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Test output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_test_impl(".", filter = "test-.*\\.R$")
  expect_s7_class(result, BtwRunToolResult)
  # deparse() should properly quote the regex
  expect_true(grepl(
    'filter = \"test-.*\\.R$\"',
    result@extra$code,
    fixed = TRUE
  ))
})

# Test return values -----------------------------------------------------------

test_that("all pkg tools return BtwRunToolResult objects", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "output")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result1 <- btw_tool_pkg_document_impl(".")
  expect_s7_class(result1, BtwRunToolResult)
  expect_type(result1@value, "list")
  expect_equal(result1@extra$status, "success")

  result2 <- btw_tool_pkg_check_impl(".")
  expect_s7_class(result2, BtwRunToolResult)
  expect_type(result2@value, "list")
  expect_equal(result2@extra$status, "success")

  result3 <- btw_tool_pkg_test_impl(".")
  expect_s7_class(result3, BtwRunToolResult)
  expect_type(result3@value, "list")
  expect_equal(result3@extra$status, "success")
})

test_that("pkg tools pass through btw_tool_run_r_impl status", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code) {
      BtwRunToolResult(
        value = list(ContentError(text = "Error message")),
        extra = list(
          code = code,
          status = "error",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result1 <- btw_tool_pkg_document_impl(".")
  expect_equal(result1@extra$status, "error")

  result2 <- btw_tool_pkg_check_impl(".")
  expect_equal(result2@extra$status, "error")

  result3 <- btw_tool_pkg_test_impl(".")
  expect_equal(result3@extra$status, "error")
})

# Test btw_tool_pkg_load_all ---------------------------------------------------

test_that("btw_tool_pkg_load_all constructs correct code", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code, show_last_value) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Package loaded successfully!")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_load_all_impl(".")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "btw:::btw_pkg_load_all")
  expect_match(result@extra$code, "pkg = '.'")
})

test_that("btw_tool_pkg_load_all handles subdirectory paths", {
  local_mocked_bindings(
    btw_tool_run_r_impl = function(code, show_last_value) {
      BtwRunToolResult(
        value = list(ContentOutput(text = "Package loaded successfully!")),
        extra = list(
          code = code,
          status = "success",
          data = NULL,
          contents = list()
        )
      )
    }
  )

  result <- btw_tool_pkg_load_all_impl("subdir/pkg")
  expect_s7_class(result, BtwRunToolResult)
  expect_match(result@extra$code, "pkg = 'subdir/pkg'")
})

test_that("btw_tool_pkg_load_all rejects paths outside current directory", {
  expect_error(
    btw_tool_pkg_load_all_impl("/tmp/outside"),
    "not allowed to list or read"
  )
  expect_error(
    btw_tool_pkg_load_all_impl("../outside"),
    "not allowed to list or read"
  )
})

test_that("btw_tool_pkg_load_all validates pkg argument", {
  expect_error(btw_tool_pkg_load_all_impl(123))
  expect_error(btw_tool_pkg_load_all_impl(c("a", "b")))
  expect_error(btw_tool_pkg_load_all_impl(NULL))
})
