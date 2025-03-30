test_that("btw_this.function()", {
  skip_if_not_macos()
  expect_snapshot(cli::cat_line(btw_this(dplyr::mutate)))
})

test_that("btw_this('{pkg}')", {
  # Gets the intro vignette if one is available
  expect_equal(
    btw_this("{dplyr}"),
    btw_tool_docs_vignette("dplyr")
  )

  # Otherwise returns the help index
  expect_equal(
    btw_this("{cli}"),
    btw_tool_docs_package_help_topics("cli")
  )
})

test_that("btw_this.btw_docs_topic()", {
  expect_equal(
    btw_this(?dplyr::mutate),
    btw_this("?dplyr::mutate")
  )
})

test_that("btw_this() handles literal strings", {
  expect_equal(
    as.character(btw_this("letters[3]")),
    "letters[3]"
  )
})

test_that("btw_this('@last_error')", {
  with_mocked_bindings(
    last_error = function() {
      stop(
        "Can't show last error because no error was recorded yet",
        call. = FALSE
      )
    },
    expect_warning(expect_equal(btw_this("@last_error"), btw_ignore()))
  )

  with_mocked_bindings(
    last_error = function() {
      rlang::catch_cnd(abort("That didn't work.", trace = FALSE, call = NULL))
    },
    # btw_this("@last_error")
    expect_snapshot(cat(btw_this("@last_error")))
  )
})

test_that('btw_this("@last_value")', {
  local_mocked_bindings(
    get_last_value = function() mtcars[2:4, ]
  )

  expect_equal(
    btw_this("@last_value"),
    btw_this(mtcars[2:4, ])
  )
})
