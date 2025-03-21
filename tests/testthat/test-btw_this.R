test_that("btw_this.function()", {
  skip_if_not_macos()
  expect_snapshot(cli::cat_line(btw_this(dplyr::mutate)))
})

test_that("btw_this('{pkg}')", {
  # Gets the intro vignette if one is available
  expect_equal(
    btw_this("{dplyr}"),
    btw_tool_get_vignette_from_package("dplyr")
  )

  # Otherwise returns the help index
  expect_equal(
    btw_this("{cli}"),
    btw_tool_get_package_help_topics("cli")
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
