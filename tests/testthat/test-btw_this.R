test_that("btw_this.function()", {
  expect_equal(btw_this('dplyr::mutate'), btw_this(dplyr::mutate))
})

test_that("btw() with package functions", {
  expect_equal(
    format(btw(dplyr::mutate))[-1],
    format(btw('dplyr::mutate'))[-1]
  )
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
