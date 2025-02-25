test_that("btw_this.function()", {
  expect_equal(btw_this('dplyr::mutate'), btw_this(dplyr::mutate))
})

test_that("btw() with package functions", {
  expect_output(
    expect_equal(btw(dplyr::mutate)[-1], btw('dplyr::mutate')[-1])
  )
})

test_that("btw_this.btw_docs_package()", {
  expect_equal(
    btw_this("{dplyr}"),
    btw_this(as_btw_docs_package("dplyr"))
  )
})

test_that("btw_this.btw_docs_topic()", {
  expect_equal(
    btw_this(?dplyr::mutate),
    btw_this("?dplyr::mutate")
  )
})
