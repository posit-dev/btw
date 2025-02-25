test_that("btw_this.environment() works", {
  skip_if_not_macos()
  env <- new_environment(list(mtcars = mtcars, boop = "bop"))

  expect_snapshot(cat(btw_this(env)))
  expect_snapshot(cat(btw_this(env, items = "mtcars")))
  expect_snapshot(cat(btw_this(env, items = "boop")))
  expect_snapshot(cat(btw_this(env, items = character(0))))
})

test_that("btw_this.environment() does not support entire namespaces (yet)", {
  expect_error(btw_this(asNamespace("dplyr")))
})
