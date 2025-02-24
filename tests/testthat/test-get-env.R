skip_if_not_macos()

test_that("btw_this.environment() works", {
  env <- new_environment(list(mtcars = mtcars, boop = "bop"))

  expect_snapshot(cat(btw_this(env)))
  expect_snapshot(cat(btw_this(env, items = "mtcars")))
  expect_snapshot(cat(btw_this(env, items = "boop")))
  expect_snapshot(cat(btw_this(env, items = character(0))))
})
