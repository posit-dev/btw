test_that("get_environment works", {
  env <- new_environment(list(mtcars = mtcars, boop = "bop"))

  expect_snapshot(cat(get_environment(env)))
  expect_snapshot(cat(get_environment(env, items = "mtcars")))
  expect_snapshot(cat(get_environment(env, items = "boop")))
  expect_snapshot(cat(get_environment(env, items = character(0))))
})
