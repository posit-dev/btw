test_that("btw works", {
  local_mocked_bindings(interactive = function() FALSE)
  # have to `print()` as the result is returned invisibly
  expect_snapshot(print(btw(mtcars)))
})
