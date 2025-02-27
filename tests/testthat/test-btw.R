skip_if_not_macos()

test_that("btw works", {
  local_mocked_bindings(interactive = function() FALSE)
  expect_snapshot(btw(mtcars))
})
