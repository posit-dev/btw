test_that("write_to_clipboard() works", {
  withr::local_envvar(list(CLIPR_ALLOW = FALSE))
  withr::local_options(list(rlang_interactive = TRUE))

  # When clipboard is not available, we echo the prompt context
  expect_snapshot(
    btw("Interactive call but clipboard not available")
  )

  # Unless, clipboard is FALSE
  expect_silent(
    expect_equal(
      format(btw("Hello world", clipboard = FALSE)),
      "## User\nHello world"
    )
  )
})
