test_that("btw_register_tools works", {
  withr::local_envvar(ANTHROPIC_API_KEY = "boop")

  # would need to poke inside of the private env to ensure the tools
  # were actually registered, so just check that there are no conditions raised
  ch <- ellmer::chat_anthropic()
  expect_silent(btw_register_tools(ch))
})
