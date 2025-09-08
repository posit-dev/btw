test_that("btw_task_summarize_chat() throws for invalid input", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep"))

  chat <- ellmer::chat_openai(model = "gpt-4.1-nano")

  expect_error(
    btw_task_summarize_chat(list())
  )

  expect_error(
    btw_task_summarize_chat(chat, additional_guidance = 123)
  )

  expect_error(
    btw_task_summarize_chat(list(), chat, start_turn = -1)
  )

  # Start turn can't be greater than the number of turns
  expect_error(
    btw_task_summarize_chat(list(list()), chat, start_turn = 2)
  )
})

test_that("turns_simplify()", {
  withr::local_envvar(list(OPENAI_API_KEY = "beep"))
  chat <- ellmer::chat_openai(model = "gpt-4.1-nano")

  turns <- list(
    ellmer::Turn(role = "user", contents = "Hello"),
    ellmer::Turn(role = "assistant", contents = "Hi there!"),
    ellmer::Turn(role = "user", contents = "How are you?"),
    ellmer::Turn(
      role = "assistant",
      contents = list(ellmer::ContentToolRequest(id = "tool1", name = "tool_1"))
    ),
    ellmer::Turn(
      role = "assistant",
      contents = list(
        ellmer::ContentToolResult(
          value = "Done",
          request = ellmer::ContentToolRequest(id = "tool1", name = "tool_1")
        )
      )
    )
  )

  expect_equal(turns_simplify(chat), list())
  expect_equal(turns_simplify(turns[1:3]), turns[1:3])

  chat$set_turns(turns[1:3])
  expect_equal(turns_simplify(chat), turns[1:3])

  turns_expected <- turns
  for (i in 4:5) {
    turns_expected[[i]]@contents[[1]] <- ellmer::ContentText(
      format(turns[[i]]@contents[[1]])
    )
  }
  expect_equal(turns_simplify(turns), turns_expected)

  chat$set_turns(turns)
  expect_equal(turns_simplify(chat), turns_expected)
})
