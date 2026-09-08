test_that("slash command specs cover every btw @ command", {
  specs <- btw_slash_command_specs()

  expect_named(specs)
  expect_true(all(grepl("^btw-", names(specs))))

  # The @ commands handled by dispatch_at_command(); if this fails because a
  # new @ command was added, add a matching /btw-* spec.
  at_commands <- c(
    "current_file",
    "current_selection",
    "clipboard",
    "platform_info",
    "attached_packages",
    "loaded_packages",
    "installed_packages",
    "last_error",
    "last_value",
    "news",
    "cran",
    "url",
    "pkg",
    "help",
    "git",
    "issue",
    "pr"
  )
  expect_setequal(
    vapply(specs, function(spec) spec$at, character(1)),
    at_commands
  )
})

test_that("btw_slash_at_string() rebuilds the @ command", {
  specs <- btw_slash_command_specs()

  expect_identical(
    btw_slash_at_string(specs[["btw-news"]], "dplyr v1.1.4 join_by"),
    "@news dplyr v1.1.4 join_by"
  )
  expect_identical(
    btw_slash_at_string(specs[["btw-news"]], ""),
    "@news"
  )
  expect_identical(
    btw_slash_at_string(specs[["btw-cran-versions"]], "dplyr"),
    "@cran versions dplyr"
  )
  expect_identical(
    btw_slash_at_string(specs[["btw-cran-versions"]], ""),
    "@cran versions"
  )
  expect_identical(
    btw_slash_at_string(specs[["btw-platform-info"]]),
    "@platform_info"
  )
})

test_that("btw_slash_join_command() formats restore text", {
  expect_identical(
    btw_slash_join_command("btw-news", "dplyr"),
    "/btw-news dplyr"
  )
  expect_identical(
    btw_slash_join_command("btw-news", "  dplyr  "),
    "/btw-news dplyr"
  )
  expect_identical(btw_slash_join_command("btw-news"), "/btw-news")
  expect_identical(
    btw_slash_join_command("review-testing", "check my code"),
    "/review-testing check my code"
  )
})

test_that("btw_slash_attachment_name() slugs the restore text", {
  specs <- btw_slash_command_specs()

  expect_identical(
    btw_slash_attachment_name(specs[["btw-news"]], "/btw-news dplyr v1.1.4"),
    "btw-news-dplyr-v1.1.4.md"
  )
  expect_identical(
    btw_slash_attachment_name(
      specs[["btw-platform-info"]],
      "/btw-platform-info"
    ),
    "btw-platform-info.md"
  )
  expect_match(
    btw_slash_attachment_name(
      specs[["btw-url"]],
      paste0("/btw-url https://example.com/", strrep("a/", 80))
    ),
    "^btw-url-[a-z0-9.-]{0,61}\\.md$"
  )
})

test_that("btw_slash_eval_at() evaluates context and errors cleanly", {
  platform <- btw_slash_eval_at("@platform_info")
  expect_type(platform, "character")
  expect_true(length(platform) == 1 && nzchar(platform))

  expect_snapshot(btw_slash_eval_at("@unknown_command"), error = TRUE)
})

test_that("btw_slash_append_context() shows and clears the running toast", {
  skip_if_no_shinychat_v05()

  specs <- btw_slash_command_specs()

  state <- list2env(list(attachments = list(), restored = NULL))
  chat <- list2env(list(
    update_user_input = function(
      value = NULL,
      focus = FALSE,
      attachments = NULL,
      ...
    ) {
      if (!is.null(attachments)) {
        state$attachments <- c(state$attachments, attachments)
      }
      if (!is.null(value)) {
        state$restored <- c(state$restored, value)
      }
    }
  ))

  running <- character()
  cleared <- character()
  local_mocked_bindings(
    btw_slash_toast_running = function(label) {
      running <<- c(running, label)
      "btw_slash_running"
    },
    btw_slash_toast_clear = function(id) {
      cleared <<- c(cleared, id)
    }
  )

  # success: toast shown and cleared, context staged as an attachment
  btw_slash_append_context(
    chat,
    specs[["btw-platform-info"]],
    "@platform_info",
    "/btw-platform-info"
  )
  expect_identical(running, "/btw-platform-info")
  expect_identical(cleared, "btw_slash_running")
  expect_length(state$attachments, 1)
  expect_match(state$attachments[[1]]$name, "^btw-platform-info\\.md$")

  # failure: toast still cleared, input restored, error toast shown
  toasts <- character()
  local_mocked_bindings(
    notifier = function(icon, action, error = NULL, ...) {
      toasts <<- c(toasts, action)
    }
  )
  btw_slash_append_context(
    chat,
    specs[["btw-news"]],
    "@news",
    "/btw-news"
  )
  expect_identical(running, c("/btw-platform-info", "/btw-news"))
  expect_identical(cleared, c("btw_slash_running", "btw_slash_running"))
  expect_identical(state$restored, "/btw-news")
  expect_identical(toasts, "/btw-news")
  expect_length(state$attachments, 1)
})

test_that("running toast helpers no-op outside a session", {
  expect_null(btw_slash_toast_running("x"))
  expect_no_error(btw_slash_toast_clear(NULL))
})

test_that("btw_slash_skill_handler() combines skill text and user input", {
  skip_if_no_shinychat_v05()

  dir <- fs::dir_create(withr::local_tempfile(fileext = "skill"))
  writeLines(
    c(
      "---",
      "name: test-skill",
      "description: A test skill",
      "---",
      "Do the thing."
    ),
    file.path(dir, "SKILL.md")
  )

  submitted <- NULL
  chat <- list2env(list(
    status = function() "idle",
    update_user_input = function(...) NULL,
    client = list(stream = function(content) {
      submitted <<- content
      content
    }),
    append = function(stream) NULL
  ))

  local_mocked_bindings(
    find_skill = function(name) {
      list(
        path = file.path(dir, "SKILL.md"),
        base_dir = dir,
        validation = list(valid = TRUE)
      )
    }
  )

  # the handler sends the skill body (no frontmatter) plus any user text
  expected <- frontmatter::read_front_matter(file.path(dir, "SKILL.md"))$body

  handler <- btw_slash_skill_handler(chat, "test-skill")
  content <- shinychat::ContentSlashCommand(
    text = "placeholder",
    command = "test-skill",
    user_text = "do it now"
  )
  handler(content)

  expect_true(S7::S7_inherits(submitted, shinychat::ContentSlashCommand))
  expect_match(submitted@text, "Do the thing.")
  expect_match(submitted@text, "do it now")
  expect_identical(submitted@text, paste(expected, "do it now", sep = "\n\n"))

  # empty user text: skill text alone
  handler(shinychat::ContentSlashCommand(
    text = "placeholder",
    command = "test-skill",
    user_text = ""
  ))
  expect_identical(submitted@text, expected)

  # failure path: input restored and error surfaced via toast
  restored <- NULL
  chat$update_user_input <- function(value = NULL, focus = FALSE, ...) {
    restored <<- value
  }
  toasts <- character()
  local_mocked_bindings(
    notifier = function(icon, action, error = NULL, ...) {
      toasts <<- c(toasts, action)
    }
  )

  chat$client$stream <- function(content) stop("stream failed")
  handler(shinychat::ContentSlashCommand(
    text = "placeholder",
    command = "test-skill",
    user_text = "oh no"
  ))

  expect_identical(restored, "/test-skill oh no")
  expect_identical(toasts, "/test-skill oh no")
})

test_that("btw_slash_new_chat_handler() starts a new chat via chat$clear()", {
  state <- list2env(list(cleared = 0))
  chat <- list2env(list(
    status = function() "idle",
    clear = function(...) state$cleared <- state$cleared + 1,
    update_user_input = function(...) NULL
  ))

  btw_slash_new_chat_handler(chat, "new")()
  expect_identical(state$cleared, 1)

  btw_slash_new_chat_handler(chat, "clear")()
  expect_identical(state$cleared, 2)

  # streaming: refuse and surface the error like other commands
  restored <- character()
  toasts <- character()
  chat$update_user_input <- function(value = NULL, focus = FALSE, ...) {
    restored <<- c(restored, value)
  }
  local_mocked_bindings(
    notifier = function(icon, action, error = NULL, ...) {
      toasts <<- c(toasts, action)
    }
  )
  chat$status <- function() "streaming"

  btw_slash_new_chat_handler(chat, "new")()
  expect_identical(restored, "/new")
  expect_identical(toasts, "/new")
})

test_that("btw_slash_register_new_chat_commands() registers /new and /clear", {
  registered <- list()
  chat <- list2env(list(
    slash_command = function(
      name,
      description,
      handler,
      ...,
      echo = NULL,
      force = FALSE
    ) {
      registered[[name]] <<- list(description = description, handler = handler)
    }
  ))

  btw_slash_register_new_chat_commands(chat)
  expect_identical(names(registered), c("new", "clear"))
  expect_identical(
    lapply(registered, function(cmd) length(formals(cmd$handler))),
    list(new = 0L, clear = 0L)
  )
  expect_false(isTRUE(registered[["new"]]$echo))
})

test_that("btw_slash_skill_handler() waits for a streaming response", {
  skip_if_no_shinychat_v05()

  streamed <- FALSE
  restored <- NULL
  error_messages <- character()
  chat <- list2env(list(
    status = function() "streaming",
    update_user_input = function(value = NULL, focus = FALSE, ...) {
      restored <<- value
    },
    client = list(stream = function(content) {
      streamed <<- TRUE
      content
    }),
    append = function(stream) NULL
  ))

  local_mocked_bindings(
    notifier = function(icon, action, error = NULL, ...) {
      error_messages <<- c(error_messages, conditionMessage(error))
    }
  )

  handler <- btw_slash_skill_handler(chat, "test-skill")
  handler(shinychat::ContentSlashCommand(
    text = "placeholder",
    command = "test-skill",
    user_text = "do it"
  ))

  expect_false(streamed)
  expect_identical(restored, "/test-skill do it")
  expect_match(error_messages[[1]], "Wait for the current response")
})
