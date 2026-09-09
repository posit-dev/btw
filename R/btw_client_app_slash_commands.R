# nocov start

# Slash commands for the shinychat >= 0.5.0 app shell. The /btw-* commands
# mirror the @<command> syntax handled by btw_this.character() and stage the
# context as a text attachment on the chat input (nothing is submitted or
# echoed into the conversation). Skills discovered by btw_skills_list() are
# registered under their own names; submitting one sends the skill's SKILL.md
# text plus the user's message to the model.

btw_slash_command_specs <- function() {
  arg_spec <- function(name, at, description, at_args = NULL) {
    list(
      name = name,
      at = at,
      at_args = at_args,
      takes_args = TRUE,
      description = description
    )
  }

  no_arg_spec <- function(name, at, description) {
    list(
      name = name,
      at = at,
      at_args = NULL,
      takes_args = FALSE,
      description = description
    )
  }

  specs <- list(
    arg_spec(
      "btw-news",
      "news",
      "Include a package's NEWS/release notes, optionally a version or search term. Usage: /btw-news dplyr [v1.1.4] [search term]"
    ),
    arg_spec(
      "btw-pkg",
      "pkg",
      "Include a package's help topics and introductory vignette. Usage: /btw-pkg dplyr"
    ),
    arg_spec(
      "btw-help",
      "help",
      "Include an R help page. Usage: /btw-help dplyr::across (or /btw-help dplyr across)"
    ),
    arg_spec(
      "btw-cran-versions",
      "cran",
      "Include a package's CRAN release versions and dates. Usage: /btw-cran-versions dplyr",
      at_args = "versions"
    ),
    arg_spec(
      "btw-url",
      "url",
      "Include the contents of a web page as markdown. Usage: /btw-url https://example.com"
    ),
    arg_spec(
      "btw-git",
      "git",
      "Include git repository information. Usage: /btw-git status|diff|log [args]"
    ),
    arg_spec(
      "btw-issue",
      "issue",
      "Include a GitHub issue. Usage: /btw-issue #123 (or /btw-issue owner/repo#123)"
    ),
    arg_spec(
      "btw-pr",
      "pr",
      "Include a GitHub pull request. Usage: /btw-pr #123 (or /btw-pr owner/repo#123)"
    ),
    no_arg_spec(
      "btw-current-file",
      "current_file",
      "Include the file currently open in the IDE editor"
    ),
    no_arg_spec(
      "btw-current-selection",
      "current_selection",
      "Include the current text selection from the IDE editor"
    ),
    no_arg_spec(
      "btw-clipboard",
      "clipboard",
      "Include the current contents of the system clipboard"
    ),
    no_arg_spec(
      "btw-platform-info",
      "platform_info",
      "Include R version, OS, and other platform details"
    ),
    no_arg_spec(
      "btw-attached-packages",
      "attached_packages",
      "Include the list of currently attached packages"
    ),
    no_arg_spec(
      "btw-loaded-packages",
      "loaded_packages",
      "Include the list of currently loaded packages"
    ),
    no_arg_spec(
      "btw-installed-packages",
      "installed_packages",
      "Include the list of installed packages"
    ),
    no_arg_spec(
      "btw-last-error",
      "last_error",
      "Include the last error (if any) recorded in the session"
    ),
    no_arg_spec(
      "btw-last-value",
      "last_value",
      "Include .Last.value, the result of the last expression evaluated in the R console"
    )
  )

  stats::setNames(specs, vapply(specs, function(spec) spec$name, character(1)))
}

btw_slash_commands_register <- function(chat, skills = TRUE) {
  if (is.null(chat$slash_command)) {
    return(invisible(chat))
  }

  for (spec in btw_slash_command_specs()) {
    chat$slash_command(
      name = spec$name,
      description = spec$description,
      handler = btw_slash_context_handler(chat, spec),
      echo = FALSE
    )
  }

  btw_slash_register_new_chat_commands(chat)

  if (isTRUE(skills)) {
    btw_skills_register_slash_commands(chat, reserved = c("new", "clear"))
  }

  invisible(chat)
}

# /new and /clear both start a new chat: the current conversation is saved to
# the chat history (when enabled), the chat UI and the client's turns are
# cleared, and the status counters reset. With history disabled they clear the
# conversation without saving. The history coordination is handled by
# shinychat's chat$new_chat() (shinychat#399); the slash handler only resets
# the app-level status counters.
btw_slash_register_new_chat_commands <- function(chat) {
  for (name in c("new", "clear")) {
    chat$slash_command(
      name = name,
      description = paste(
        "Start a new chat, saving the current conversation to the chat history.",
        "Chat history must be enabled for the conversation to be saved."
      ),
      handler = btw_slash_new_chat_handler(chat, name),
      echo = FALSE
    )
  }

  invisible(chat)
}

btw_slash_new_chat_handler <- function(chat, name) {
  function() {
    tryCatch(
      {
        if (identical(chat$status(), "streaming")) {
          cli::cli_abort(
            "Wait for the current response to finish before starting a new chat."
          )
        }

        btw_chat_new_chat(chat)

        session <- shiny::getDefaultReactiveDomain()
        if (!is.null(session)) {
          # reset the token and cost counters in the status bar, the same
          # message the Clear chat button sends from the status_bar module
          session$sendCustomMessage(
            "btw_reset_status",
            list(ns = "status_bar-")
          )
        }
      },
      error = function(e) {
        btw_slash_command_failed(chat, paste0("/", name), e)
      }
    )
  }
}

btw_slash_at_string <- function(spec, user_text = "") {
  parts <- c(spec$at, spec$at_args)
  if (isTRUE(spec$takes_args) && nzchar(trimws(user_text))) {
    parts <- c(parts, trimws(user_text))
  }
  paste0("@", paste(parts, collapse = " "))
}

btw_slash_restore_text <- function(spec, user_text = "") {
  btw_slash_join_command(spec$name, user_text)
}

btw_slash_join_command <- function(name, user_text = "") {
  parts <- c(name, if (nzchar(user_text)) trimws(user_text))
  paste0("/", paste(parts, collapse = " "))
}

btw_slash_context_handler <- function(chat, spec) {
  if (isTRUE(spec$takes_args)) {
    function(content) {
      user_text <- content@user_text %||% ""
      btw_slash_append_context(
        chat,
        spec,
        btw_slash_at_string(spec, user_text),
        restore_text = btw_slash_restore_text(spec, user_text)
      )
    }
  } else {
    function() {
      btw_slash_append_context(
        chat,
        spec,
        btw_slash_at_string(spec),
        restore_text = btw_slash_restore_text(spec)
      )
    }
  }
}

btw_slash_append_context <- function(chat, spec, at_string, restore_text) {
  toast_id <- btw_slash_toast_running(btw_slash_join_command(spec$name))
  on.exit(btw_slash_toast_clear(toast_id), add = TRUE)

  result <- tryCatch(
    btw_slash_eval_at(at_string),
    error = function(e) e
  )

  if (inherits(result, "error")) {
    btw_slash_command_failed(chat, restore_text, result)
    return(invisible())
  }

  path <- tempfile(spec$name, fileext = ".md")
  writeLines(result, path, useBytes = TRUE)

  attachment <- shinychat_chat_attachment(
    path,
    name = btw_slash_attachment_name(spec, restore_text)
  )
  chat$update_user_input(
    attachments = list(attachment),
    attachment_mode = "append",
    focus = TRUE
  )
  unlink(path)

  invisible()
}

btw_slash_eval_at <- function(at_string) {
  result <- btw_this(at_string)

  if (inherits(result, "btw_ignore")) {
    cli::cli_abort("{at_string} did not return anything to include.")
  }
  if (inherits(result, "btw_user_prompt")) {
    cli::cli_abort("Unknown btw command: {at_string}")
  }

  paste(as.character(result), collapse = "\n")
}

btw_slash_toast_running <- function(label) {
  if (is.null(shiny::getDefaultReactiveDomain())) {
    return(NULL)
  }

  bslib_toast <- asNamespace("bslib")[["toast"]]
  bslib_show_toast <- asNamespace("bslib")[["show_toast"]]
  if (is.null(bslib_toast) || is.null(bslib_show_toast)) {
    return(NULL)
  }

  toast <- bslib_toast(
    shiny::span(
      shiny::tags$span(
        class = "spinner-border spinner-border-sm",
        role = "status"
      ),
      shiny::span(
        class = "ms-2",
        "Running",
        shiny::span(
          class = "badge text-bg-secondary font-monospace ms-1",
          label
        )
      )
    ),
    id = "btw_slash_running",
    duration_s = NA,
    closable = FALSE,
    position = "top-right"
  )
  bslib_show_toast(toast)
  "btw_slash_running"
}

btw_slash_toast_clear <- function(id) {
  if (is.null(id)) {
    return(invisible())
  }

  bslib_hide_toast <- asNamespace("bslib")[["hide_toast"]]
  if (!is.null(bslib_hide_toast)) {
    bslib_hide_toast(id)
  }
  invisible()
}

btw_slash_attachment_name <- function(spec, restore_text) {
  # drop the leading "/btw-" and the ".md" suffix comes from the file; keep the
  # command and the user's arguments so the attachment chip is identifiable
  name <- sub("^/", "", restore_text)
  name <- gsub("[^a-zA-Z0-9._-]+", "-", name)
  name <- sub("^-+|-+$", "", name)
  if (nchar(name) > 64) {
    name <- paste0(substring(name, 1, 61), "...")
  }
  paste0(name, ".md")
}

btw_slash_command_failed <- function(chat, restore_text, err) {
  chat$update_user_input(value = restore_text, focus = TRUE)

  message <- cli::ansi_strip(conditionMessage(err))
  err <- simpleError(message, call = conditionCall(err))

  notifier(shiny::icon("triangle-exclamation"), restore_text, error = err)
}

# nocov end
