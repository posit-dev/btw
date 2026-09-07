# shinychat 0.5.0 adds chat_server(), page_chat(), chat_attachment(),
# history_options(), and the ConversationStore class. While CRAN ships only
# shinychat 0.4.0, referencing them as shinychat::<name> triggers an R CMD
# check warning ("Missing or unexported objects") because the static analysis
# can't see them in the 0.4.0 namespace. The proxies below resolve the objects
# at runtime through asNamespace(), which static analysis can't follow, and
# check the installed version first so that shinychat 0.4.0 fails with a clear
# message instead of a missing-object error.

shinychat_version_050 <- "0.4.0.9000"

btw_shinychat_version <- function() {
  tryCatch(
    utils::packageVersion("shinychat"),
    error = function(e) numeric_version("0")
  )
}

btw_shinychat_050_object <- function(fn, call = caller_env()) {
  if (btw_shinychat_version() < shinychat_version_050) {
    cli::cli_abort(
      c(
        "{.code shinychat::{fn}} requires shinychat {.strong 0.5.0} or later.",
        "i" = "Installed shinychat is version {btw_shinychat_version()}.",
        "i" = "Install the development version with {.run pak::pak('posit-dev/shinychat')}."
      ),
      call = call
    )
  }

  asNamespace("shinychat")[[fn]]
}

shinychat_chat_server <- function(...) {
  btw_shinychat_050_object("chat_server")(...)
}

shinychat_page_chat <- function(...) {
  btw_shinychat_050_object("page_chat")(...)
}

shinychat_chat_attachment <- function(...) {
  btw_shinychat_050_object("chat_attachment")(...)
}

shinychat_history_options <- function(...) {
  btw_shinychat_050_object("history_options")(...)
}

shinychat_conversation_store <- function() {
  btw_shinychat_050_object("ConversationStore")
}
