# nocov start

# btw_app shell for shinychat 0.4.0: tool selection lives in a bslib sidebar.
# Delete this file (and the sidebar toggle chunk in btw_app.js) when btw
# requires shinychat >= 0.5.0.

btw_app_shell_sidebar <- function(state) {
  client <- state$client

  ui <- function(req) {
    bslib::page_sidebar(
      window_title = "Chat with {btw} tools",
      sidebar = bslib::sidebar(
        id = "tools_sidebar",
        title = btw_app_title(state$path_logo),
        width = NULL,
        height = "100%",
        style = bslib::css(max_height = "100%"),
        open = "closed",
        btw_app_tools_controls(
          state$all_available_tools,
          names(state$original_client_tools)
        ),
        bslib::input_dark_mode(style = "display: none")
      ),
      shiny::actionButton(
        "close_btn",
        label = "",
        class = "btn-close",
        style = "position: fixed; top: 6px; right: 6px;"
      ),
      class = "bslib-page-dashboard",
      class = if (nzchar(which_ide())) {
        c("btw-in-ide", sprintf("btw-in-%s", which_ide()))
      },
      htmltools::tags$div(
        "aria-label" = "Show keyboard shortcuts",
        "aria-keyshortcuts" = "?",
        class = "visually-hidden"
      ),
      shinychat::chat_mod_ui(
        "chat",
        messages = if (length(state$messages)) state$messages,
        greeting = btw_app_greeting(state$path_logo),
        width = "min(750px, 100%)",
        footer = btw_status_bar_ui(
          "status_bar",
          client = client,
          models = state$app_models,
          selected = state$selected_client
        )
      ),
      btw_app_html_dep()
    )
  }

  make_chat <- function() {
    shinychat::chat_mod_server("chat", client = client)
  }

  list(
    ui = ui,
    make_chat = make_chat,
    handlers = list(
      reveal_tools = function(session) {
        bslib::toggle_sidebar("tools_sidebar")
      }
    ),
    close_button = TRUE
  )
}

# nocov end
