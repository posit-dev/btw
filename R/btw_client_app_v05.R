# nocov start

# btw_app shell for shinychat >= 0.5.0: the tool selection menu and the
# system prompt editor live in bslib offcanvases triggered from the
# page_chat() toolbar; the status bar sits below the chat input via
# toolbar_input.

btw_app_shell_page_chat <- function(state) {
  client <- state$client

  ui <- function(req) {
    htmltools::tagList(
      shinychat::page_chat(
        title = btw_app_title(state$path_logo),
        window_title = "Chat with {btw} tools",
        id = "chat",
        toolbar = bslib::toolbar(
          bslib::toolbar_input_button(
            id = "show_tools",
            label = "Tools",
            icon = tool_icon("construction"),
            tooltip = "Select tools"
          ),
          bslib::toolbar_input_button(
            id = "show_sys_prompt",
            label = "System prompt",
            icon = tool_icon("quick-reference"),
            tooltip = "Edit system prompt"
          )
        ),
        toolbar_input = btw_status_bar_ui(
          "status_bar",
          client = client,
          models = state$app_models,
          selected = state$selected_client,
          buttons = NULL,
          wrapper = "none"
        ),
        greeting = btw_app_greeting(state$path_logo)
      ),
      bslib::offcanvas(
        btw_app_tools_controls(
          state$all_available_tools,
          names(state$original_client_tools)
        ),
        title = "Chat with {btw} tools",
        id = "tools_offcanvas",
        placement = "right",
        width = 440
      ),
      btw_app_html_dep()
    )
  }

  make_chat <- function() {
    chat <- shinychat::chat_server(
      "chat",
      client = client,
      history = state$history
    )
    if (length(state$messages)) {
      app_replay_messages(chat, state$messages)
    }
    chat
  }

  server <- function(input, session, chat) {
    shiny::observeEvent(input$show_sys_prompt, {
      bslib::show_offcanvas(
        bslib::offcanvas(
          btw_system_prompt_input(
            "system_prompt",
            chat$client$get_system_prompt()
          ),
          title = "System prompt",
          id = "system_prompt_offcanvas",
          placement = "right",
          width = 480,
          footer = shiny::actionButton(
            "system_prompt_close",
            "Done",
            class = "btn-primary"
          )
        ),
        session = session
      )
    })

    shiny::observeEvent(input$system_prompt_close, {
      bslib::hide_offcanvas("system_prompt_offcanvas", session = session)
    })

    shiny::observeEvent(input$system_prompt, ignoreInit = TRUE, {
      btw_app_set_system_prompt(chat, input$system_prompt)
    })
  }

  list(
    ui = ui,
    make_chat = make_chat,
    server = server,
    status_bar_buttons = character(0),
    handlers = list(
      reveal_tools = function(session) {
        bslib::toggle_offcanvas("tools_offcanvas", session = session)
      }
    ),
    close_button = state$close_button
  )
}

# nocov end
