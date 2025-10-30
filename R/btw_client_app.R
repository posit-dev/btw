# nocov start

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @param messages A list of initial messages to show in the chat, passed to
#'   [shinychat::chat_mod_ui()].
#' @export
btw_app <- function(
  ...,
  client = NULL,
  tools = NULL,
  path_btw = NULL,
  messages = list()
) {
  rlang::check_installed("shiny")
  rlang::check_installed("bslib")
  rlang::check_installed("htmltools")
  rlang::check_installed("shinychat", version = "0.2.0")

  if (getOption("btw.app.close_on_session_end", FALSE)) {
    cli::cli_alert("Starting up {.fn btw::btw_app} ...")
  }

  if (!inherits(client, "AsIs")) {
    client <- btw_client(
      client = client,
      tools = tools,
      path_btw = path_btw
    )
  }

  btw_app_from_client(client, messages = messages, ...)
}

btw_app_from_client <- function(client, messages = list(), ...) {
  path_figures_installed <- system.file("help", "figures", package = "btw")
  path_figures_dev <- system.file("man", "figures", package = "btw")
  path_logo <- "btw_figures/logo.png"

  provider_model <- sprintf(
    "%s/%s",
    client$get_provider()@name,
    client$get_model()
  )

  if (nzchar(path_figures_installed)) {
    shiny::addResourcePath("btw_figures", path_figures_installed)
  } else if (nzchar(path_figures_dev)) {
    shiny::addResourcePath("btw_figures", path_figures_dev)
  } else {
    path_logo <- NULL
  }

  btw_title <- function(in_sidebar) {
    logo <- shiny::img(
      src = path_logo,
      class = "me-2 dib",
      style = bslib::css(max_width = "35px"),
      .noWS = c("before", "after")
    )
    shiny::tags$header(
      if (!is.null(path_logo)) {
        if (in_sidebar) {
          shiny::span(logo)
        } else {
          shiny::actionLink("show_sidebar", logo)
        }
      },
      "Chat with",
      shiny::code("{btw}"),
      "tools",
      class = "sidebar-title mb-0",
    )
  }

  ui <- function(req) {
    bslib::page_sidebar(
      window_title = "Chat with {btw} tools",
      sidebar = bslib::sidebar(
        id = "tools_sidebar",
        title = btw_title(TRUE),
        width = NULL,
        height = "100%",
        style = bslib::css(max_height = "100%"),
        open = "closed",
        shiny::div(
          class = "btn-group",
          shiny::actionButton(
            "select_all",
            "Select All",
            icon = shiny::icon("check-square"),
            class = "btn-sm"
          ),
          shiny::actionButton(
            "deselect_all",
            "Select none",
            icon = shiny::icon("square"),
            class = "btn-sm"
          )
        ),
        shiny::div(
          class = "overflow-y-auto overflow-x-visible",
          app_tool_group_inputs(
            btw_tools_df(),
            initial_tool_names = map_chr(client$get_tools(), S7::prop, "name")
          ),
          shiny::uiOutput("ui_other_tools")
        ),
        bslib::input_dark_mode(style = "display: none")
      ),
      shiny::actionButton(
        "close_btn",
        label = "",
        class = "btn-close",
        style = "position: fixed; top: 6px; right: 6px;"
      ),
      btw_title(FALSE),
      shinychat::chat_mod_ui(
        "chat",
        messages = messages,
        width = "min(750px, 100%)"
      ),
      shiny::tags$footer(
        class = "status-footer d-flex align-items-center gap-3 small text-muted",
        style = "width: min(725px, 100%); margin-inline: auto;",
        bslib::tooltip(
          shiny::actionLink("show_sys_prompt", tool_icon("quick-reference")),
          "Show system prompt"
        ),
        shiny::div(
          class = "status-provider-model",
          shiny::span(class = "font-monospace", provider_model),
        ),
        shiny::div(
          class = "ms-auto status-tokens font-monospace",
          bslib::tooltip(
            shiny::span(id = "status_tokens_input"),
            "Input tokens"
          ),
          bslib::tooltip(
            shiny::span(id = "status_tokens_output"),
            "Output tokens"
          )
        ),
        shiny::div(
          class = "status-cost font-monospace",
          bslib::tooltip(
            id = "status_cost_tooltip",
            shiny::span(id = "status_cost"),
            "Estimated cost"
          )
        ),
        htmltools::htmlDependency(
          name = "countup.js",
          version = readLines(
            system.file("js/countupjs/VERSION", package = "btw")
          ),
          package = "btw",
          src = "js/countupjs",
          script = list(
            list(src = "countUp.min.js", type = "module"),
            list(src = "btw_app.js", type = "module")
          ),
          stylesheet = "btw_app.css",
          all_files = FALSE
        )
      ),
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          "
        :root { --bslib-sidebar-width: max(30vw, 275px); }
        .opacity-100-hover:hover { opacity: 1 !important; }
        :hover > .opacity-100-hover-parent, .opacity-100-hover-parent:hover { opacity: 1 !important; }
        .bslib-sidebar-layout > .main > main .sidebar-title { display: none; }
        .sidebar-collapsed > .main > main .sidebar-title { display: block; }
        .bslib-sidebar-layout.sidebar-collapsed>.collapse-toggle { top: 1.8rem; }
        .bslib-page-main { gap: 0.5rem; }
        .status-footer a { color: inherit; }
        .status-footer svg { height: 1em; width: 1em; }
        .status-provider-model {
          text-overflow: ellipsis;
          text-wrap-mode: nowrap;
          overflow: hidden;
        }
        .modal { --bs-modal-margin: min(2rem, 10%); }
      "
        )),
      )
    )
  }

  server <- function(input, output, session) {
    chat <- shinychat::chat_mod_server("chat", client = client)

    chat_get_tokens <- function() {
      tryCatch(
        chat$client$get_tokens(),
        error = function(e) NULL
      )
    }

    chat_get_cost <- function() {
      tryCatch(
        chat$client$get_cost(),
        error = function(e) NA
      )
    }

    chat_tokens <- shiny::reactiveVal(
      chat_get_tokens(),
      label = "btw_app_tokens"
    )
    chat_cost <- shiny::reactiveVal(
      chat_get_cost(),
      label = "btw_app_cost"
    )

    shiny::observeEvent(chat$last_turn(), {
      chat_tokens(chat_get_tokens())
      chat_cost(chat_get_cost())
    })

    shiny::observeEvent(chat$last_input(), {
      ids <- paste0("status_", c("tokens_input", "tokens_output", "cost"))
      for (id in ids) {
        session$sendCustomMessage(
          "btw_update_status",
          list(id = id, status = "recalculating")
        )
      }
    })

    shiny::observeEvent(chat_tokens(), {
      tokens <- chat_tokens()
      value <- 0

      if (!is.null(tokens) && nrow(tokens) > 1) {
        last_user <- tokens[tokens$role == "user", ]
        if (nrow(last_user) > 0) {
          value <- as.integer(utils::tail(last_user$tokens_total, 1))
        }
      }

      session$sendCustomMessage(
        "btw_update_status",
        list(
          id = "status_tokens_input",
          value = value,
          status = "ready"
        )
      )
    })

    shiny::observeEvent(chat_tokens(), {
      tokens <- chat_tokens()
      value <- 0

      if (!is.null(tokens) && nrow(tokens) > 1) {
        tokens_assistant <- tokens[tokens$role == "assistant", ]
        if (nrow(tokens_assistant) > 0) {
          value <- as.integer(sum(tokens_assistant$tokens))
        }
      }

      session$sendCustomMessage(
        "btw_update_status",
        list(
          id = "status_tokens_output",
          value = value,
          status = "ready"
        )
      )
    })

    shiny::observeEvent(chat_cost(), {
      cost <- chat_cost()
      if (is.null(cost)) {
        return(0)
      }

      if (is.na(cost)) {
        session$sendCustomMessage(
          "btw_update_status",
          list(
            id = "status_cost",
            status = "unknown"
          )
        )
        bslib::update_tooltip("status_cost_tooltip", "Token pricing is unknown")
        return()
      }

      session$sendCustomMessage(
        "btw_update_status",
        list(
          id = "status_cost",
          value = cost
        )
      )
    })

    shiny::observeEvent(input$show_sys_prompt, {
      input_sys_prompt <- shiny::textAreaInput(
        "system_prompt",
        label = NULL,
        placeholder = "Instructions for the AI assistant...",
        value = chat$client$get_system_prompt(),
        width = "100%",
        autoresize = TRUE,
        updateOn = "blur"
      )
      input_sys_prompt <- htmltools::tagAppendAttributes(
        input_sys_prompt,
        class = "font-monospace",
        .cssSelector = "textarea"
      )

      modal <- shiny::modalDialog(
        title = "System Prompt",
        size = "xl",
        easyClose = TRUE,
        footer = shiny::modalButton("Close"),
        input_sys_prompt
      )

      modal <- htmltools::tagAppendAttributes(
        modal,
        class = "modal-fullscreen-lg-down",
        .cssSelector = ".modal-dialog"
      )

      shiny::showModal(modal)
    })

    shiny::observeEvent(
      input$system_prompt,
      ignoreInit = TRUE,
      {
        new_system_prompt <- input$system_prompt

        if (identical(new_system_prompt, chat$client$get_system_prompt())) {
          return()
        }

        if (nzchar(trimws(new_system_prompt))) {
          action <- "Updated system prompt"
          icon <- shiny::icon("check")
        } else {
          action <- "Cleared system prompt"
          new_system_prompt <- NULL
          icon <- shiny::icon("eraser")
        }

        tryCatch(
          {
            chat$client$set_system_prompt(new_system_prompt)
            shiny::showNotification(shiny::span(icon, action))
          },
          error = function(e) {
            shiny::showNotification(
              shiny::tagList(
                shiny::p(
                  shiny::icon("warning"),
                  "Failed to update system prompt",
                  class = "fw-bold"
                ),
                shiny::p(shiny::HTML(sprintf("<code>%s</code>", e$message)))
              ),
              type = "error"
            )
          }
        )
      },
    )

    shiny::observeEvent(input$show_sidebar, {
      bslib::toggle_sidebar("tools_sidebar")
    })

    tool_groups <- unique(btw_tools_df()$group)
    other_tools <- keep(client$get_tools(), function(tool) {
      !identical(substring(tool@name, 1, 9), "btw_tool_")
    })

    selected_tools <- shiny::reactive({
      tool_groups <- c(tool_groups, if (length(other_tools) > 0) "other")
      unlist(
        map(tool_groups, function(group) input[[paste0("tools_", group)]])
      )
    })

    shiny::observeEvent(input$select_all, {
      tools <- btw_tools_df()
      for (group in tool_groups) {
        shiny::updateCheckboxGroupInput(
          session = session,
          inputId = paste0("tools_", group),
          selected = tools[tools$group == group, ][["name"]]
        )
      }
    })

    shiny::observeEvent(input$deselect_all, {
      tools <- btw_tools_df()
      for (group in tool_groups) {
        shiny::updateCheckboxGroupInput(
          session = session,
          inputId = paste0("tools_", group),
          selected = ""
        )
      }
    })

    lapply(tool_groups, function(group) {
      shiny::observeEvent(input[[paste0("tools_toggle_", group)]], {
        current <- input[[paste0("tools_", group)]]
        all_tools <- btw_tools_df()
        group_tools <- all_tools[all_tools$group == group, ][["name"]]
        if (length(current) == length(group_tools)) {
          # All selected, so deselect all
          shiny::updateCheckboxGroupInput(
            session = session,
            inputId = paste0("tools_", group),
            selected = ""
          )
        } else {
          # Not all selected, so select all
          shiny::updateCheckboxGroupInput(
            session = session,
            inputId = paste0("tools_", group),
            selected = group_tools
          )
        }
      })
    })

    shiny::observe({
      if (!length(selected_tools())) {
        client$set_tools(list())
      } else {
        .btw_tools <- keep(btw_tools(), function(tool) {
          tool@name %in% selected_tools()
        })
        .other_tools <- keep(other_tools, function(tool) {
          tool@name %in% selected_tools()
        })
        client$set_tools(c(.btw_tools, other_tools))
      }
    })

    output$ui_other_tools <- shiny::renderUI({
      if (length(other_tools) == 0) {
        return(NULL)
      }

      other_tools_df <- dplyr::bind_rows(
        map(other_tools, function(tool) {
          dplyr::tibble(
            group = "other",
            name = tool@name,
            description = tool@description,
            title = tool@annotations$title %||% tool@name,
            is_read_only = tool@annotations$read_only_hint %||% NA,
            is_open_world = tool@annotations$open_world_hint %||% NA
          )
        })
      )

      app_tool_group_choice_input("other", other_tools_df)
    })

    shiny::observeEvent(input$close_btn, {
      shiny::stopApp()
    })

    if (getOption("btw.app.close_on_session_end", FALSE)) {
      shiny::onSessionEnded(function() {
        cli::cli_alert_success("Shutting down background {.fn btw::btw_app}.")
        shiny::stopApp()
      })
    }
  }

  old_load <- shiny::getShinyOption("load.interface")
  old_save <- shiny::getShinyOption("save.interface")
  opts <- shiny::shinyOptions(
    load.interface = btw_shiny_bookmark_load,
    save.interface = btw_shiny_bookmark_save
  )
  on.exit(shiny::shinyOptions(
    load.interface = old_load,
    save.interface = old_save
  ))

  app <- shiny::shinyApp(ui, server, ...)
  tryCatch(shiny::runGadget(app), interrupt = function(cnd) NULL)
  invisible(client)
}

btw_tools_df <- function() {
  .btw_tools <- map(.btw_tools, function(def) {
    tool <- def$tool()
    if (is.null(tool)) {
      return()
    }
    dplyr::tibble(
      group = def$group,
      name = tool@name,
      description = tool@description,
      title = tool@annotations$title,
      is_read_only = tool@annotations$read_only_hint %||% NA,
      is_open_world = tool@annotations$open_world_hint %||% NA
    )
  })
  dplyr::bind_rows(.btw_tools)
}

app_tool_group_inputs <- function(tools_df, initial_tool_names = NULL) {
  tools_df <- split(tools_df, tools_df$group)

  map2(
    names(tools_df),
    tools_df,
    app_tool_group_choice_input,
    initial_tool_names = initial_tool_names
  )
}

app_tool_group_choice_input <- function(
  group,
  group_tools_df,
  initial_tool_names = NULL
) {
  choice_names <- pmap(group_tools_df, app_tool_group_choices_labels)

  if (is.null(initial_tool_names)) {
    initial_tool_names <- group_tools_df$name
  }

  label_icon <- tool_group_icon(group, "construction")

  label_text <- switch(
    group,
    "docs" = shiny::span(label_icon, "Documentation"),
    "env" = shiny::span(label_icon, "Environment"),
    "files" = shiny::span(label_icon, "Files"),
    "git" = shiny::span(label_icon, "Git"),
    "github" = shiny::span(label_icon, "GitHub"),
    "ide" = shiny::span(label_icon, "IDE"),
    "search" = shiny::span(label_icon, "Search"),
    "session" = shiny::span(label_icon, "Session Info"),
    "web" = shiny::span(label_icon, "Web Tools"),
    "other" = shiny::span(label_icon, "Other Tools"),
    to_title_case(group)
  )

  shiny::checkboxGroupInput(
    inputId = paste0("tools_", group),
    label = shiny::h3(
      class = "h6 mb-0",
      shiny::actionLink(
        paste0("tools_toggle_", group),
        label_text,
        class = "link-body-emphasis",
        style = "text-decoration: none;"
      )
    ),
    choiceNames = choice_names,
    choiceValues = group_tools_df$name,
    selected = intersect(group_tools_df$name, initial_tool_names),
  )
}

app_tool_group_choices_labels <- function(
  title,
  description,
  ...,
  is_read_only = NA,
  is_open_world = NA
) {
  description <- strsplit(description, "\\.\\s")[[1]][1]
  description <- paste0(sub("\\.$", "", description), ".")

  shiny::tagList(
    bslib::tooltip(
      shiny::span(
        title,
        shiny::HTML("&nbsp;", .noWS = c("before", "after")),
        shiny::icon(
          "info-circle",
          class = "small text-secondary opacity-50 opacity-100-hover-parent",
          .noWS = c("before", "after")
        ),
      ),
      description,
      placement = "right"
    ),
    if (!isTRUE(is_read_only)) {
      bslib::tooltip(
        shiny::icon(
          "file-pen",
          class = "small text-danger opacity-50 opacity-100-hover"
        ),
        shiny::HTML(
          "<strong>Not Read-Only</strong><br>This tool self-reports that it can modify files."
        )
      )
    },
    if (isTRUE(is_open_world)) {
      bslib::tooltip(
        shiny::icon(
          "satellite-dish",
          class = "small text-primary opacity-50 opacity-100-hover"
        ),
        shiny::HTML(
          "<strong>Open World Tool</strong><br>This tool may access external resources, such as the web or databases."
        )
      )
    }
  )
}

btw_shiny_bookmarks_clean <- function(max_age_d = 30) {
  dirs_og <- fs::dir_info(btw_shiny_bookmark_path(""))
  dirs <- dirs_og[
    dirs_og$modification_time < (Sys.time() - max_age_d * 24 * 3600),
  ]

  if (nrow(dirs) == 0) {
    cli::cli_inform("No shiny bookmarks older than {max_age_d} days found.")
    return(invisible(dirs_og))
  }

  cli::cli_inform(
    "Clean up shiny {nrow(dirs)} bookmark{?s} older than {max_age_d} days?"
  )
  is_ok <- utils::menu(choices = c("Yes", "No"), graphics = FALSE)
  if (identical(is_ok, 1L)) {
    fs::dir_delete(dirs$path)
  } else {
    cli::cli_inform("Cancelled.")
  }
  invisible(dirs)
}

btw_shiny_bookmark_path <- function(id) {
  path_btw_cache("shiny_bookmarks", id)
}

btw_shiny_bookmark_save <- function(id, callback) {
  stateDir <- btw_shiny_bookmark_path(id)
  if (!fs::dir_exists(stateDir)) {
    fs::dir_create(stateDir)
  }

  cli::cli_inform("Saving bookmark state to {.path {stateDir}}")
  callback(stateDir)
}

btw_shiny_bookmark_load <- function(id, callback) {
  stateDir <- btw_shiny_bookmark_path(id)
  cli::cli_inform("Loading bookmark state from {.path {stateDir}}")
  callback(stateDir)
}

# nocov end
