# nocov start

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @param messages A list of initial messages to show in the chat, passed to
#'   [shinychat::chat_mod_ui()].
#' @param model_choices Can be one of `"btw_md"` (model choices from your
#'   `path_btw` configuration), `"provider"` (models from the provider API),
#'   `"auto"` (uses `path_btw` if `client` comes from `path_btw`, otherwise
#'   falling back to provider), or `"none"` (don't show model choices).
#' @export
btw_app <- function(
  ...,
  client = NULL,
  tools = NULL,
  path_btw = NULL,
  messages = list(),
  model_choices = c("auto", "btw_md", "provider", "none")
) {
  rlang::check_installed("shiny")
  rlang::check_installed("bslib", version = "0.11.0")
  rlang::check_installed("htmltools")
  rlang::check_installed("shinychat", version = "0.4.0")

  model_choices <- rlang::arg_match(model_choices)

  if (getOption("btw.app.close_on_session_end", FALSE)) {
    cli::cli_alert("Starting up {.fn btw::btw_app} ...")
  }

  client_name <- if (is_string(client)) client
  client_is_object <- inherits(client, "Chat")

  # Get reference tools for the app
  if (inherits(client, "AsIs")) {
    # When client is AsIs (pre-configured), use btw_tools() as reference
    reference_tools <- btw_tools()
    app_models <- app_resolve_model_choices(model_choices, path_btw = FALSE)
  } else {
    client <- btw_client(
      client = client,
      tools = tools,
      path_btw = path_btw
    )

    # Create a reference client to get the full tool set
    withr::with_options(list(btw.client.quiet = TRUE), {
      bare_client <- client$clone()
      bare_client$set_tools(list())

      reference_client <- btw_client(
        client = bare_client,
        tools = names(btw_tools()),
        path_btw = path_btw
      )
      reference_tools <- reference_client$get_tools()
    })

    app_models <- app_resolve_model_choices(
      model_choices,
      path_btw,
      client_name = client_name,
      client_is_object = client_is_object
    )
  }

  selected_client <- if (is.list(app_models) && !is.null(client_name)) {
    resolve_model_choice_name(client_name, names(app_models))
  }

  btw_app_from_client(
    client,
    messages = messages,
    allowed_tools = reference_tools,
    app_models = app_models,
    selected_client = selected_client,
    ...
  )
}

btw_app_html_dep <- function() {
  htmltools::htmlDependency(
    name = "btw",
    version = utils::packageVersion("btw"),
    package = "btw",
    src = "js/app",
    script = list(
      list(src = "btw_app.js", type = "module")
    ),
    stylesheet = "btw_app.css",
    all_files = TRUE
  )
}

btw_app_from_client <- function(
  client,
  messages = list(),
  allowed_tools = btw_tools(),
  app_models = "provider",
  selected_client = NULL,
  ...
) {
  path_figures_installed <- system.file("help", "figures", package = "btw")
  path_figures_dev <- system.file("man", "figures", package = "btw")
  path_logo <- "btw_figures/logo.png"

  # Store original client tools (preserves configuration like closures)
  # $get_tools() returns a named list where names are tool names
  original_client_tools <- client$get_tools()

  # Union: all tools to show in UI preferring original client tools
  all_available_tools <- utils::modifyList(allowed_tools, original_client_tools)

  if (nzchar(path_figures_installed)) {
    shiny::addResourcePath("btw_figures", path_figures_installed)
  } else if (nzchar(path_figures_dev)) {
    shiny::addResourcePath("btw_figures", path_figures_dev)
  } else {
    path_logo <- NULL
  }

  btw_title <- function() {
    logo <- shiny::img(
      src = path_logo,
      class = "me-2 dib",
      style = bslib::css(max_width = "35px"),
      .noWS = c("before", "after")
    )
    shiny::tags$header(
      if (!is.null(path_logo)) {
        shiny::span(logo)
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
        title = btw_title(),
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
            btw_tools_df(all_available_tools),
            # names(list()) is NULL, but NULL means "select all" in
            # app_tool_group_choice_input; use character(0) for "select none".
            initial_tool_names = names(original_client_tools) %||% character(0)
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
        messages = messages,
        greeting = btw_app_greeting(path_logo),
        width = "min(750px, 100%)",
        footer = if (utils::packageVersion("shinychat") >= "0.4.0") {
          btw_status_bar_ui(
            "status_bar",
            client = client,
            models = app_models,
            selected = selected_client
          )
        }
      ),
      btw_app_html_dep(),
    )
  }

  server <- function(input, output, session) {
    chat <- shinychat::chat_mod_server("chat", client = client)

    if (utils::packageVersion("shinychat") >= "0.4.0") {
      res <- btw_status_bar_server("status_bar", chat, app_models)

      shiny::observeEvent(res$clear_chat(), {
        chat$clear(client_history = "clear")
      })
    }

    shiny::observeEvent(input$show_sidebar, {
      bslib::toggle_sidebar("tools_sidebar")
    })

    shiny::observeEvent(input$greeting_reveal_tools, {
      bslib::toggle_sidebar("tools_sidebar")
    })

    output$ui_greeting_n_tools <- shiny::renderUI({
      n_tools <- length(selected_tools())
      if (n_tools > 0) {
        shiny::tags$p(
          class = "text-muted small",
          shiny::actionLink(
            "greeting_reveal_tools",
            sprintf(
              "%d tool%s enabled",
              n_tools,
              if (n_tools == 1) " is" else "s are"
            )
          )
        )
      }
    })

    tool_groups <- unique(btw_tools_df(all_available_tools)$group)

    # Split tools: btw tools (including built-in) and other (non-btw) tools
    is_btw_tool <- function(tool) {
      identical(substring(tool@name, 1, 9), "btw_tool_") ||
        (!is.null(BtwToolBuiltIn) && S7::S7_inherits(tool, BtwToolBuiltIn))
    }
    btw_available_tools <- keep(all_available_tools, is_btw_tool)
    other_tools <- discard(all_available_tools, is_btw_tool)

    selected_tools <- shiny::reactive({
      tool_groups <- c(tool_groups, if (length(other_tools) > 0) "other")
      unlist(
        map(tool_groups, function(group) input[[paste0("tools_", group)]])
      )
    })

    shiny::observeEvent(input$select_all, {
      tools <- btw_tools_df(all_available_tools)
      for (group in tool_groups) {
        shiny::updateCheckboxGroupInput(
          session = session,
          inputId = paste0("tools_", group),
          selected = tools[tools$group == group, ][["name"]]
        )
      }
    })

    shiny::observeEvent(input$deselect_all, {
      tools <- btw_tools_df(all_available_tools)
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
        all_tools <- btw_tools_df(all_available_tools)
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
        sel_tools <- all_available_tools[selected_tools()]
        client$set_tools(sel_tools)
      }
    })

    skills_read_file_mismatch <- shiny::reactive({
      sel <- selected_tools()
      "btw_tool_skill" %in% sel && !"btw_tool_files_read" %in% sel
    })

    shiny::observeEvent(skills_read_file_mismatch(), {
      if (isTRUE(skills_read_file_mismatch())) {
        notifier(
          id = "skills_read_file_mismatch",
          shiny::icon("triangle-exclamation", class = "text-warning"),
          shiny::tagList(
            shiny::HTML(
              "The <strong>Load Skill tool</strong> works best with the <strong>Read File tool</strong> enabled"
            ),
            shiny::actionButton(
              class = "btn-sm mt-2",
              "enable_read_file_tool",
              "Enable Read File Tool"
            )
          )
        )
      }
    })

    shiny::observeEvent(input$enable_read_file_tool, {
      file_tools <- c(input$tools_files, "btw_tool_files_read")
      shiny::updateCheckboxGroupInput(
        session = session,
        inputId = "tools_files",
        selected = file_tools
      )
      bslib_hide_toast <- asNamespace("bslib")[["hide_toast"]]
      if (!is.null(bslib_hide_toast)) {
        bslib_hide_toast("skills_read_file_mismatch")
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

    shiny::observeEvent(input[["__btw_show_shortcuts"]], {
      shortcut_data <- input[["__btw_show_shortcuts"]]

      shiny::showModal(
        shiny::modalDialog(
          id = "btw_keyboard_shortcuts_modal",
          title = sprintf("Keyboard Shortcuts (%s)", shortcut_data$platform),
          size = "m",
          easyClose = TRUE,
          footer = NULL,
          btw_keyboard_shortcuts(shortcut_data$shortcuts)
        )
      )
    })

    shiny::observeEvent(input[["__btw_ide_open_file"]], {
      path <- input[["__btw_ide_open_file"]]
      if (rstudioapi::hasFun("navigateToFile")) {
        rstudioapi::navigateToFile(path)
      } else {
        cli::cli_alert(c(
          "Your IDE does not support opening files with {.pkg rstudioapi}.",
          "i" = "{.path {path}}"
        ))
      }
    })

    if (rstudioapi::hasFun("insertText")) {
      shiny::observeEvent(input[["__btw_ide_insert_code"]], {
        code <- input[["__btw_ide_insert_code"]]
        tryCatch(
          switch(
            code$location,
            new_file = ide_insert_new_file(code$code, code$language %||% "r"),
            cursor = ide_insert_cursor(code$code),
            console = ide_run_in_console(code$code),
            cli::cli_abort(
              "Unknown code insertion location: {.val {code$location}}"
            )
          ),
          error = function(err) {
            if (nchar(code$code) > 50) {
              code_preview <- paste0(
                substr(code$code, 1, 47),
                "..."
              )
            } else {
              code_preview <- code$code
            }

            cli::cli_warn(c(
              "Failed to insert code into IDE: {.msg {err$message}}",
              "i" = "Location: {.val {code$location}}",
              "i" = "Code: {.val {code_preview}}"
            ))
          }
        )
      })
    }

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

  if (identical(Sys.getenv("BTW_IN_TESTING"), "true")) {
    return(list(ui = ui, server = server))
  }

  app <- shiny::shinyApp(ui, server, ...)
  if (getOption("btw.app.in_addin", FALSE)) {
    shiny::runApp(app, launch.browser = function(url) {
      rstudioapi::setPersistentValue("btw_app_addin_url", url)
      invisible(url)
    })
  } else {
    tryCatch(shiny::runGadget(app), interrupt = function(cnd) NULL)
    invisible(client)
  }
}

btw_app_greeting <- function(path_logo) {
  pkg_ver <- function(pkg) {
    tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) NULL)
  }

  ver_label <- function(pkg) {
    v <- pkg_ver(pkg)
    if (!is.null(v)) paste0(pkg, " ", v)
  }

  versions <- Filter(
    Negate(is.null),
    list(
      ver_label("btw"),
      ver_label("shinychat"),
      ver_label("ellmer")
    )
  )

  shinychat::chat_greeting(
    htmltools::div(
      class = "text-center",
      if (!is.null(path_logo)) {
        shiny::img(
          src = path_logo,
          alt = "btw",
          style = bslib::css(
            width = "100%",
            width = "clamp(100px, 40vw, 200px)",
            margin_bottom = "1rem",
            filter = "drop-shadow(0px 6px 10px #00000030)"
          )
        )
      },
      shiny::tags$h2(
        "Chat with ",
        shiny::code("{btw}"),
        " tools",
        class = "mb-2"
      ),
      shiny::tags$p(
        class = "text-muted small font-monospace",
        paste(unlist(versions), collapse = " \u00b7 ")
      ),
      shiny::uiOutput("ui_greeting_n_tools")
    )
  )
}

# Status Bar ----

notifier <- function(icon, action, error = NULL, ...) {
  error_body <- if (!is.null(error)) {
    shiny::p(shiny::HTML(sprintf("<code>%s</code>", error$message)))
  }

  bslib_toast <- asNamespace("bslib")[["toast"]]
  bslib_show_toast <- asNamespace("bslib")[["show_toast"]]
  bslib_toast_header <- asNamespace("bslib")[["toast_header"]]

  if (is.null(bslib_toast) || is.null(bslib_show_toast)) {
    if (!is.null(error)) {
      body <- shiny::span(icon, action)
    } else {
      body <- shiny::tagList(
        shiny::p(
          shiny::icon("warning"),
          "Failed to update system prompt",
          class = "fw-bold"
        ),
        error_body
      )
    }
    shiny::showNotification(
      body,
      type = if (is.null(error)) "message" else "error"
    )
    return()
  }

  toast <- bslib_toast(
    if (is.null(error)) action else error_body,
    header = if (!is.null(error)) {
      bslib_toast_header(action, icon = icon)
    },
    icon = if (is.null(error)) icon,
    position = "top-right",
    ...
  )
  bslib_show_toast(toast)
}

btw_status_bar_ui <- function(
  id,
  client,
  models = "provider",
  selected = NULL
) {
  ns <- shiny::NS(id)

  if (identical(models, "provider")) {
    selected <- client$get_model()
    choices <- selected # full list populated asynchronously in server
  } else if (length(models) > 0) {
    selected <- selected %||% names(models)[[1]]
    choices <- names(models)
  }

  shiny::tagList(
    shiny::tags$footer(
      class = "status-footer small text-muted",
      style = "width: min(725px, 100%); margin-inline: auto;",
      bslib::toolbar(
        gap = "0.25em",
        shiny::uiOutput(ns("provider")),
        if (is.null(models)) {
          shiny::div(
            class = "status-model badge text-body-secondary fw-normal",
            client$get_model()
          )
        } else {
          bslib::toolbar_input_select(
            id = ns("model"),
            label = "Model",
            selected = selected,
            choices = choices,
            style = bslib::css(min_width = "12rem")
          )
        },
        bslib::toolbar_spacer(),
        bslib::toolbar_input_button(
          id = ns("show_sys_prompt"),
          label = "Show system prompt",
          icon = tool_icon("quick-reference")
        ),
        bslib::toolbar_input_button(
          id = ns("clear_chat"),
          label = "Clear chat",
          icon = tool_icon("ink-eraser")
        ),
        bslib::toolbar_divider(),
        shiny::div(
          class = "status-tokens font-monospace",
          bslib::tooltip(
            id = ns("status_tokens_input_tooltip"),
            shiny::span(
              id = ns("status_tokens_input"),
              class = "status-countup",
              "data-type" = "tokens_input"
            ),
            "Input tokens"
          ),
          bslib::tooltip(
            shiny::span(
              id = ns("status_tokens_output"),
              class = "status-countup",
              "data-type" = "tokens_output"
            ),
            "Output tokens"
          )
        ),
        shiny::div(
          class = "status-cost font-monospace",
          bslib::tooltip(
            id = ns("status_cost_tooltip"),
            shiny::span(
              id = ns("status_cost"),
              class = "status-countup",
              "data-type" = "cost"
            ),
            "Estimated cost"
          )
        ),
        width = "100%"
      )
    )
  )
}

btw_status_bar_server <- function(id, chat, models = "provider") {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      provider_name <- shiny::reactiveVal({
        # chat$client is not reactive, will be updated manually on model change
        chat$client$get_provider()@name
      })

      model_name <- shiny::reactiveVal({
        chat$client$get_model()
      })

      if (identical(models, "provider")) {
        shiny::observe({
          provider_df <- client_get_models(chat$client)
          if (!is.null(provider_df)) {
            current <- shiny::isolate(model_name())
            all_choices <- union(current, sort(provider_df$id))
            shiny::updateSelectInput(
              session,
              "model",
              choices = all_choices,
              selected = current
            )
          }
        })
      }

      output$provider <- shiny::renderUI({
        badge <- shiny::div(
          class = "status-provider badge",
          provider_name()
        )
        if (identical(models, "provider")) {
          badge
        } else {
          bslib::tooltip(badge, model_name(), placement = "top")
        }
      })

      shiny::observeEvent(input$model, ignoreInit = TRUE, {
        tryCatch(
          {
            old_provider <- chat$client$get_provider()@name

            if (identical(models, "provider")) {
              new_client <- chat$client$clone()
              new_client$set_model(input$model)
            } else {
              new_config <- models[[input$model]]
              new_client <- btw_client(client = new_config, tools = FALSE)
              new_client$set_system_prompt(chat$client$get_system_prompt())
              turns <- chat$client$get_turns()
              new_provider <- new_client$get_provider()@name
              if (!identical(old_provider, new_provider)) {
                turns <- turns_replace_thinking(turns)
              }
              new_client$set_turns(turns)
              new_client$set_tools(chat$client$get_tools())
            }

            chat$set_client(new_client, sync = FALSE)
            new_provider <- chat$client$get_provider()@name
            provider_name(new_provider)
            model_name(chat$client$get_model())

            notifier(
              shiny::icon("check"),
              shiny::HTML(
                sprintf(
                  "Switched model to <code>%s</code> from %s.",
                  new_client$get_model(),
                  new_provider
                )
              )
            )
          },
          error = function(err) {
            notifier(
              shiny::icon("warning"),
              sprintf("Failed to switch model to %s", input$model),
              error = err
            )
          }
        )
      })

      chat_tokens <- shiny::reactiveVal(
        chat_get_tokens(chat$client),
        label = "btw_app_tokens"
      )
      chat_cost <- shiny::reactiveVal(
        chat_get_cost(chat$client),
        label = "btw_app_cost"
      )

      shiny::observeEvent(chat$last_turn(), {
        chat_tokens(chat_get_tokens(chat$client))
        chat_cost(chat_get_cost(chat$client))
      })

      send_status_message <- function(id, status, ...) {
        session$sendCustomMessage(
          "btw_update_status",
          list(id = session$ns(id), status = status, ...)
        )
      }

      shiny::observeEvent(chat$last_input(), {
        ids <- paste0(
          "status_",
          c(
            "tokens_input",
            "tokens_output",
            if (!is.null(chat_cost()) && !is.na(chat_cost())) "cost"
          )
        )
        for (id in ids) {
          send_status_message(id, "recalculating")
        }
      })

      shiny::observeEvent(input$clear_chat, {
        session$sendCustomMessage("btw_reset_status", list(ns = session$ns("")))
      })

      shiny::observeEvent(chat_tokens(), {
        tokens <- chat_tokens()

        send_status_message(
          "status_tokens_input",
          "ready",
          # show input + cached tokens to get total tokens
          value = tokens$input + tokens$cached
        )
        if (tokens$cached > 0) {
          bslib::update_tooltip(
            "status_tokens_input_tooltip",
            sprintf(
              "Input tokens (%s cached)",
              format(tokens$cached, big.mark = ",")
            )
          )
        }
        send_status_message(
          "status_tokens_output",
          "ready",
          value = tokens$output
        )
      })

      shiny::observeEvent(chat_cost(), {
        cost <- chat_cost()
        if (is.null(cost)) {
          cost <- 0
        }

        if (is.na(cost)) {
          send_status_message("status_cost", "unknown")
          bslib::update_tooltip(
            "status_cost_tooltip",
            "Token pricing is unknown"
          )
          return()
        }

        send_status_message("status_cost", "ready", value = cost)
      })

      shiny::observeEvent(input$show_sys_prompt, {
        bslib_input_code_editor <- asNamespace("bslib")[["input_code_editor"]]

        if (is.null(bslib_input_code_editor)) {
          input_sys_prompt <- shiny::textAreaInput(
            session$ns("system_prompt"),
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
        } else {
          input_sys_prompt <- bslib_input_code_editor(
            id = session$ns("system_prompt"),
            label = NULL,
            value = chat$client$get_system_prompt(),
            language = "markdown"
          )
        }

        modal <- shiny::modalDialog(
          title = NULL,
          size = "xl",
          easyClose = TRUE,
          footer = shiny::modalButton("Close"),
          bslib::card(
            style = "height: 100%",
            bslib::card_header("Edit System Prompt"),
            bslib::card_body(
              padding = 4,
              input_sys_prompt
            ),
          )
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
              notifier(icon, action)
            },
            error = function(err) {
              notifier(icon, "Error updating system prompt", error = err)
            }
          )
        }
      )

      return(
        list(
          clear_chat = shiny::reactive(input$clear_chat)
        )
      )
    }
  )
}

# Tools in sidebar ----

btw_tools_df <- function(tools = btw_tools()) {
  all_btw_tools <- map(tools, function(tool) {
    group <- tool@annotations$btw_group %||% "other"
    if (group == "env" && isTRUE(getOption("btw.app.in_addin"))) {
      # TODO: Remove this check when the addin can reach the global env
      return()
    }
    dplyr::tibble(
      group = group,
      name = tool@name,
      description = tool@description,
      title = tool@annotations$title,
      is_read_only = tool@annotations$read_only_hint %||% NA,
      is_open_world = tool@annotations$open_world_hint %||% NA
    )
  })
  dplyr::bind_rows(all_btw_tools)
}

app_tool_group_inputs <- function(tools_df, initial_tool_names = NULL) {
  tools_df <- split(tools_df, tools_df$group)

  # Only show "deprecated" group if any deprecated tools are initially selected,

  # and only show the specific deprecated tools that are selected
  if ("deprecated" %in% names(tools_df)) {
    deprecated_df <- tools_df[["deprecated"]]
    selected_deprecated <- deprecated_df$name %in% initial_tool_names
    if (!any(selected_deprecated)) {
      tools_df[["deprecated"]] <- NULL
    } else {
      tools_df[["deprecated"]] <-
        deprecated_df[selected_deprecated, , drop = FALSE]
    }
  }

  # Reorder groups: agent, docs, files, env first, then alphabetical,

  # then other, then deprecated (if shown)
  group_names <- names(tools_df)
  priority_groups <- c("agent", "skills", "docs", "files", "env")
  trailing_groups <- c("other", "deprecated")
  priority_present <- intersect(priority_groups, group_names)
  middle_groups <- sort(setdiff(
    group_names,
    c(priority_groups, trailing_groups)
  ))
  trailing_present <- intersect(trailing_groups, group_names)
  ordered_groups <- c(priority_present, middle_groups, trailing_present)

  map2(
    ordered_groups,
    tools_df[ordered_groups],
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
    "agent" = shiny::span(label_icon, "Agents"),
    "built-in" = shiny::span(label_icon, "Built-in"),
    "cran" = shiny::span(label_icon, "CRAN"),
    "deprecated" = shiny::span(label_icon, "Deprecated", class = "text-danger"),
    "docs" = shiny::span(label_icon, "Documentation"),
    "env" = shiny::span(label_icon, "Environment"),
    "eval" = shiny::span(label_icon, "Code Evaluation"),
    "files" = shiny::span(label_icon, "Files"),
    "git" = shiny::span(label_icon, "Git"),
    "github" = shiny::span(label_icon, "GitHub"),
    "ide" = shiny::span(label_icon, "IDE"),
    "pkg" = shiny::span(label_icon, "Package Tools"),
    "run" = shiny::span(label_icon, "Run Code"),
    "sessioninfo" = shiny::span(label_icon, "Session Info"),
    "skills" = shiny::span(label_icon, "Skills"),
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

# Keyboard Shortcuts ----
btw_keyboard_shortcuts <- function(shortcuts) {
  items_html <- lapply(shortcuts, function(s) {
    keys <- strsplit(s$keys, "+", fixed = TRUE)[[1]]

    htmltools::tags$div(
      style = "margin-bottom: 8px;",
      lapply(keys, htmltools::tags$kbd),
      htmltools::tags$span(style = "margin-left: 6px;", s$action)
    )
  })

  items_html
}

# App bookmarking ----

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
