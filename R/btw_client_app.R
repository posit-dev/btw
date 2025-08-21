# nocov start

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @export
btw_app <- function(..., client = NULL, tools = NULL, path_btw = NULL) {
  rlang::check_installed("shiny")
  rlang::check_installed("bslib")
  rlang::check_installed("shinychat", version = "0.2.0")

  client <- btw_client(
    client = client,
    tools = tools,
    path_btw = path_btw
  )

  path_figures_installed <- system.file("help", "figures", package = "btw")
  path_figures_dev <- system.file("man", "figures", package = "btw")
  path_logo <- "btw_figures/logo.png"

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
      shinychat::chat_mod_ui("chat"),
      shiny::tags$head(
        shiny::tags$style(shiny::HTML(
          "
        :root { --bslib-sidebar-width: max(30vw, 275px); }
        .opacity-100-hover:hover { opacity: 1 !important; }
        :hover > .opacity-100-hover-parent, .opacity-100-hover-parent:hover { opacity: 1 !important; }
        .bslib-sidebar-layout > .main > main .sidebar-title { display: none; }
        .sidebar-collapsed > .main > main .sidebar-title { display: block; }
        .bslib-sidebar-layout.sidebar-collapsed>.collapse-toggle { top: 1.8rem; }
      "
        )),
      )
    )
  }

  server <- function(input, output, session) {
    shinychat::chat_mod_server("chat", client = client)
    shiny::observeEvent(input$close_btn, {
      shiny::stopApp()
    })

    shiny::observeEvent(input$show_sidebar, {
      bslib::sidebar_toggle("tools_sidebar")
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

  label_text <- switch(
    group,
    "docs" = shiny::span(shiny::icon("book"), "Documentation"),
    "env" = shiny::span(shiny::icon("globe"), "Environment"),
    "files" = shiny::span(shiny::icon("folder"), "Files"),
    "ide" = shiny::span(shiny::icon("code"), "IDE"),
    "search" = shiny::span(shiny::icon("search"), "Search"),
    "session" = shiny::span(shiny::icon("desktop"), "Session Info"),
    "other" = shiny::span(shiny::icon("tools"), "Other Tools"),
    paste0(toupper(substring(group, 1, 1)), substring(group, 2))
  )

  shiny::checkboxGroupInput(
    inputId = paste0("tools_", group),
    label = shiny::h3(label_text, class = "h6 mb-0"),
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
