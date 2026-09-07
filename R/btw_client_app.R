# nocov start

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @param messages A list of initial messages to show in the chat, passed to
#'   [shinychat::chat_mod_ui()]. With shinychat >= 0.5.0 the messages are
#'   replayed into the chat when the app session starts.
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

  page_style <- if ("page_chat" %in% getNamespaceExports("shinychat")) {
    rlang::check_installed("bslib", version = "0.12.0")
    "page_chat"
  } else {
    "sidebar"
  }

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
    page_style = page_style,
    ...
  )
}

btw_app_from_client <- function(
  client,
  messages = list(),
  allowed_tools = btw_tools(),
  app_models = "provider",
  selected_client = NULL,
  page_style = c("page_chat", "sidebar"),
  ...
) {
  page_style <- rlang::arg_match(page_style)

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

  state <- list(
    client = client,
    messages = messages,
    all_available_tools = all_available_tools,
    original_client_tools = original_client_tools,
    app_models = app_models,
    selected_client = selected_client,
    path_logo = path_logo,
    close_button = rlang::is_interactive()
  )

  shell <- switch(
    page_style,
    page_chat = btw_app_shell_page_chat(state),
    sidebar = btw_app_shell_sidebar(state)
  )

  server <- btw_app_server(
    client = client,
    app_models = app_models,
    all_available_tools = all_available_tools,
    original_client_tools = original_client_tools,
    make_chat = shell$make_chat,
    handlers = shell$handlers,
    close_button = shell$close_button,
    shell_server = shell$server,
    status_bar_buttons = shell$status_bar_buttons %||%
      c("show_sys_prompt", "clear_chat")
  )

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
    return(list(ui = shell$ui, server = server))
  }

  app <- shiny::shinyApp(shell$ui, server, ...)
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

# nocov end
