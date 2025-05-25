#' Create a btw-enhanced ellmer chat client
#'
#' @description
#' Creates an [ellmer::Chat] client, enhanced with the tools from
#' [btw_tools()]. Use `btw_client()` to create the chat client for
#' general or interactive use at the console, or `btw_app()` to create a chat
#' client and launch a Shiny app for chatting with a btw-enhanced LLM in your
#' local workspace.
#'
#' ## Project Context
#'
#' You can keep track of project-specific rules, guidance and context by adding
#' a `btw.md` file in your project directory. Any time you start a chat client
#' with `btw_client()` or launch a chat session with `btw_app()`, btw will
#' automatically find and include the contents of the `btw.md` file in your
#' chat.
#'
#' Use `btw.md` to inform the LLM of your preferred code style, to provide
#' domain-specific terminology or definitions, to establish project
#' documentation, goals and constraints, to include reference materials such or
#' technical specifications, or more. Storing this kind of information in
#' `btw.md` may help you avoid repeating yourself and can be used to maintain
#' coherence across many chat sessions.
#'
#' The `btw.md` file, when present, is included as part of the system prompt for
#' your chat conversation. You can structure the file in any way you wish.
#'
#' You can also use the `btw.md` file to choose default chat settings for your
#' project in a YAML block at the top of the file. In this YAML block you can
#' choose the default `provider`, `model` and `tools` for `btw_client()` or
#' `btw_app()`. `provider` chooses the `ellmer::chat_*()` function, e.g.
#' `provider: openai` or `provider: chat_openai` to use [ellmer::chat_openai()].
#' `tools` chooses which btw tools are included in the chat, and all other
#' values are passed to the `ellmer::chat_*()` constructor, e.g. `model:
#' gpt-4o`, `seed: 42`, or `echo: all``.
#'
#' Here's an example `btw.md` file:
#'
#' ````
#' ---
#' provider: claude
#' model: claude-3-7-sonnet-20250219
#' tools: [data, docs, environment]
#' ---
#'
#' Follow these important style rules for any R code in this project:
#'
#' * Prefer solutions that use {tidyverse}
#' * Always use `<-` for assignment
#' * Always use the native base-R pipe `|>` for piped expressions
#'
#' ````
#'
#' ## Client Options
#'
#' * `btw.client`: The [ellmer::Chat] client to use as the basis for new
#'   `btw_client()` or `btw_app()` chats.
#' * `btw.tools`: The btw tools to include by default when starting a new
#'   btw chat, see [btw_tools()] for details.
#'
#' @examplesIf rlang::is_interactive()
#' withr::local_options(list(
#'   btw.client = ellmer::chat_ollama(model="llama3.1:8b")
#' ))
#'
#' chat <- btw_client()
#' chat$chat("How can I replace `stop()` calls with functions from the cli package?")
#'
#' @param client An [ellmer::Chat] client, defaults to
#'   [ellmer::chat_anthropic()]. You can use the `btw.client` option to set a
#'   default client for new `btw_client()` calls, or use a `btw.md` project file
#'   for default chat client settings, like provider and model. We check the
#'   `client` argument, then the `btw.client` R option, and finally the `btw.md`
#'   project file, using only the client definition from the first of these that
#'   is available.
#' @param tools Optional names of tools or tool groups to include in the chat
#'   client. By default, all btw tools are included. For example, use
#'   `include = "docs"` to include only the documentation related tools, or
#'   `include = c("env", "docs")`, etc. `btw_client()` also supports
#'   `tools = FALSE` to skip registering \pkg{btw} tools with the chat client.
#' @param path_btw A path to a `btw.md` project context file. If `NULL`, btw
#'   will find a project-specific `btw.md` file in the parents of the current
#'   working directory.
#' @param ... Additional arguments are ignored. `...` are included for future
#'   feature expansion.
#'
#' @return Returns an [ellmer::Chat] object with additional tools registered
#'   from [btw_tools()]. `btw_app()` returns the chat object invisibly, and
#'   the chat object with the messages added during the chat session.
#'
#' @describeIn btw_client Create a btw-enhanced [ellmer::Chat] client
#' @export
btw_client <- function(..., client = NULL, tools = NULL, path_btw = NULL) {
  check_dots_empty()

  config <- btw_client_config(client, tools, config = read_btw_file(path_btw))
  skip_tools <- isFALSE(config$tools) || identical(config$tools, "none")

  client <- config$client

  sys_prompt <- client$get_system_prompt()
  sys_prompt <- c(
    "# System and Session Context",
    "Please account for the following R session and system settings in all responses.",
    "",
    btw_tool_session_platform_info()@value,
    "",
    if (!skip_tools) {
      c(
        "# Tools",
        "",
        paste(
          "You have access to tools that help you interact with the user's R session and workspace.",
          "Use these tools when they are helpful and appropriate to complete the user's request.",
          "These tools are available to augment your ability to help the user,",
          "but you are smart and capable and can answer many things on your own.",
          "It is okay to answer the user without relying on these tools."
        ),
        ""
      )
    },
    if (!is.null(config$btw_context)) {
      c(
        "# Project Context",
        "",
        trimws(paste(config$btw_context, collapse = "\n")),
        ""
      )
    },
    "---\n",
    sys_prompt
  )
  client$set_system_prompt(paste(sys_prompt, collapse = "\n"))

  if (!skip_tools) {
    client$set_tools(tools = c(client$get_tools(), btw_tools(config$tools)))
  }

  client
}

# nocov start

#' @describeIn btw_client Create a btw-enhanced client and launch a Shiny app to
#'   chat
#' @export
btw_app <- function(..., client = NULL, tools = NULL, path_btw = NULL) {
  check_dots_empty()
  rlang::check_installed("shiny")
  rlang::check_installed("bslib")
  rlang::check_installed("shinychat", version = "0.2.0")

  client <- btw_client(
    client = client,
    tools = tools,
    path_btw = path_btw
  )

  ui <- bslib::page_sidebar(
    sidebar = bslib::sidebar(
      title = shiny::tagList(
        "btw Tools",
        # shiny::img(
        #   src = system.file("man", "figures", "logo.png", package = "btw")
        # )
      ),
      width = "30vw",
      height = "100%",
      style = bslib::css(max_height = "100%"),
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
        class = "overflow-y-auto",
        create_tool_accordion(btw_tools_df())
      )
    ),
    shiny::actionButton(
      "close_btn",
      label = "",
      class = "btn-close",
      style = "position: fixed; top: 6px; right: 6px;"
    ),
    shinychat::chat_mod_ui("chat", client = client)
  )

  server <- function(input, output, session) {
    shinychat::chat_mod_server("chat", client = client)
    shiny::observeEvent(input$close_btn, {
      shiny::stopApp()
    })

    tool_groups <- unique(btw_tools_df()$group)

    selected_tools <- shiny::reactive({
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
        client$set_tools(btw_tools(selected_tools()))
      }
    })
  }

  app <- shiny::shinyApp(ui, server)
  tryCatch(shiny::runGadget(app), interrupt = function(cnd) NULL)
  invisible(client)
}

btw_tools_df <- function() {
  .btw_tools <- map(.btw_tools, function(def) {
    tool <- def$tool()
    if (is.null(tool)) return()
    data.frame(
      group = def$group,
      name = tool@name,
      description = tool@description,
      title = tool@annotations$title,
      is_read_only = tool@annotations$read_only_hint %||% FALSE,
      is_open_world = tool@annotations$open_world_hint %||% FALSE
    )
  })
  .btw_tools <- dplyr::bind_rows(.btw_tools)
  dplyr::as_tibble(.btw_tools)
}

create_tool_accordion <- function(tools_df) {
  # Get unique groups
  groups <- unique(tools_df$group)

  # Create an accordion panel for each group
  panels <- lapply(groups, function(group) {
    # Filter tools for this group
    group_tools <- tools_df[tools_df$group == group, ]

    # Create choice names with tooltips for each tool
    choice_names <- lapply(seq_len(nrow(group_tools)), function(i) {
      tool <- group_tools[i, ]

      # Extract description up to the first empty line
      description <- strsplit(tool$description, "\n\n")[[1]][1]

      # Create a label with tooltip
      bslib::tooltip(
        span(tool$title),
        description,
        placement = "right"
      )
    })

    # Create choice values (tool names)
    choice_values <- group_tools$name

    # Create checkbox group input
    checkbox_group <- shiny::checkboxGroupInput(
      inputId = paste0("tools_", group),
      label = NULL,
      choiceNames = choice_names,
      choiceValues = choice_values,
      selected = choice_values,
    )

    # Create the accordion panel with the group name as title
    shiny::tagList(
      shiny::h3(
        class = "h6",
        paste0(toupper(substring(group, 1, 1)), substring(group, 2))
      ),
      checkbox_group
    )
  })

  shiny::tagList(!!!panels)
}

# nocov end

btw_client_config <- function(client = NULL, tools = NULL, config = list()) {
  config$tools <-
    tools %||%
    getOption("btw.tools") %||%
    config$tools

  if (!is.null(client)) {
    check_inherits(client, "Chat")
    config$client <- client
    return(config)
  }

  default <- getOption("btw.client")
  if (!is.null(default)) {
    check_inherits(default, "Chat")
    config$client <- default$clone()
    return(config)
  }

  if (!is.null(config$provider)) {
    chat_args <- config[setdiff(
      names(config),
      c("tools", "provider", "btw_context")
    )]
    chat_fn <- gsub(" ", "_", tolower(config$provider))
    if (!grepl("^chat_", chat_fn)) {
      chat_fn <- paste0("chat_", chat_fn)
    }
    chat_client <- call2(.ns = "ellmer", chat_fn, !!!chat_args)
    config$client <- eval(chat_client)
    return(config)
  }

  config$client <- ellmer::chat_anthropic(echo = "output")
  config
}

read_btw_file <- function(path = NULL) {
  must_find <- !is.null(path)

  path <- path %||% path_find_in_project("btw.md")

  if (!must_find && is.null(path)) {
    return(list())
  }

  if (must_find && (is.null(path) || !fs::file_exists(path))) {
    cli::cli_abort("Invalid {.arg path}: {.path {path}} does not exist.")
  }

  config <- rmarkdown::yaml_front_matter(path)

  remove_yaml <- function(path) {
    pyfm <- asNamespace("rmarkdown")[["partition_yaml_front_matter"]]
    pyfm(readLines(path, warn = FALSE))$body
  }

  config$btw_context <- remove_yaml(path)
  config
}
