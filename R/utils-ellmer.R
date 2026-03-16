btw_prompt <- function(path, ..., .envir = parent.frame()) {
  path <- system.file("prompts", path, package = "btw")
  ellmer::interpolate_file(path, ..., .envir = .envir)
}

chat_get_tokens <- function(client) {
  tokens <- tryCatch(
    client$get_tokens(),
    error = function(e) NULL
  )
  if (is.null(tokens)) {
    return(NULL)
  }

  input_tokens <- 0
  output_tokens <- 0
  cached_tokens <- 0

  if (!is.null(tokens) && nrow(tokens) > 0) {
    if (utils::packageVersion("ellmer") <= "0.3.0") {
      last_user <- tokens[tokens$role == "user", ]
      if (nrow(last_user) > 0) {
        input_tokens <- as.integer(utils::tail(last_user$tokens_total, 1))
      }
      tokens_assistant <- tokens[tokens$role == "assistant", ]
      if (nrow(tokens_assistant) > 0) {
        output_tokens <- as.integer(sum(tokens_assistant$tokens))
      }
    } else {
      # output tokens are by turn, so we sum them all
      if ("output" %in% colnames(tokens)) {
        output_tokens <- sum(tokens$output)
      }
      # input and cached tokens are accumulated in the last API call
      if ("input" %in% colnames(tokens)) {
        input_tokens <-
          tokens$input[[length(tokens$input)]]
      }
      if ("cached_input" %in% colnames(tokens)) {
        cached_tokens <- tokens$cached_input[[
          length(tokens$cached_input)
        ]]
      }
    }
  }

  list(
    input = input_tokens,
    output = output_tokens,
    cached = cached_tokens
  )
}

chat_get_cost <- function(client) {
  tryCatch(
    client$get_cost(),
    error = function(e) NA
  )
}

# Built-in tool wrapping ---------------------------------------------------

ellmer_ToolBuiltIn <- function() {
  asNamespace("ellmer")[["ToolBuiltIn"]]
}

BtwToolBuiltIn <- tryCatch(
  S7::new_class(
    "BtwToolBuiltIn",
    parent = ellmer_ToolBuiltIn(),
    properties = list(
      title = S7::class_character,
      description = S7::class_character,
      annotations = S7::class_list
    )
  ),
  error = function(e) NULL
)

built_in_tool_info <- function(name) {
  switch(name,
    web_search = list(
      title = "Web Search",
      description = "Search the web for up-to-date information.",
      read_only_hint = TRUE,
      open_world_hint = TRUE
    ),
    web_fetch = list(
      title = "Web Fetch",
      description = "Fetch and read content from web URLs.",
      read_only_hint = TRUE,
      open_world_hint = FALSE
    ),
    list(
      title = to_title_case(gsub("_", " ", name)),
      description = sprintf("A provider built-in %s tool.", name)
    )
  )
}

wrap_built_in_tools <- function(client) {
  ToolBuiltIn <- ellmer_ToolBuiltIn()
  if (is.null(ToolBuiltIn) || is.null(BtwToolBuiltIn)) {
    return(invisible(client))
  }

  tools <- client$get_tools()
  tools <- map(tools, function(tool) {
    if (!S7::S7_inherits(tool, ToolBuiltIn)) {
      return(tool)
    }
    if (S7::S7_inherits(tool, BtwToolBuiltIn)) {
      return(tool)
    }
    info <- built_in_tool_info(tool@name)
    BtwToolBuiltIn(
      name = tool@name,
      json = tool@json,
      title = info$title,
      description = info$description,
      annotations = compact(list(
        title = info$title,
        btw_group = "built-in",
        icon = tool_group_icon("built-in"),
        read_only_hint = info$read_only_hint,
        open_world_hint = info$open_world_hint
      ))
    )
  })
  client$set_tools(tools)
  invisible(client)
}
