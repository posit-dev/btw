client_get_models <- function(client) {
  provider <- client$get_provider()

  models_fns <- list(
    ProviderAnthropic = function(p) {
      ellmer::models_anthropic(
        base_url = p@base_url,
        credentials = p@credentials
      )
    },
    ProviderGoogleGemini = function(p) {
      ellmer::models_google_gemini(
        base_url = p@base_url,
        credentials = p@credentials
      )
    },
    ProviderAWSBedrock = function(p) {
      base_url <- sub("bedrock-runtime", "bedrock", p@base_url)
      ellmer::models_aws_bedrock(profile = p@profile, base_url = base_url)
    },
    ProviderOpenAI = function(p) {
      ellmer::models_openai(base_url = p@base_url, credentials = p@credentials)
    },
    ProviderMistral = function(p) {
      ellmer::models_mistral()
    },
    ProviderLMStudio = function(p) {
      base_url <- sub("/v1$", "", p@base_url)
      ellmer::models_lmstudio(base_url = base_url, credentials = p@credentials)
    },
    ProviderVllm = function(p) {
      ellmer::models_vllm(base_url = p@base_url, credentials = p@credentials)
    },
    ProviderOllama = function(p) {
      base_url <- sub("/v1$", "", p@base_url)
      ellmer::models_ollama(base_url = base_url, credentials = p@credentials)
    },
    ProviderPortkeyAI = function(p) {
      ellmer::models_portkey(base_url = p@base_url)
    },
    ProviderOpenAICompatible = function(p) {
      base_url <- sub("/v1$", "", p@base_url)
      ellmer::models_openai(base_url = p@base_url, credentials = p@credentials)
    }
  )

  for (cls in names(models_fns)) {
    if (inherits(provider, sprintf("ellmer::%s", cls))) {
      return(
        tryCatch(models_fns[[cls]](provider), error = function(e) {
          cli::cli_warn(
            "Failed to fetch models for provider {provider@name}",
            parent = e
          )
          NULL
        })
      )
    }
  }

  NULL
}

# Returns NULL (no selector), "provider" (lazy fetch), or list of btw.md client configs
app_resolve_model_choices <- function(
  model_choices,
  path_btw,
  client_name = NULL
) {
  if (model_choices == "none") {
    return(NULL)
  }
  if (model_choices == "provider") {
    return("provider")
  }

  config <- read_btw_file(path_btw)
  btw_models <- config$client

  if (is.null(btw_models)) {
    return("provider")
  }

  if (is_list(btw_models)) {
    if (all(nzchar(names2(btw_models)))) {
      if (
        model_choices == "auto" &&
        !is.null(client_name) &&
        is.null(resolve_model_choice_name(client_name, names(btw_models)))
      ) {
        return("provider")
      }
      return(btw_models)
    }
    cli::cli_inform(
      "Model choices in `client` in {.path {btw_md}} must be named for model selection to work."
    )
  }

  "provider"
}

resolve_model_choice_name <- function(name, choices) {
  idx <- match(tolower(name), tolower(choices))
  if (is.na(idx)) NULL else choices[[idx]]
}

client_models_from_config <- function(client_config) {
  aliases <- client_aliases(client_config)
  if (is.null(aliases)) {
    return(NULL)
  }

  model_ids <- vapply(
    client_config,
    function(cfg) {
      if (is_string(cfg)) {
        parts <- strsplit(cfg, "/", fixed = TRUE)[[1]]
        if (length(parts) > 1) paste(parts[-1], collapse = "/") else ""
      } else if (is.list(cfg) && !inherits(cfg, "Chat")) {
        cfg$model %||% ""
      } else if (inherits(cfg, "Chat")) {
        cfg$get_model()
      } else {
        ""
      }
    },
    character(1)
  )

  valid <- nzchar(model_ids)
  if (!any(valid)) {
    return(NULL)
  }

  model_ids[valid]
}

turns_replace_thinking <- function(turns) {
  lapply(turns, function(turn) {
    turn@contents <- lapply(turn@contents, function(content) {
      if (S7::S7_inherits(content, ellmer::ContentThinking)) {
        ellmer::ContentText(format(content))
      } else {
        content
      }
    })
    turn
  })
}

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
  switch(
    name,
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
