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
