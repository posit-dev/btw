#' Describe a data frame in plain text
#'
#' @param data_frame A single string naming a data frame or, alternatively,
#' the data frame itself.
# TODO: should it be a different function name when there's no `get()`ting
# happening?
#' @param format One of `"glimpse"`, `"print"`, or `"json"`.
#' * To glimpse the data column-by-column, use `"glimpse"`. This is
#'   particularly helpful for getting a sense of data frame column names,
#'   types, and distributions, when pairings of entries in individual rows
#'   aren't particularly important.
#' * To just print out the data frame, use `print()`.
#' * To get a json representation of the data, use `"json"`. This is
#'   particularly helpful when the pairings among entries in specific rows
#'   are important to demonstrate.
#' @param dims The number of rows and columns to show, as a numeric vector of
#' length two. For example, the default `dims = c(5, 100)` shows the first 5
#' rows and 100 columns, whereas `dims = c(Inf, Inf)` would show all of the data.
#'
#' @returns
#' A character vector containing a representation of the data frame.
#' Will error if the named data frame is not found in the environment.
#'
#' @export
get_data_frame <- function(
    data_frame,
    format = c("glimpse", "print", "json"),
    dims = c(5, 100)
) {
  format <- rlang::arg_match(format)
  check_inherits(dims, "numeric")

  # models have likely the seen the "object ___ not found" quite a bit,
  # so no need to rethrow / handle errors nicely
  if (!inherits(data_frame, "data.frame")) {
    data_frame <- get(data_frame)
  }

  n_row <- min(dims[1], nrow(data_frame))
  n_col <- min(dims[2], ncol(data_frame))
  data_frame_small <- data_frame[seq_len(n_row), seq_len(n_col), drop = FALSE]

  res <- switch(
    format,
    glimpse = get_data_frame_glimpse(x = data_frame_small),
    print = get_data_frame_print(x = data_frame_small),
    json = get_data_frame_json(x = data_frame_small)
  )

  paste0(res, collapse = "\n")
}

tool_get_data_frame <- function() {
  ellmer::tool(
    get_data_frame,
    "Function to extract or manipulate a data frame with various formatting
  options.",
    data_frame = ellmer::type_string(
      "The name of the data frame to be described."
    ),
    format = ellmer::type_string(
      "The output format of the data frame. Options are \"glimpse\",
  \"print\", or \"json\". Defaults to \"glimpse\".",
      required = FALSE
    ),
    dims = ellmer::type_array(
      "Dimensions for printing the data frame. A numeric vector of length
  2, where the first element represents the number of rows and the second
  the number of columns. Defaults to `c(5, 100)`.",
      items = ellmer::type_integer(),
      required = FALSE
    )
  )
}

get_data_frame_glimpse <- function(x, x_name) {
  res <- cli::ansi_strip(capture.output(dplyr::glimpse(x)))
  res[3:length(res)]
}

get_data_frame_print <- function(x) {
  withr::local_options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

  res <- cli::ansi_strip(capture.output(tibble::as_tibble(x, width = 1000, n = Inf)))
  res[2:length(res)]
}

get_data_frame_json <- function(x) {
  capture.output(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE))
}
