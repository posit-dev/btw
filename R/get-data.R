#' Describe a data frame in plain text
#'
#' @param data_frame A single string naming a data frame.
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
#' @param ... Currently unused; must be empty.
#' @param clipboard Optional. Whether to copy the output to the clipboard.
#' Defaults to `interactive()`.
#'
#' @returns
#' A character vector containing a representation of the data frame.
#' Will error if the named data frame is not found in the environment.
#'
#' @export
get_data_frame <- function(
    data_frame,
    format = c("glimpse", "print", "json"),
    dims = c(5, 100),
    ...,
    clipboard = interactive()
) {
  check_bool(clipboard)
  rlang::arg_match(format)
  check_inherits(dims, "numeric")

  # models have likely the seen the "object ___ not found" quite a bit,
  # so no need to rethrow / handle errors nicely
  d <- get(data_frame)
  d_small <- d[dims[[1]], dims[[2]]]

  res <- switch(
    format,
    glimpse = get_data_frame_glimpse(x = d_small),
    print = get_data_frame_print(x = d_small),
    json = get_data_frame_json(x = d_small)
  )

  if (clipboard && !in_btw()) {
    write_to_clipboard(res)
  }

  res
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
    ),
    clipboard = ellmer::type_boolean(
      "Logical value. If `TRUE`, stores the result in the clipboard.
       Always leave this as FALSE.",
      required = FALSE
    )
  )
}

get_data_frame_glimpse <- function(x, x_name) {
  cli::ansi_strip(capture.output(dplyr::glimpse(x)))
}

get_data_frame_print <- function(x) {
  withr::local_options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

  cli::ansi_strip(capture.output(tibble::as_tibble(x, width = 1000, n = Inf)))
}

get_data_frame_json <- function(x) {
  capture.output(jsonlite::toJSON(mtcars, auto_unbox = TRUE, pretty = TRUE))
}
