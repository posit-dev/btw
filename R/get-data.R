#' Describe a data frame in plain text
#'
#' @param x A data frame or tibble.
#' @param format One of `"skim"`, `"glimpse"`, `"print"`, or `"json"`.
#' * `"skim"` is the most information-dense format for describing the data. It
#'   uses and returns the same information as [skimr::skim()] but formatting as
#'   a JSON object that describes the dataset.
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
#' @param ... Additional arguments are silently ignored.
#'
#' @inheritSection get_installed_packages See Also
#'
#' @returns
#' A character vector containing a representation of the data frame.
#' Will error if the named data frame is not found in the environment.
#'
#' @examples
#' btw_this(mtcars)
#'
#' btw_this(mtcars, format = "print")
#'
#' btw_this(mtcars, format = "json")
#'
#' @describeIn btw_this.data.frame Summarize a data frame.
#' @export
btw_this.data.frame <- function(
  x,
  ...,
  format = c("skim", "glimpse", "print", "json"),
  dims = c(5, 100)
) {
  describe_data_frame(x, format = format, dims = dims)
}

#' @describeIn btw_this.data.frame Summarize a `tbl`.
#' @export
btw_this.tbl <- function(
  x,
  ...,
  format = c("skim", "glimpse", "print", "json"),
  dims = c(5, 100)
) {
  describe_data_frame(x, format = format, dims = dims)
}

describe_data_frame <- function(
  data_frame,
  format = c("skim", "glimpse", "print", "json"),
  dims = c(5, 100)
) {
  format <- arg_match(format)
  check_inherits(dims, "numeric")

  # models have likely the seen the "object ___ not found" quite a bit,
  # so no need to rethrow / handle errors nicely
  if (inherits(data_frame, "character")) {
    .data_name <- data_frame
    data_frame <- get(data_frame)
  }

  if (format %in% c("print", "json")) {
    n_row <- min(dims[1], nrow(data_frame))
    n_col <- min(dims[2], ncol(data_frame))
    data_frame_small <- data_frame[seq_len(n_row), seq_len(n_col), drop = FALSE]
  }

  res <- switch(
    format,
    glimpse = describe_data_frame_glimpse(x = data_frame),
    print = describe_data_frame_print(x = data_frame_small),
    json = describe_data_frame_json(x = data_frame_small),
    skim = describe_data_frame_skim(data_frame)
  )

  res
}

tool_describe_data_frame <- .btw_add_to_tools(function() {
  ellmer::tool(
    describe_data_frame,
    .name = "btw_show_data_frame_structure",
    .description = "Show the data frame or table or get information about the structure of a data frame or table.",
    data_frame = ellmer::type_string(
      "The name of the data frame."
    ),
    format = ellmer::type_string(
      paste(
        "The output format of the data frame: 'skim', 'glimpse', 'print', or 'json'. Default 'skim'.",
        "",
        "* skim: Returns a JSON object with information about every column in the table.",
        "* glimpse: Returns the number of rows, columns, column names and types and the first values of each column",
        "* print: Prints the data frame",
        "* json: Returns the data frame as JSON",
        sep = "\n"
      ),
      required = FALSE
    ),
    dims = ellmer::type_array(
      paste(
        'Dimensions of the data frame to use for the "print" or "json" format.',
        "A numeric vector of length 2 as number of rows and columns. Default `c(5, 100)`."
      ),
      items = ellmer::type_integer(),
      required = FALSE
    )
  )
})

describe_data_frame_glimpse <- function(x, x_name) {
  res <- cli::ansi_strip(capture.output(dplyr::glimpse(x)))
  res
}

describe_data_frame_print <- function(x) {
  withr::local_options(pillar.advice = FALSE, pillar.min_title_chars = Inf)

  res <- cli::ansi_strip(
    capture.output(tibble::as_tibble(x, width = 1000, n = Inf))
  )
  res[2:length(res)]
}

describe_data_frame_json <- function(x) {
  capture.output(jsonlite::toJSON(x, auto_unbox = TRUE, pretty = TRUE))
}

describe_data_frame_skim <- function(df) {
  cols <- skimr::skim(df, .data_name = "")

  attrs <- attributes(cols)[c("data_cols", "data_rows", "groups")]
  names(attrs) <- c("n_cols", "n_rows", "groups")
  attrs[["class"]] <- class(df)

  # Move variable to the front
  cols <- cols[c("skim_variable", setdiff(names(cols), "skim_variable"))]

  # Drop histogram and whitespace stats
  # TODO: Others
  cols <- cols[!grepl("[.](whitespace|hist)$", names(cols))]

  # Transpose the list into row-major (purrr::transpose() in base)
  cols <- do.call(mapply, c(FUN = list, cols, SIMPLIFY = FALSE))

  cols <- lapply(cols, function(col) {
    keep <- sprintf("^(skim_|%s.)", col$skim_type)
    col <- col[grepl(keep, names(col))]
    names(col) <- sub("^skim_", "", names(col))
    names(col) <- sub(paste0(col$type, "."), "", names(col), fixed = TRUE)

    if (col$type == "character") {
      var <- sym(col$variable)

      if (col$n_unique <= 10) {
        col$values <- dplyr::pull(dplyr::distinct(df, !!var))
      } else {
        counts <- dplyr::count(df, !!var)
        counts <- dplyr::mutate(counts, rank = dplyr::row_number(.data$n))
        counts <- dplyr::filter(counts, .data$rank <= 11)
        values <- dplyr::pull(counts, !!var)
        col$values <- values[seq_len(min(length(values), 10))]
      }
      col$values <- vapply(col$values, FUN.VALUE = character(1), function(x) {
        if (is.na(x) || nchar(x) <= 140) return(x)
        paste(substring(x, 1, 140), "...")
      })
    } else if (col$type == "factor") {
      col$levels <- levels(df[[col$variable]])
    }

    col
  })

  attrs$columns <- cols

  jsonlite::toJSON(attrs, auto_unbox = TRUE)
}
