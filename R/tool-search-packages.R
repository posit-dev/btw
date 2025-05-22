#' @include tool-result.R
NULL

#' Tool: Search for an R package on CRAN
#'
#' @description
#' Uses [pkgsearch::pkg_search()] to search for R packages on CRAN.
#'
#' @examples
#' btw_tool_search_packages("LLMs")
#'
#' btw_tool_search_packages("network visualization")
#'
#' @inheritParams pkgsearch::pkg_search
#' @param n_results Number of search results to include.
#'
#' @returns A listing of packages matching the search term.
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @name btw_tool_search_packages
#' @export
btw_tool_search_packages <- function(
  query,
  format = c("long", "short"),
  n_results = 10
) {
  format <- arg_match(format)
  check_number_whole(n_results)

  res <- pkg_search(query, format = format, size = n_results)

  btw_tool_result(btw_this(res), res, cls = BtwPackageSearchToolResult)
}

pkg_search <- function(query, format = c("long", "short"), size = 10) {
  pkgsearch::pkg_search(query, format = format, size = size)
}

#' @export
btw_this.pkg_search_result <- function(x, ...) {
  meta <- attr(x, "metadata", exact = TRUE)

  if (meta$format == "long") {
    value <- ellmer::interpolate(
      "### {{ package }} (v{{ version }}) -- {{ title }}

Maintainer
:    {{ maintainer_name }}

Homepage
:    {{ gsub('\n', ' ', url) }}

Date
:    {{ as.Date(date) }}

Downloads Last Month
:    {{ format(downloads_last_month, big.mark = ',') }}

{{ description }}
      ",
      .envir = list2env(x)
    )
    value <- paste(value, collapse = "\n\n")
  } else {
    # fmt: skip
    cols <- c("package", "title", "version", "date", "url", "downloads_last_month")
    value <- x[cols]
    value$version <- as.character(value$version)
    value$date <- strftime(value$date, "%F")
    value$downloads_last_month <- format(
      value$downloads_last_month,
      big.mark = ","
    )

    value <- md_table(value)
  }

  plural <- function(x, singular = "", plural = "s") {
    if (x == 1) singular else plural
  }

  header <- ellmer::interpolate(
    "Found {{total}} package{{ plural(total) }} matching `{{query}}`, showing {{size}} result{{ plural(size) }}.",
    .envir = list2env(meta)
  )

  paste(header, value, sep = "\n\n")
}


BtwPackageSearchToolResult <- S7::new_class(
  "BtwPackageSearchToolResult",
  parent = BtwToolResult
)

.btw_add_to_tools(
  name = "btw_tool_search_packages",
  group = "search",
  tool = function() {
    ellmer::tool(
      btw_tool_search_packages,
      .description = "Search for an R package on CRAN.",
      .annotations = ellmer::tool_annotations(
        title = "CRAN Package Search",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE
      ),
      query = ellmer::type_string(
        paste(
          "The search query, e.g. \"network visualization\", \"literate programming\".",
          "The search uses stemming to find related terms and weights phrases higher than individual terms."
        )
      ),
      format = ellmer::type_string(
        paste(
          "The format of the search results, either \"long\" or \"short\".",
          "Prefer \"long\" for more details about each package, or \"short\" for a quick overview when used with a higher number of results."
        ),
        required = FALSE
      ),
      n_results = ellmer::type_number(
        "The number of search results to include, defaults to 10.",
        required = FALSE
      )
    )
  }
)
