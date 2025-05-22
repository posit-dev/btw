#' @include tool-result.R
NULL

#' Tool: Search for an R package on CRAN
#'
#' @description
#' Uses [pkgsearch::pkg_search()] to search for R packages on CRAN.
#'
#' @examples
#' # Copy pkgsearch results to the clipboard for use in any LLM app
#' btw(
#'   pkgsearch::pkg_search("network visualization", size = 1),
#'   clipboard = FALSE
#' )
#' btw(
#'   pkgsearch::pkg_search("network visualization", format = "long", size = 1),
#'   clipboard = FALSE
#' )
#'
#' # LLMs can use the tool directly, e.g. in `btw_client()` or `btw_app()`
#' \dontrun{
#' btw_tool_search_packages("network visualization")
#' }
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

  btw_tool_result(
    value = btw_this(res, for_tool_use = TRUE),
    data = res,
    cls = BtwPackageSearchToolResult
  )
}

pkg_search <- function(query, format = c("long", "short"), size = 10) {
  pkgsearch::pkg_search(query, format = format, size = size)
}

#' @export
btw_this.pkg_search_result <- function(x, ..., for_tool_use = FALSE) {
  meta <- attr(x, "metadata", exact = TRUE)

  res <- x
  res$version <- as.character(res$version)
  res$date <- strftime(res$date, "%F", tz = "UTC")
  res$url <- gsub("\n", " ", res$url)
  res$downloads_last_month <- format(res$downloads_last_month, big.mark = ',')

  if (meta$format == "long") {
    rows <- seq_len(min(nrow(res), 10))
    value <- ellmer::interpolate(
      "### {{ package }} (v{{ version }}) -- {{ title }}

* Maintainer: {{ maintainer_name }}
* Homepage: {{ url }}
* Date: {{ date }}
* Downloads Last Month: {{ downloads_last_month }}

{{ description }}
      ",
      .envir = list2env(res[rows, ])
    )
    value <- paste(value, collapse = "\n\n")
  } else {
    # fmt: skip
    cols <- c("package", "title", "version", "date", "url", "downloads_last_month")
    rows <- seq_len(min(nrow(res), 50))
    value <- md_table(res[rows, cols])
  }

  plural <- function(x, singular = "", plural = "s") {
    if (x == 1) singular else plural
  }

  header <- ellmer::interpolate(
    "Found {{total}} package{{ plural(total) }} matching `{{query}}`, showing {{size}} result{{ plural(size) }}.",
    .envir = list2env(meta)
  )

  if (meta$total >= 1000) {
    warning <- c(
      "Your package search query is too broad and returned too many results!",
      "*" = "It's likely the exact phrase in `query` wasn't found, so the search fell back to searching for the individual words in `query`.",
      "i" = "Try removing common words like `data`, `API`, `tools`, `statistics`, etc. or find a more specific phrase."
    )

    if (!isTRUE(for_tool_use)) {
      cli::cli_warn(warning)
    } else {
      warning[1] <- paste("WARNING:", toupper(warning[1]))
      header <- paste0(paste(warning, collapse = " "), "\n\n", header)
    }
  }

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
      .description = 'Search for an R package on CRAN.

## Search Behavior
- Prioritizes exact phrase matches over individual words
- Falls back to word matching only when phrase matching fails

## Query Strategy
- Submit separate searches for distinct concepts (e.g., `flights`, `airlines`)
- Break multi-concept queries (e.g., `flights airlines data API`) into multiple searches and synthesize results
- Search for single, specific technical terms that package authors would use
- If the search result includes more than a 1000 results, refine your query and try again.

## Examples
Good: Search for `"permutation test"` or just `"permutation"`
Bad: Search for `"statistical analysis tools for permutation test"`
',
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
        paste(
          "The number of search results to include, defaults to 10.",
          "Limited to 10 results for the 'long' format or 50 results for 'short' format."
        ),
        required = FALSE
      )
    )
  }
)
