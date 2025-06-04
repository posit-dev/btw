#' @include tool-result.R
NULL

#' Tool: Search DuckDuckGo
#'
#' @examples
#' btw_tool_files_search_ddg("R programming language", max_results = 3)
#'
#' @param keywords Search keywords
#' @param max_results Maximum number of results to return, between 1 and 10.
#'
#' @return Returns a markdown table of search results.
#'
#' @family Tools
#' @export
btw_tool_files_search_ddg <- function(keywords, max_results = 3) {
  check_string(keywords, allow_empty = FALSE)
  check_number_whole(max_results, min = 1, max = 10)

  rlang::check_installed("ddgsearch", "to use the DuckDuckGo search tool")
  rlang::check_installed("ragnar", "to fetch full text content")

  results <- ddgsearch::ddg_search_text(
    keywords = keywords,
    max_results = max_results,
    full_text = TRUE
  )

  if (nrow(results) == 0) {
    return(sprintf("No results found for '%s'", keywords))
  }

  text <- glue_(
    '<web_search_result>
<title>{{title}}</title>
<url>{{href}}</url>
<content>
{{trimws(ifelse(is.na(body), summary, body))}}
</content>
</web_search_result>',
    .envir = list2env(results)
  )

  text <- paste(text, collapse = "\n\n")

  BtwWebSearchToolResult(
    value = text,
    extra = list(data = results)
  )
}

BtwWebSearchToolResult <- S7::new_class(
  "BtwWebSearchToolResult",
  parent = BtwToolResult
)

.btw_add_to_tools(
  name = "btw_tool_files_search_ddg",
  group = "search",
  tool = function() {
    if (!is_installed("ragnar")) return()
    if (!is_installed("ddgsearch")) return()

    ellmer::tool(
      btw_tool_files_search_ddg,
      .description = "Search DuckDuckGo for web pages.

Returns the full text content of the search result.
Results are cached to avoid repeated requests.
Use this tool sparingly and prefer a small number of search results at a time.",
      .annotations = ellmer::tool_annotations(
        title = "DuckDuckGo Search",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE
      ),
      keywords = ellmer::type_string(
        "The search terms to look for on DuckDuckGo"
      ),
      max_results = ellmer::type_number(
        paste(
          "Maximum number of results to return.",
          "The search tool only returns one page of DuckDuckGo results, so the maximum is 10.",
          "Defaults to 3."
        ),
        required = FALSE
      )
    )
  }
)
