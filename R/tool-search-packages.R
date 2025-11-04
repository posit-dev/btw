#' @include tool-result.R
NULL

#' Tool: Search for an R package on CRAN
#'
#' @description
#' Uses [pkgsearch::pkg_search()] to search for R packages on CRAN.
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
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
#' @inheritParams pkgsearch::pkg_search
#' @param n_results Number of search results to include. Defaults to 10 for
#'   'short' format and 5 for 'long' format.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns A listing of packages matching the search term.
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @name btw_tool_search_packages
#' @export
btw_tool_search_packages <- function(query, format, n_results, `_intent`) {}

btw_tool_search_packages_impl <- function(
  query,
  format = c("short", "long"),
  n_results = NULL
) {
  check_string(query)
  format <- arg_match(format)
  if (is.null(n_results)) {
    n_results <- switch(format, short = 10, long = 5)
  }
  check_number_whole(n_results)

  res <- pkg_search(query, format = format, size = n_results)

  btw_tool_result(
    value = btw_this(res, for_tool_use = TRUE),
    data = res,
    display = list(markdown = md_table(res)),
    cls = BtwSearchPackageToolResult
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


BtwSearchPackageToolResult <- S7::new_class(
  "BtwSearchPackageToolResult",
  parent = BtwToolResult
)

.btw_add_to_tools(
  name = "btw_tool_search_packages",
  group = "search",
  tool = function() {
    ellmer::tool(
      btw_tool_search_packages_impl,
      name = "btw_tool_search_packages",
      description = 'Search for an R package on CRAN.

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
      annotations = ellmer::tool_annotations(
        title = "CRAN Package Search",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        # Could move pkgsearch to Suggests...
        btw_can_register = function() TRUE
      ),
      arguments = list(
        query = ellmer::type_string(
          paste(
            "The search query, e.g. \"network visualization\", \"literate programming\".",
            "The search uses stemming to find related terms and weights phrases higher than individual terms."
          )
        ),
        format = ellmer::type_string(
          paste(
            "The format of the search results, either \"long\" or \"short\".",
            "Default is 'short' for discovery with a higher number of results.",
            "Switch to \"long\" to gather more details about each package."
          ),
          required = FALSE
        ),
        n_results = ellmer::type_number(
          paste(
            "The number of search results to include, defaults to 20 for 'short' format and 5 for 'long' format.",
            "Limited to 10 results for the 'long' format or 50 results for 'short' format."
          ),
          required = FALSE
        )
      )
    )
  }
)


#' Tool: Describe a CRAN package
#'
#' @description
#' Describes a CRAN package using [pkgsearch::cran_package()].
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' cli::cat_line(
#'   btw_this(pkgsearch::cran_package("anyflights"))
#' )
#'
#'
#' @param package_name The name of a package on CRAN.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns An info sheet about the package.
#' @export
btw_tool_search_package_info <- function(package_name, `_intent`) {}

btw_tool_search_package_info_impl <- function(package_name) {
  check_string(package_name)

  pkg <- cran_package(package_name)
  value <- btw_this(pkg)

  BtwSearchPackageInfoToolResult(
    value = value,
    extra = list(
      info = pkg,
      display = list(
        title = sprintf("{%s} Package Info", pkg$Package),
        markdown = value,
        show_request = FALSE
      )
    )
  )
}

cran_package <- function(package_name) {
  pkgsearch::cran_package(package_name)
}

BtwSearchPackageInfoToolResult <- S7::new_class(
  "BtwSearchPackageInfoToolResult",
  parent = BtwToolResult
)

#' @export
btw_this.cran_package <- function(x, ...) {
  template <- "### {{Package}} (v{{Version}}) -- {{Title}}

#### Description

{{Description}}

#### Details

* License: {{License}}{{ links_text }}
* Last Updated: {{ strftime(`Date/Publication`, '%F', tz = 'UTC') }}

#### Dependencies
{{ depends_text }}{{ imports_text }}{{ suggests_text }}

#### Author Information

{{Author}}

**Maintainer**: {{Maintainer}}"

  format_deps <- function(x, field) {
    deps <- x[[field]]
    if (is.null(deps) || length(deps) == 0) {
      return("")
    }

    deps_text <- sapply(names(deps), function(dep_name) {
      if (dep_name == "R") {
        return(paste0("* R ", deps[[dep_name]]))
      } else {
        ver <- if (deps[[dep_name]] == "*") {
          ""
        } else {
          paste0(" (", deps[[dep_name]], ")")
        }
        return(paste0("* ", dep_name, ver))
      }
    })

    paste0("\n* ", field, "\n  ", paste(deps_text, collapse = "\n  "))
  }

  depends_text <- format_deps(x, "Depends")
  imports_text <- format_deps(x, "Imports")
  suggests_text <- format_deps(x, "Suggests")

  links_text <- ""
  if (!is.null(x$URL)) {
    url_home <- gsub("\n", "", x$URL)
    links_text <- paste0(links_text, paste("\n* Home:", url_home))
  }
  if (!is.null(x$BugReports)) {
    url_bugs <- gsub("\n", "", x$BugReports)
    links_text <- paste0(links_text, paste("\n* Issue Tracker:", url_bugs))
  }

  md_text <- glue_(
    template,
    depends_text = depends_text,
    imports_text = imports_text,
    suggests_text = suggests_text,
    links_text = links_text,
    .envir = list2env(x, parent = parent.frame()),
    .trim = FALSE
  )

  return(md_text)
}

.btw_add_to_tools(
  name = "btw_tool_search_package_info",
  group = "search",
  tool = function() {
    ellmer::tool(
      btw_tool_search_package_info_impl,
      name = "btw_tool_search_package_info",
      description = paste(
        "Describe a CRAN package.",
        "Shows the title, description, dependencies and author information for a package on CRAN, regardless of whether the package is installed or not."
      ),
      annotations = ellmer::tool_annotations(
        title = "CRAN Package Info",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          paste(
            "The name of a package on CRAN.",
            "The package does not need to be installed locally."
          )
        )
      )
    )
  }
)
