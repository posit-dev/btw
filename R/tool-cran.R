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
#' @family cran tools
#' @name btw_tool_cran_search
#' @export
btw_tool_cran_search <- function(query, format, n_results, `_intent`) {}

btw_tool_cran_search_impl <- function(
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
  name = "btw_tool_cran_search",
  group = "cran",
  alias_group = "search",
  alias_name = "btw_tool_search_packages",
  tool = function() {
    ellmer::tool(
      btw_tool_cran_search_impl,
      name = "btw_tool_cran_search",
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
#' @param after Only return releases on or after this ISO date (`YYYY-MM-DD`).
#' @param before Only return releases on or before this ISO date
#'   (`YYYY-MM-DD`).
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns An info sheet about the package.
#' @family cran tools
#' @export
btw_tool_cran_package <- function(package_name, `_intent`) {}

btw_tool_cran_package_impl <- function(package_name) {
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

#' Tool: List CRAN package versions
#'
#' @description
#' Lists the current CRAN version and archived package versions with their
#' release dates. Archive dates are taken from CRAN's package archive index.
#'
#' @param package_name The name of a package on CRAN.
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns A data frame with the version, release date and timestamp, current
#'   release status, and source tarball URL for each package release.
#' @seealso [btw_tools()]
#' @family cran tools
#' @export
btw_tool_cran_versions <- function(package_name, after, before, `_intent`) {}

btw_tool_cran_versions_impl <- function(
  package_name,
  after = NULL,
  before = NULL
) {
  versions <- cran_versions(package_name, after = after, before = before)
  value <- paste(
    sprintf("### CRAN releases for %s", package_name),
    md_table(versions[c("version", "released")]),
    sep = "\n\n"
  )

  btw_tool_result(
    value = value,
    data = versions,
    display = list(
      title = sprintf("{%s} CRAN Releases", package_name),
      markdown = value,
      show_request = FALSE
    )
  )
}

cran_versions <- function(package_name, after = NULL, before = NULL) {
  check_string(package_name)
  after <- as_cran_release_date(after, "after")
  before <- as_cran_release_date(before, "before")
  if (!is.null(after) && !is.null(before) && after > before) {
    cli::cli_abort("{.arg after} must be on or before {.arg before}.")
  }

  current <- cran_current_version(package_name)
  archived <- cran_archive_versions(package_name)
  versions <- rbind(current, archived)

  if (!nrow(versions)) {
    cli::cli_abort("Package {.pkg {package_name}} was not found on CRAN.")
  }

  versions <- versions[!duplicated(versions$version), ]
  if (!is.null(after)) {
    versions <- versions[versions$released >= after, ]
  }
  if (!is.null(before)) {
    versions <- versions[versions$released <= before, ]
  }
  versions[order(base::package_version(versions$version), decreasing = TRUE), ]
}

as_cran_release_date <- function(x, arg) {
  check_string(x, allow_null = TRUE)
  if (is.null(x)) {
    return(NULL)
  }
  if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    cli::cli_abort("{.arg {arg}} must be an ISO date like {.val 2023-01-01}.")
  }

  date <- as.Date(x)
  if (is.na(date)) {
    cli::cli_abort("{.arg {arg}} must be a valid ISO date.")
  }
  date
}

cran_archive_versions <- function(package_name) {
  archive <- tryCatch(
    cran_archive_page(package_name),
    error = function(e) NULL
  )
  if (is.null(archive)) {
    return(cran_versions_data())
  }

  rows <- xml2::xml_find_all(
    archive,
    "//tr[td/a[contains(@href, '.tar.gz')]]"
  )
  if (!length(rows)) {
    return(cran_versions_data())
  }

  hrefs <- xml2::xml_attr(
    xml2::xml_find_first(rows, ".//a[contains(@href, '.tar.gz')]"),
    "href"
  )
  pattern <- paste0(
    "^",
    gsub(".", "\\.", package_name, fixed = TRUE),
    "_(.+)\\.tar\\.gz$"
  )
  matches <- regexec(pattern, hrefs)
  versions <- vapply(
    regmatches(hrefs, matches),
    function(x) if (length(x) == 2) x[2] else NA_character_,
    character(1)
  )

  dates <- vapply(rows, function(row) {
    cells <- xml2::xml_find_all(row, "./td")
    trimws(xml2::xml_text(cells[[3]]))
  }, character(1))

  keep <- !is.na(versions)
  released_at <- format_cran_timestamp(dates[keep])
  cran_versions_data(
    version = versions[keep],
    released = as.Date(released_at),
    released_at = released_at,
    current = FALSE,
    tarball_url = paste0(
      "https://cran.r-project.org/src/contrib/Archive/",
      package_name,
      "/",
      hrefs[keep]
    )
  )
}

cran_archive_page <- function(package_name) {
  xml2::read_html(
    sprintf(
      "https://cran.r-project.org/src/contrib/Archive/%s/",
      utils::URLencode(package_name, reserved = TRUE)
    )
  )
}

cran_current_version <- function(package_name) {
  packages <- utils::available.packages(repos = "https://cran.r-project.org")
  if (!package_name %in% rownames(packages)) {
    return(cran_versions_data())
  }

  released_at <- format_cran_timestamp(packages[package_name, "Published"])
  cran_versions_data(
    version = packages[package_name, "Version"],
    released = as.Date(released_at),
    released_at = released_at,
    current = TRUE,
    tarball_url = sprintf(
      "https://cran.r-project.org/src/contrib/%s_%s.tar.gz",
      package_name,
      packages[package_name, "Version"]
    )
  )
}

format_cran_timestamp <- function(x) {
  format(
    as.POSIXct(x, tz = "UTC"),
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
}

cran_versions_data <- function(
  version = character(),
  released = as.Date(character()),
  released_at = format_cran_timestamp(released),
  current = FALSE,
  tarball_url = NA_character_
) {
  n <- length(version)
  data.frame(
    version = as.character(version),
    released = rep_len(as.Date(released), n),
    released_at = rep_len(as.character(released_at), n),
    current = rep_len(as.logical(current), n),
    tarball_url = rep_len(as.character(tarball_url), n),
    stringsAsFactors = FALSE
  )
}

.btw_add_to_tools(
  name = "btw_tool_cran_versions",
  group = "cran",
  alias_group = "search",
  tool = function() {
    ellmer::tool(
      btw_tool_cran_versions_impl,
      name = "btw_tool_cran_versions",
      description = paste(
        "List a CRAN package's release versions and dates.",
        "Includes the current CRAN release and versions in the CRAN archive."
      ),
      annotations = ellmer::tool_annotations(
        title = "CRAN Package Releases",
        read_only_hint = TRUE,
        open_world_hint = TRUE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The name of a package on CRAN.",
          required = TRUE
        ),
        after = ellmer::type_string(
          "Only return releases on or after this ISO date (YYYY-MM-DD).",
          required = FALSE
        ),
        before = ellmer::type_string(
          "Only return releases on or before this ISO date (YYYY-MM-DD).",
          required = FALSE
        )
      )
    )
  }
)

.btw_add_to_tools(
  name = "btw_tool_cran_package",
  group = "cran",
  alias_group = "search",
  alias_name = "btw_tool_search_package_info",
  tool = function() {
    ellmer::tool(
      btw_tool_cran_package_impl,
      name = "btw_tool_cran_package",
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
