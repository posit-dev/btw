#' @include tool-result.R
NULL

#' Tool: Package Release Notes
#'
#' @description
#' Include release notes for a package, either the release notes for the most
#' recent package release or release notes matching a search term.
#'
#' @examplesIf rmarkdown::pandoc_available()
#' # Copy release notes to the clipboard for use in any AI app
#' btw("@news dplyr", clipboard = FALSE)
#'
#' btw("@news dplyr join_by", clipboard = FALSE)
#'
#' if (interactive()) { # can be slow
#'   if (R.version$major == 4 && R.version$minor > "2.0") {
#'     # Search through R's release notes.
#'     # This should find a NEWS entry from R 4.2
#'     btw("@news R dynamic rd content", clipboard = FALSE)
#'   }
#' }
#'
#' # Tool use by LLMs via ellmer or MCP ----
#' btw_tool_docs_package_news("dplyr")
#'
#' btw_tool_docs_package_news("dplyr", "join_by")
#'
#' @param package_name The name of the package as a string, e.g. `"shiny"`.
#' @param search_term A regular expression to search for in the NEWS entries.
#'   If empty, the release notes of the current installed version is included.
#' @param _intent An optional string describing the intent of the tool use.
#'   When the tool is used by an LLM, the model will use this argument to
#'   explain why it called the tool.
#'
#' @returns Returns the release notes for the currently installed version of the
#'   package, or the release notes matching the search term.
#'
#' @seealso [btw_tools()]
#' @family docs tools
#' @export
#' @rdname btw_tool_docs_package_news
btw_tool_docs_package_news <- function(package_name, search_term, `_intent`) {}

btw_tool_docs_package_news_impl <- function(package_name, search_term = "") {
  news <- package_news_search(package_name, search_term %||% "")

  if (nrow(news) == 0) {
    if (nzchar(search_term)) {
      cli::cli_abort(
        "No NEWS entries found for package '{package_name}' matching '{search_term}'."
      )
    } else {
      cli::cli_abort(
        "No NEWS entries found for package '{package_name}' v{package_version(package_name)}."
      )
    }
  }

  result <- unclass(btw_this(news))

  BtwPackageNewsToolResult(
    result,
    extra = list(display = list(markdown = result))
  )
}

.btw_add_to_tools(
  "btw_tool_docs_package_news",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_package_news_impl,
      name = "btw_tool_docs_package_news",
      description = paste0(
        "Read the release notes (NEWS) for a package.",
        "\n\n",
        "Use this tool when you need to learn what changed in a package release, i.e. when code no longer works after a package update, or when the user asks to learn about new features.",
        "\n\n",
        "If no search term is provided, the release notes for the current installed version are returned. ",
        "If a search term is provided, the tool returns relevant entries in the NEWS file matching the search term from the most recent 5 versions of the package where the term is matched.",
        "\n\n",
        "Use a search term to learn about recent changes to a function, feature or argument over the last few package releases. ",
        "For example, if a user recently updated a package and asks why a function no longer works, you can use this tool to find out what changed in the package release notes."
      ),
      annotations = ellmer::tool_annotations(
        title = "Package Release Notes",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The name of the package.",
          required = TRUE
        ),
        search_term = ellmer::type_string(
          paste(
            "A regular expression to use to search the NEWS entries.",
            "Use simple regular expressions (perl style is supported).",
            "The search term is case-insensitive.",
            "If empty, the tool returns the release notes for the current installed version."
          ),
          required = FALSE
        )
      )
    )
  }
)

BtwPackageNewsToolResult <- S7::new_class(
  "BtwPackageNewsToolResult",
  parent = BtwToolResult
)

#' @export
btw_this.news_db <- function(x, ...) {
  news <- x
  package_name <- attr(x, "package")

  if (!"match" %in% names(x)) {
    news$match <- news$HTML
  }

  if (!inherits(news, "btw_filtered_news_db")) {
    news <- news[news$Version == package_version(package_name), ]
  }

  if (!nrow(news)) {
    return(btw_ignore())
  }

  news$Category[news$Category == "Full changelog"] <- ""

  news <- dplyr::summarize(
    news,
    md = paste(.data$match, collapse = "\n\n"),
    .by = c("Version", "Category")
  )
  news$md <- pandoc_convert_text(news$md, to = "markdown")

  has_cat <- nzchar(news$Category)
  news$Category[has_cat] <- paste0("#### ", news$Category[has_cat], "\n\n")

  news <- dplyr::summarize(
    news,
    md = paste(paste0(.data$Category, .data$md), collapse = "\n\n"),
    .by = "Version"
  )

  news <- news[order(news$Version, decreasing = TRUE), ]
  news$Version <- as.character(news$Version)

  news_md <- glue_(
    "
### {{package_name}} v{{news$Version}}

{{news$md}}",
  )

  # Returns as-is so that btw(news(package = package_name)) is treated as
  # pre-formatted text and not formatted as an object/result pair
  I(paste(news_md, collapse = "\n\n"))
}

package_news <- function(package_name) {
  news <- utils::news(package = package_name)
  if (package_name %in% r_docs_versions()) {
    news$Version <- map_chr(news$Version, as_package_or_r_version)
  }
  news
}

r_docs_versions <- function() {
  c("R", sprintf("R-%d", seq_len(R.version$major)))
}

package_news_search <- function(package_name, search_term = "") {
  r_docs <- r_docs_versions()
  if (!package_name %in% r_docs) {
    check_installed(package_name)
  } else {
    if (package_name == sprintf("R-%s", R.version$major)) {
      package_name <- "R"
    }
  }

  news <- package_news(package_name)
  if (is.null(news)) {
    return(data.frame())
  }
  news$Version <- base::package_version(news$Version)

  if (!nzchar(search_term)) {
    version <-
      if (!package_name %in% setdiff(r_docs, "R")) {
        package_version(package_name)
      } else {
        max(news$Version)
      }
    news <- news[news$Version == version, ]
    news$match <- news$HTML
  } else {
    news$match <- map_chr(
      news$HTML,
      extract_relevant_news,
      search_term = search_term
    )
    news <- news[!is.na(news$match), ]

    # Take at most the results from the 5 most recent versions
    versions <- unique(news$Version)
    if (length(versions) > 5) {
      versions <- sort(versions, decreasing = TRUE)[1:5]
    }
    news <- news[news$Version %in% versions, ]
  }

  class(news) <- c("btw_filtered_news_db", class(news))
  news
}

as_package_or_r_version <- function(v) {
  if (!grepl("[^\\d.-]", v)) {
    return(v)
  }

  if (identical(v, "R-devel")) {
    # Assuming the presence of `R-devel` means we're using dev R
    return(package_version("R"))
  }

  if (grepl("patched", v)) {
    # Remove " patched" suffix
    v <- sub(" patched", "", v, fixed = TRUE)
    v <- unclass(base::package_version(v))[[1]]
    v[3] <- v[3] + 1L
    return(paste(v, collapse = "."))
  }

  v
}

extract_relevant_news <- function(news_html, search_term) {
  if (!nzchar(news_html)) {
    return(NA_character_)
  }

  doc <- xml2::read_html(news_html)

  # Find all first-level <li> elements and top-level <p> elements
  # First-level <li> are direct children of <ul> or <ol>
  # Top-level <p> are direct children of the root (not nested in <li>)

  li_elements <- xml2::xml_find_all(
    doc,
    "//ul[not(ancestor::ul) and not(ancestor::ol)]/li"
  )
  top_level_p <- xml2::xml_find_all(
    doc,
    "//p[not(ancestor::li)]"
  )

  all_elements <- c(li_elements, top_level_p)

  has_text <- map_lgl(all_elements, function(el) {
    grepl(search_term, xml2::xml_text(el), perl = TRUE, ignore.case = TRUE)
  })

  if (!any(has_text)) {
    return(NA_character_)
  }

  res <- map_chr(all_elements[has_text], as.character)
  is_li <- grepl("^<li>", res, fixed = TRUE)
  res[is_li] <- paste0("<ul>", res[is_li], "</ul>")
  paste(res, collapse = "\n")
}
