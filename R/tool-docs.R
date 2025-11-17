#' @include tool-result.R
NULL

#' Tool: Describe R package documentation
#'
#' @description
#' These functions describe package documentation in plain text.
#'
#' ## Additional Examples
#'
#' Show a list of available vignettes in the `dplyr` package:
#'
#' ```r
#' btw_tool_docs_available_vignettes("dplyr")
#' ```
#'
#' Get the introductory vignette for the `dplyr` package:
#'
#' ```r
#' btw_tool_docs_vignette("dplyr")
#' ```
#'
#' Get a specific vignette, such as the programming vignette for the `dplyr`
#' package:
#'
#' ```r
#' btw_tool_docs_vignette("dplyr", "programming")
#' ```
#'
#' @examplesIf rmarkdown::pandoc_available()
#' btw_tool_docs_package_help_topics("btw")
#'
#' btw_tool_docs_help_page("btw", "btw")
#'
#' @param package_name The name of the package as a string, e.g. `"shiny"`.
#' @param topic The `topic_id` or `alias` of the help page, e.g.
#'   `"withProgress"` or `"incProgress"`. Find `topic_id`s or `alias`es using
#'   `get_package_help()`.
#' @param vignette The name (or index) of the vignette to
#'   retrieve. Defaults to the "intro" vignette to the package (by the same
#'   rules as pkgdown.)
#' @inheritParams btw_tool_docs_package_news
#'
#' @returns
#' * `btw_tool_docs_package_help_topics()` returns the `topic_id`, `title`, and
#'   `aliases` fields for every topic in a package's documentation as a
#'   json-formatted string.
#' * `btw_tool_docs_help_page()` returns the help-page for a package topic as a
#'   string.
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @name btw_tool_package_docs
#' @export
btw_tool_docs_package_help_topics <- function(package_name, `_intent`) {}

btw_tool_docs_package_help_topics_impl <- function(package_name) {
  check_installed(package_name)

  help_db <- help.search(
    "",
    package = package_name,
    fields = c("alias", "title"),
    ignore.case = TRUE
  )

  res <- help_db$matches
  res <- dplyr::group_by(res, Name)
  res <- dplyr::summarize(
    res,
    topic_id = dplyr::first(Name),
    title = dplyr::first(Entry[Field == "Title"]),
    aliases = list(I(Entry[Field == "alias"]))
  )
  res <- dplyr::ungroup(res)
  res <- dplyr::select(res, topic_id, title, aliases)

  ret <- btw_tool_env_describe_data_frame_impl(
    res,
    format = "json",
    max_rows = Inf,
    max_cols = Inf
  )
  ret@extra$display <- list(
    title = sprintf("{%s} Help Topics", package_name),
    markdown = md_table(res)
  )
  ret
}


.btw_add_to_tools(
  name = "btw_tool_docs_package_help_topics",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_package_help_topics_impl,
      name = "btw_tool_docs_package_help_topics",
      description = "Get available help topics for an R package.",
      annotations = ellmer::tool_annotations(
        title = "Package Help Topics",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The exact name of the package, e.g. \"shiny\"."
        )
      )
    )
  }
)

# TODO: should this run the examples so the model can see what it does?
# TODO: should there just be a way to get examples?
#' @name btw_tool_package_docs
#' @export
btw_tool_docs_help_page <- function(topic, package_name, `_intent`) {}

btw_tool_docs_help_page_impl <- function(topic, package_name = "") {
  if (identical(package_name, "")) {
    package_name <- NULL
  }

  if (!is.null(package_name)) {
    check_installed(package_name)
  }

  withr::local_options(list(menu.graphics = FALSE))

  help_page <- inject(help(
    package = !!package_name,
    topic = !!topic,
    help_type = "text",
    try.all.packages = !!(is.null(package_name))
  ))

  if (!length(help_page)) {
    cli::cli_abort(c(
      paste0(
        "No help page found for topic {.val {topic}}",
        if (!is.null(package_name)) {
          " in package {.pkg {package_name}}"
        } else {
          " in all installed packages"
        },
        "."
      ),
      "i" = if (!is.null(package_name)) {
        "To search in all packages, call `btw_tool_docs_help_page()` with an empty string for {.code package_name}."
      }
    ))
  }

  resolved <- help_package_topic(help_page)

  if (length(resolved$resolved) > 1) {
    calls <- sprintf(
      '{"topic":"%s", "package_name":"%s"}',
      resolved$resolved,
      resolved$package
    )
    calls <- set_names(calls, "*")
    cli::cli_abort(c(
      "Topic {.val {topic}} matched {length(resolved$resolved)} different topics.",
      "i" = "Choose one or submit individual tool calls for each topic.",
      cli_escape(calls)
    ))
  }

  md <- format_help_page_markdown(
    help_page,
    options = c("--shift-heading-level-by=1")
  )

  # Remove up to the first empty line
  first_empty <- match(TRUE, !nzchar(md), nomatch = 1) - 1
  if (first_empty > 0) {
    md <- md[-seq_len(first_empty)]
  }

  heading <- sprintf(
    "## `help(package = \"%s\", \"%s\")`",
    resolved$package,
    topic
  )

  help_call <- format(call2("::", sym(resolved$package), sym(topic)))

  BtwHelpPageToolResult(
    value = c(heading, md),
    extra = list(
      help_text = md,
      topic = basename(resolved$topic),
      package = resolved$package,
      display = list(
        title = HTML(sprintf('<code>?%s</code>', help_call)),
        markdown = paste(md, collapse = "\n")
      )
    )
  )
}

BtwHelpPageToolResult <- S7::new_class(
  "BtwHelpPageToolResult",
  parent = BtwToolResult
)

help_package_topic <- function(help_page) {
  if (inherits(help_page, "dev_topic")) {
    # Assuming that dev topics are scalar (`?btw::btw_app` in dev workspace)
    return(list(
      topic = help_page$topic,
      resolved = help_page$path,
      package = help_page$pkg
    ))
  }

  # help() mainly returns a path to the un-aliased help topic
  # help("promise"): .../library/promises/help/promise
  # help("mutate_if", "dplyr"): .../library/dplyr/help/mutate_all
  topic <- attr(help_page, "topic", exact = TRUE)

  help_path <- as.character(help_page)

  # In the case where there are multiple matches, sort them so that the
  # raised error is deterministic (#55)
  package <- basename(dirname(dirname(help_path)))
  sort_indices <- rank(package, ties.method = "first")

  list(
    topic = rep_along(topic, help_path),
    resolved = basename(help_path)[sort_indices],
    package = if (length(package)) package[sort_indices]
  )
}

help_to_rd <- function(help_page) {
  check_inherits(help_page, c("help_files_with_topic", "dev_topic"))

  if (inherits(help_page, "dev_topic")) {
    rd_path <- help_page$path
    return(tools::parse_Rd(rd_path))
  }

  help_path <- as.character(help_page)
  rd_name <- basename(help_path)
  rd_package <- basename(dirname(dirname(help_path)))
  tools::Rd_db(rd_package)[[paste0(rd_name, ".Rd")]]
}

format_help_page_markdown <- function(
  help_page,
  ...,
  to = "markdown_strict+pipe_tables+backtick_code_blocks"
) {
  rd_obj <- help_to_rd(help_page)
  tmp_rd_file <- withr::local_tempfile()

  tools::Rd2HTML(rd_obj, out = tmp_rd_file)

  # Simplify HTML tables before converting to markdown
  html <- readLines(tmp_rd_file)
  html <- simplify_help_page_arguments(html)

  tmp_simplified <- withr::local_tempfile()
  writeLines(html, tmp_simplified)

  pandoc_convert(
    tmp_simplified,
    to = to,
    ...
  )
}

format_help_page_html <- function(help_page) {
  rd_obj <- help_to_rd(help_page)
  tmp_rd_file <- withr::local_tempfile()

  html <- tools::Rd2HTML(rd_obj, out = tmp_rd_file)
  readLines(html)
}

format_help_page_text <- function(help_page) {
  rd_obj <- help_to_rd(help_page)
  tmp_rd_file <- withr::local_tempfile()

  rd_opts <- tools::Rd2txt_options(
    itemBullet = "* ",
    showURLs = TRUE,
    underline_titles = FALSE
  )
  withr::defer(tools::Rd2txt_options(rd_opts))

  tools::Rd2txt(
    rd_obj,
    out = tmp_rd_file,
    outputEncoding = "utf-8"
  )
  readLines(tmp_rd_file)
}

.btw_add_to_tools(
  name = "btw_tool_docs_help_page",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_help_page_impl,
      name = "btw_tool_docs_help_page",
      description = "Get help page from package.",
      annotations = ellmer::tool_annotations(
        title = "Help Page",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The exact name of the package, e.g. 'shiny'. Can be an empty string to search for a help topic across all packages."
        ),
        topic = ellmer::type_string(
          "The topic_id or alias of the help page, e.g. 'withProgress' or 'incProgress'."
        )
      )
    )
  }
)

#' Simplify help page argument tables to heading format
#'
#' Converts argument tables in the Arguments section to heading format.
#' Each parameter becomes a heading with its description preserved in full,
#' including multi-paragraph descriptions and lists. This avoids pandoc's
#' issues with complex table content while maintaining full information.
#'
#' @param html Character vector of HTML content
#' @return Character vector with simplified argument tables
#' @noRd
simplify_help_page_arguments <- function(html) {
  doc <- xml_from_html(html)

  if (is.null(doc)) {
    return(html)
  }

  # Find the Arguments section heading
  # Note: R help HTML h3 tags don't have id attributes, so we match by text
  args_heading <- xml2::xml_find_first(
    doc,
    "//h3[normalize-space(text())='Arguments']"
  )
  if (length(args_heading) == 0 || is.na(args_heading)) {
    # No Arguments section found
    return(html)
  }

  # Find the table immediately following the Arguments heading
  # We only want to transform the first table under Arguments section
  args_table <- xml2::xml_find_first(
    args_heading,
    "following-sibling::table[@role='presentation'][1]"
  )

  if (length(args_table) == 0 || is.na(args_table)) {
    # No table found in Arguments section
    return(html)
  }

  rows <- xml2::xml_find_all(args_table, ".//tr")
  if (length(rows) == 0) {
    return(html)
  }

  # Extract parameter name and description from each row
  items <- map(rows, function(row) {
    cells <- xml2::xml_find_all(row, ".//td")
    if (length(cells) < 2) {
      return(NULL)
    }

    # First cell contains parameter name
    param_node <- xml2::xml_find_first(cells[[1]], ".//code")
    if (is.na(param_node)) {
      return(NULL)
    }
    param <- xml2::xml_text(param_node)

    # Second cell contains description - preserve full HTML structure
    # This includes lists, multiple paragraphs, code blocks, etc.
    children <- xml2::xml_children(cells[[2]])
    if (length(children) > 0) {
      description <- paste(map_chr(children, as.character), collapse = "\n")
    } else {
      # Fallback for simple text content
      description <- paste0("<p>", xml2::xml_text(cells[[2]]), "</p>")
    }

    list(param = param, description = description)
  })

  items <- discard(items, is.null)
  if (length(items) == 0) {
    return(html)
  }

  # Build heading structure
  # Use h4 because btw_tool_docs_help_page() shifts heading levels by +1,
  # so h4 becomes h5 (#####) in the final markdown
  replacement <- map_chr(
    items,
    function(item) {
      sprintf(
        "<h4><code>%s</code></h4>\n%s",
        item$param,
        as.character(item$description)
      )
    }
  )

  replacement <- sprintf("<div>%s</div>", paste(replacement, collapse = "\n\n"))
  xml2::xml_replace(args_table, xml2::read_html(replacement))

  as.character(doc)
}

#' @name btw_tool_package_docs
#' @export
btw_tool_docs_available_vignettes <- function(package_name, `_intent`) {}

btw_tool_docs_available_vignettes_impl <- function(package_name) {
  check_installed(package_name)

  vignettes <- as.data.frame(tools::getVignetteInfo(package = package_name))
  if (nrow(vignettes) == 0) {
    cli::cli_abort("Package {.pkg {package_name}} has no vignettes.")
  }

  df <- vignettes[, c("Topic", "Title")]
  names(df) <- c("vignette", "title") # Named to match vignette tool

  btw_tool_result(
    value = strsplit(as_json_rowwise(df), "\n")[[1]],
    data = df,
    display = list(
      title = sprintf("{%s} Vignettes", package_name),
      markdown = md_table(df)
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_docs_available_vignettes",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_available_vignettes_impl,
      name = "btw_tool_docs_available_vignettes",
      description = paste(
        "List available vignettes for an R package.",
        "Vignettes are articles describing key concepts or features of an R package.",
        "Returns the listing as a JSON array of `vignette` and `title`.",
        "To read a vignette, use `btw_tool_docs_vignette(package_name, vignette)`."
      ),
      annotations = ellmer::tool_annotations(
        title = "Available Vignettes",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The exact name of the package, e.g. 'shiny'."
        )
      )
    )
  }
)

#' @name btw_tool_package_docs
#' @export
btw_tool_docs_vignette <- function(package_name, vignette, `_intent`) {}

btw_tool_docs_vignette_impl <- function(
  package_name,
  vignette = package_name
) {
  check_installed(package_name)
  check_string(vignette, allow_null = TRUE)

  vignettes <- as.data.frame(tools::getVignetteInfo(package = package_name))
  if (nrow(vignettes) == 0) {
    cli::cli_abort("Package {.pkg {package_name}} has no vignettes.")
  }

  vignette_info <- vignettes[vignettes$Topic == vignette, , drop = FALSE]
  if (nrow(vignette_info) == 0) {
    cli::cli_abort(
      "No vignette {.val {vignette}} for package {.pkg {package_name}} found."
    )
  }

  html_vignette <- pandoc_convert(
    file.path(vignette_info$Dir, "doc", vignette_info$PDF),
    to = "html"
  )
  md_vignette <- pandoc_html_simplify(html_vignette)

  btw_tool_result(
    md_vignette,
    data = vignette_info,
    display = list(
      title = sprintf("{%s} Vignette: %s", package_name, vignette_info$Title),
      markdown = paste(md_vignette, collapse = "\n")
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_docs_vignette",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_vignette_impl,
      name = "btw_tool_docs_vignette",
      description = "Get a package vignette in plain text.",
      annotations = ellmer::tool_annotations(
        title = "Vignette",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The exact name of the package, e.g. 'shiny'."
        ),
        vignette = ellmer::type_string(
          "The name or index of the vignette to retrieve. This is optional; if you
      do not provide a value, the function retrieves the introductory vignette
      for the package.",
          required = FALSE
        )
      )
    )
  }
)
