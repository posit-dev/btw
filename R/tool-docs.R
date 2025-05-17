#' @include tool-result.R
NULL

#' Tool: Describe R package documentation
#'
#' @description
#' These functions describe package documentation in plain text.
#'
#' @examples
#' btw_tool_docs_package_help_topics("btw")
#'
#' btw_tool_docs_help_page("btw", "btw")
#'
#' # show the TOC of vignettes in the dplyr package
#' btw_tool_docs_available_vignettes("dplyr")
#'
#' # returns a whole bunch of output and relies on
#' # dplyr to have the mentioned vignettes available
#' \dontrun{
#' # grab the intro vignette
#' btw_tool_docs_vignette("dplyr")
#'
#' # grab the programming vignette specifically
#' btw_tool_docs_vignette("dplyr", "programming")
#' }
#'
#' @param package_name The name of the package as a string, e.g. `"shiny"`.
#' @param topic The `topic_id` or `alias` of the help page, e.g.
#'   `"withProgress"` or `"incProgress"`. Find `topic_id`s or `alias`es using
#' `get_package_help()`. @param vignette The name (or index) of the vignette to
#'   retrieve. Defaults to the "intro" vignette to the package (by the same
#'   rules as pkgdown.)
#'
#' @returns
#' * `btw_tool_docs_package_help_topics()` returns the `topic_id`, `title`, and
#'   `aliases` fields for every topic in a package's documentation as a
#'   json-formatted string.
#' * `btw_tool_docs_help_page()` return the help-page for a package topic as a
#'   string.
#'
#' @seealso [btw_tools()]
#' @family Tools
#' @name btw_tool_package_docs
#' @export
btw_tool_docs_package_help_topics <- function(package_name) {
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

  btw_tool_env_describe_data_frame(
    res,
    format = "json",
    max_rows = Inf,
    max_cols = Inf
  )
}


.btw_add_to_tools(
  name = "btw_tool_docs_package_help_topics",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_package_help_topics,
      .description = "Get available help topics for an R package.",
      .annotations = ellmer::tool_annotations(
        title = "Package Help Topics",
        read_only_hint = TRUE,
        open_world_hint = FALSE
      ),
      package_name = ellmer::type_string(
        "The exact name of the package, e.g. \"shiny\"."
      )
    )
  }
)

# TODO: should this run the examples so the model can see what it does?
# TODO: should there just be a way to get examples?
#' @name btw_tool_package_docs
#' @export
btw_tool_docs_help_page <- function(topic, package_name = "") {
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
    try.all.packages = FALSE
  ))

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

  BtwHelpPageToolResult(
    value = c(heading, md),
    extra = list(
      help_text = md,
      topic = basename(resolved$topic),
      package = resolved$package
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
    package = package[sort_indices]
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

  pandoc_convert(
    tmp_rd_file,
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
      btw_tool_docs_help_page,
      .description = "Get help page from package.",
      .annotations = ellmer::tool_annotations(
        title = "Help Page",
        read_only_hint = TRUE,
        open_world_hint = FALSE
      ),
      package_name = ellmer::type_string(
        "The exact name of the package, e.g. 'shiny'. Can be an empty string to search for a help topic across all packages."
      ),
      topic = ellmer::type_string(
        "The topic_id or alias of the help page, e.g. 'withProgress' or 'incProgress'."
      )
    )
  }
)

#' @name btw_tool_package_docs
#' @export
btw_tool_docs_available_vignettes <- function(package_name) {
  check_installed(package_name)

  vignettes <- as.data.frame(tools::getVignetteInfo(package = package_name))
  if (nrow(vignettes) == 0) {
    cli::cli_abort("Package {.pkg {package_name}} has no vignettes.")
  }

  df <- vignettes[, c("Topic", "Title")]
  names(df) <- c("vignette", "title") # Named to match vignette tool
  strsplit(as_json_rowwise(df), "\n")[[1]]
}

.btw_add_to_tools(
  name = "btw_tool_docs_available_vignettes",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_available_vignettes,
      .description = paste(
        "List available vignettes for an R package.",
        "Vignettes are articles describing key concepts or features of an R package.",
        "Returns the listing as a JSON array of `vignette` and `title`.",
        "To read a vignette, use `btw_tool_docs_vignette(package_name, vignette)`."
      ),
      package_name = ellmer::type_string(
        "The exact name of the package, e.g. 'shiny'."
      )
    )
  }
)

#' @name btw_tool_package_docs
#' @export
btw_tool_docs_vignette <- function(
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

  pandoc_convert(file.path(vignette_info$Dir, "doc", vignette_info$PDF))
}

.btw_add_to_tools(
  name = "btw_tool_docs_vignette",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_vignette,
      .description = "Get a package vignette in plain text.",
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
  }
)
