#' Tool: Describe R package documentation
#'
#' @description
#' These functions describe package documentation in plain text:
#'
#' @param package_name The name of the package as a string, e.g. `"shiny"`.
#' @param topic The `topic_id` or `alias` of the help page, e.g. `"withProgress"`
#' or `"incProgress"`. Find `topic_id`s or `alias`es using `get_package_help()`.
#' @param vignette The name (or index) of the vignette to retrieve. Defaults to
#' the "intro" vignette to the package (by the same rules as pkgdown.)
#'
#' @returns
#' * `btw_tool_docs_package_help_topics()` returns the `topic_id`, `title`, and
#'   `aliases` fields for every topic in a package's documentation as a
#'   json-formatted string.
#' * `btw_tool_docs_help_page()` return the help-page for a package topic as a
#'   string.
#'
#' @seealso [btw_register_tools()]
#'
#' @examples
#' cat(btw_tool_docs_package_help_topics("btw"))
#'
#' cat(btw_tool_docs_help_page("btw", "btw"))
#'
#' # show the TOC of vignettes in the dplyr package
#' cat(btw_tool_docs_available_vignettes("dplyr"))
#'
#' # returns a whole bunch of output and relies on
#' # dplyr to have the mentioned vignettes available
#' \dontrun{
#' # grab the intro vignette
#' cat(btw_tool_docs_vignette("dplyr"))
#'
#' # grab the programming vignette specifically
#' cat(btw_tool_docs_vignette("dplyr", "programming"))
#' }
#'
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

  btw_tool_env_describe_data_frame(res, format = "json", dims = c(Inf, Inf))
}


.btw_add_to_tools(
  name = "btw_tool_docs_package_help_topics",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_package_help_topics,
      .description = "Get available help topics for an R package.",
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

  help_page <- inject(help(
    package = !!package_name,
    topic = !!topic,
    help_type = "text",
    try.all.packages = FALSE
  ))

  pager_result <- NULL
  pager <- function(files, header, title, delete.file) {
    str <- readLines(files, warn = FALSE)
    str <- gsub("_\b", "", str)
    if (isTRUE(delete.file)) {
      unlink(files)
    }
    pager_result <<- str
    invisible()
  }

  withr::with_options(list(pager = pager), {
    print(help_page)
  })

  return(pager_result)
}

.btw_add_to_tools(
  name = "btw_tool_docs_help_page",
  group = "docs",
  tool = function() {
    ellmer::tool(
      btw_tool_docs_help_page,
      .description = "Get help page from package.",
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

  tmp_file <- withr::local_tempfile(fileext = ".md")
  rmarkdown::pandoc_convert(
    file.path(vignette_info$Dir, "doc", vignette_info$PDF),
    from = "html",
    to = "markdown",
    output = tmp_file
  )

  readLines(tmp_file)
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
