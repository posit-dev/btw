#' Describe installed packages
#'
#' @description
#' Displays the name and title of all installed R packages in json format.
#'
#' @section See Also:
#' * [register_btw_tools()] registers this function as a tool for LLMs to call.
#' * Other `get_*()` functions: `r paste0('[', purrr::map_chr(btw_tools, purrr::pluck, "name"), '()]')`
#'
#' @returns
#' A single string describing installed packages in json.
#'
#' @examples
#' cat(get_installed_packages())
#'
#' @export
get_installed_packages <- function() {
  fields <- c("Package", "Title") # , "Description")
  df <- as.data.frame(installed.packages(fields = fields))[, fields]

  # show every installed package, along with a description of what
  # it does, in json format
  get_data_frame(df, format = "json", dims = c(Inf, 2))
}

tool_get_installed_packages <- function() {
  ellmer::tool(
    get_installed_packages,
    "Displays the name and title of all installed R packages in json format."
  )
}

#' Describe R package documentation
#'
#' @description
#' These functions describe package documentation in plain text:
#'
#' @param package_name The name of the package as a string, e.g. `"shiny"`.
#' @param topic The `topic_id` or `alias` of the help page, e.g. `"withProgress"`
#' or `"incProgress"`. Find `topic_id`s or `alias`es using `get_package_help()`.
#'
#' @returns
#' * `get_package_help()` returns the `topic_id`, `title`, and `aliases` fields
#'   for every topic in a package's documentation as a json-formatted string.
#' * `get_help_page()` return the help-page for a package topic as a string.
#'
#' @inheritSection get_installed_packages See Also
#'
#' @examples
#' cat(get_package_help("btw"))
#'
#' cat(get_help_page("btw", "btw"))
#'
#' @name get_pkg
#' @export
get_package_help <- function(package_name) {
  if (!package_name %in% installed.packages()[,"Package"]) {
    cli::cli_abort("Package {.pkg {package_name}} is not installed.")
  }

  help_db <- help.search(
    "",
    package = package_name,
    fields = c("alias", "title"),
    ignore.case = TRUE
  )

  res <- help_db$matches |>
    dplyr::group_by(Name) |>
    dplyr::summarize(
      topic_id = dplyr::first(Name),
      title = dplyr::first(Entry[Field == "Title"]),
      aliases = list(I(Entry[Field == "alias"]))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(topic_id, title, aliases)

  get_data_frame(res, format = "json", dims = c(Inf, Inf))
}

tool_get_package_help <- function() {
  ellmer::tool(
    get_package_help,
    "Get available help topics for an R package.",
    package_name = ellmer::type_string(
      "The exact name of the package, e.g. \"shiny\"."
    )
  )
}

# TODO: should this run the examples so the model can see what it does?
# TODO: should there just be a way to get examples?
#' @rdname get_pkg
#' @export
get_help_page <- function(package_name, topic) {
  if (!package_name %in% installed.packages()[,"Package"]) {
    cli::cli_abort("Package {.pkg {package_name}} is not installed.")
  }

  help_page <- rlang::inject(help(
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
    pager_result <<- paste(str, collapse = "\n")
    invisible()
  }

  withr::with_options(list(pager = pager), {
    print(help_page)
  })

  return(pager_result)
}

tool_get_help_page <- function() {
  ellmer::tool(
    get_help_page,
    "Get help page from package.",
    package_name = ellmer::type_string(
      "The exact name of the package, e.g. 'shiny'."
    ),
    topic = ellmer::type_string(
      "The topic_id or alias of the help page, e.g. 'withProgress' or 'incProgress'."
    )
  )
}
