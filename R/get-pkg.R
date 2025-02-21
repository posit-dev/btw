#' Get installed R packages
#' 
#' @param pattern Optional regular expression (case insensitive, PCRE syntax)
#'   to filter by. If not provided, all installed packages will be returned.
#' 
#' @return A dataframe of installed packages
#' 
#' @export
get_installed_packages <- function(pattern = NULL) {
  fields <- c("Package", "Title") # , "Description")
  df <- as.data.frame(installed.packages(fields = fields))[, fields]

  # show every installed package, along with a description of what
  # it does, in json format
  get_data_frame(df, format = "json", dims = c(Inf, 2))
}

tool_get_installed_packages <- function() {
  ellmer::tool(
    get_installed_packages,
    "Get a dataframe of installed R packages. If a pattern is provided,
  it filters the installed packages using a case insensitive regular
  expression.",
    pattern = ellmer::type_string(
      "Optional regular expression (case insensitive, PCRE syntax) to
  filter by. If not provided, all installed packages will be returned.
  Defaults to `NULL`.",
      required = FALSE
    )
  )
}

#' Get available help topics for an R package
#'
#' @param package_name The exact name of the package, e.g. "shiny"
#' 
#' @return JSON data of help topics, along with their aliases
#' 
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

#' Get help page from package
# TODO: should this run the examples so the model can see what it does?
# TODO: should there just be a way to get examples? 
# `get_function_help()` and `get_function_examples()`?
#'
#' @param package_name The exact name of the package, e.g. "shiny"
#' @param topic The topic_id or alias of the help page, e.g. "withProgress" or
#'   "incProgress"
#' @return The help page, in plain text format
#' 
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
