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
#' @param vignette The name (or index) of the vignette to retrieve. Defaults to
#' the "intro" vignette to the package (by the same rules as pkgdown.)
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
#' # show the TOC of vignettes in the dplyr package
#' cat(get_package_vignettes("dplyr"))
#' 
#' # returns a whole bunch of output and relies on
#' # dplyr to have the mentioned vignettes available
#' \dontrun{
#' # grab the intro vignette
#' cat(get_package_vignette("dplyr"))
#' 
#' # grab the programming vignette specifically
#' cat(get_package_vignette("dplyr", "programming"))
#' }
#' @name get_pkg
#' @export
get_package_help <- function(package_name) {
  check_installed(package_name)

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
  check_installed(package_name)

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

#' @rdname get_pkg
#' @export
get_package_vignettes <- function(package_name) {
  check_installed(package_name)

  vignettes <- as.data.frame(tools::getVignetteInfo(package = package_name))
  if (nrow(vignettes) == 0) {
    cli::cli_abort("Package {.pkg {package_name}} has no vignettes.")
  }

  df <- vignettes[, c("Topic", "Title")]
  get_data_frame(df, format = "json", dims = c(Inf, 2))
}

tool_get_package_vignettes <- function() {
  ellmer::tool(
    get_package_vignettes,
    "Get a table of available vignettes for an R package.",
    package_name = ellmer::type_string(
      "The exact name of the package, e.g. 'shiny'."
    )
  )
}

#' @rdname get_pkg
#' @export
get_package_vignette <- function(package_name, vignette = package_name) {
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

  rmarkdown::pandoc_convert(
    file.path(vignette_info$Dir, "doc", vignette_info$PDF),
    from = "html",
    to = "markdown"
  )
}

tool_get_package_vignette <- function() {
  ellmer::tool(
    get_package_vignette,
    "Get a package vignette in plain text.",
    package_name = ellmer::type_string(
      "The exact name of the package, e.g. 'shiny'."
    ),
    vignette = ellmer::type_string(
      "The name or index of the vignette to retrieve. This is optional--leave
      as default to retrieve the introductory vignette for the package."
    )
  )
}
