#' Describe the contents of an environment
#'
#' @param environment An environment. Optional, defaults to global environment.
#' @param items Optional. A character vector of objects in the environment to
#' describe.
#'
#' @returns
#' A character vector describing the environment contents with `#>` prefixing
#' each object's printed representation.
#'
#' @export
get_environment <- function(environment = global_env(), items = NULL) {
  if (!is.environment(environment)) {
    # TODO: does the env name live in the global env?
    # is it in `search_envs`?
    cli::cli_abort("Not implemented yet.")
  }

  res <- character()
  env_item_names <- names(environment)
  if (!is.null(items)) {
    env_item_names <- env_item_names[env_item_names %in% items]
  }

  for (item_name in env_item_names) {
    item <- env_get(environment, item_name)

    if (inherits(item, "data.frame")) {
      item_desc <- get_data_frame(item)
    } else {
      item_desc <- capture.output(item)
    }

    item_res <- c(item_name, paste0("#> ", item_desc), "\n")
    res <- c(res, item_res)
  }

  res
}

tool_get_environment <- function() {
  ellmer::tool(
    get_environment,
    "Retrieve specified items from a given environment.",
    environment = ellmer::type_unknown(
      "The environment from which to retrieve items. Defaults to `global_env()`--
       use this function to learn about the objects in the user's environment."
    ),
    items = ellmer::type_array(
      "The names of items to retrieve from the environment. Defaults to `NULL`, indicating all 
  items.",
      items = ellmer::type_string()
    )
  )
}
