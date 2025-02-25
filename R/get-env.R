#' Describe the contents of an environment
#'
#' @examples
#' btw_this(globalenv())
#'
#' @param x An environment.
#' @param items Optional. A character vector of objects in the environment to
#'   describe.
#' @param ... Additional arguments are silently ignored.
#'
#' @returns
#' A string describing the environment contents with `#>` prefixing
#' each object's printed representation.
#'
#' @inheritSection get_installed_packages See Also
#'
#' @export
btw_this.environment <- function(x, ..., items = NULL) {
  btw_describe_environment(environment = x, items = items)
}

btw_describe_environment <- function(environment = global_env(), items = NULL) {
  if (!is.environment(environment)) {
    # TODO: does the env name live in the global env?
    # is it in `search_envs`?
    cli::cli_abort("Not implemented yet.")
  }

  if (is_namespace(environment)) {
    cli::cli_abort(c(
      "Describing an entire package namespace is not supported",
      "i" = "Try choosing specific functions to describe: {.code btw('dplyr::mutate', 'dplyr::across')}."
    ))
  }

  res <- character()
  env_item_names <- ls(environment)
  if (!is.null(items)) {
    env_item_names <- env_item_names[env_item_names %in% items]
  }

  for (item_name in env_item_names) {
    item <- env_get(environment, item_name)

    if (is_function(item) && is_namespace(fn_env(item))) {
      item <- item_name
    }

    res <- c(
      res,
      btw_item_with_description(
        item_name,
        btw_this(item, caller_env = environment)
      )
    )
  }

  res
}

tool_describe_environment <- .btw_add_to_tools(function() {
  ellmer::tool(
    btw_describe_environment,
    .name = "btw_list_and_describe_environment",
    .description = "List and describe items in an environment.",
    items = ellmer::type_array(
      "The names of items to describe from the environment. Defaults to `NULL`, indicating all items.",
      items = ellmer::type_string()
    )
  )
})

btw_item_with_description <- function(item_name, description) {
  if (inherits(description, "btw_ignore")) {
    return(invisible())
  }
  # assuming the new contract is that `btw_this()` returns `description` as
  # lines of text. (Alternative: btw_this() always returns a single character.)
  c(item_name, paste0("#> ", description), "\n")
}
