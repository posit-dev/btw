#' Describe the contents of an environment
#'
#' @examples
#' cyl_6 <- mtcars[mtcars$cyl == 6, ]
#' gear_5 <- mtcars[mtcars$gear == 5, ]
#' btw_this(environment())
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
#' @seealso [btw_tool_describe_environment()]
#'
#' @family `btw_this()` methods
#' @export
btw_this.environment <- function(x, ..., items = NULL) {
  btw_tool_describe_environment(environment = x, items = items)
}

#' Tool: Describe an environment
#'
#' @param environment An environment to describe.
#' @inheritParams btw_this.environment
#'
#' @inherit btw_this.environment return
#'
#' @seealso [btw_this.environment()], [btw_register_tools()]
#' @family Tools
#' @export
btw_tool_describe_environment <- function(
  environment = global_env(),
  items = NULL
) {
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
    # Subset to `items`, keeping the order of `items`
    env_item_names <- intersect(items, env_item_names)
  }

  item_desc_prev <- NULL

  item_desc <- map(env_item_names, function(item_name) {
    item <- env_get(environment, item_name)

    if (identical(class(item), "character")) {
      # Only string literals passed through btw() hit `btw_this.character()`.
      # We rely on `dots_list()` turning `"foo"` into `list('"foo"' = "foo")`.
      if (!identical(item_name, sprintf('"%s"', item))) {
        item <- btw_returns_character(item)
      }
    }

    btw_this(item, caller_env = environment)
  })

  res <- c()
  for (i in seq_along(item_desc)) {
    desc <- item_desc[[i]]
    name <- env_item_names[[i]]
    is_user_prompt <- inherits(desc, "btw_user_prompt")

    if (i == 1) {
      res <- c(
        if (!is_user_prompt) c("## Context", ""),
        btw_item_with_description(name, desc)
      )
      next
    }

    is_adjacent_user_prompt <-
      is_user_prompt &&
      inherits(item_desc[[i - 1]], "btw_user_prompt")

    is_adjacent_user_context <-
      !is_user_prompt &&
      inherits(item_desc[[i - 1]], "btw_user_prompt")

    if (is_adjacent_user_prompt) {
      # Append text to previous prompt text
      res[length(res)] <- paste0(res[length(res)], "\n", desc)
    } else {
      res <- c(
        res,
        "",
        btw_item_with_description(
          name,
          desc,
          header = if (is_adjacent_user_context) "## Context"
        )
      )
    }
  }

  if (identical(res, c("## Context", ""))) {
    return("")
  }

  res
}

.btw_add_to_tools(
  name = "btw_tool_describe_environment",
  group = "environment",
  tool = function() {
    ellmer::tool(
      btw_tool_describe_environment,
      .description = "List and describe items in an environment.",
      items = ellmer::type_array(
        "The names of items to describe from the environment. Defaults to `NULL`, indicating all items.",
        items = ellmer::type_string()
      )
    )
  }
)

btw_item_with_description <- function(item_name, description, header = NULL) {
  if (inherits(description, "AsIs")) {
    return(description)
  }
  if (inherits(description, "btw_user_prompt")) {
    return(c("## User", description))
  }
  if (inherits(description, "btw_ignore")) {
    return(invisible())
  }
  if (inherits(description, "btw_captured")) {
    item_name <- gsub('^"|"$', '', item_name)
    item_name <- switch(
      item_name,
      "@last_error" = "last_error()",
      item_name
    )

    description <- md_code_block("r", item_name, paste("#>", description))
    item_name <- NULL
  }

  if (!is.null(header)) header <- c(header, "")
  paste(c(header, item_name, description), collapse = "\n")
}
