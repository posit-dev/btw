#' Tools: Register tools from btw
#'
#' @description
#' The `btw_tools()` function provides a list of tools that can be registered
#' with an ellmer chat via `chat$set_tools()` that allow the chat to
#' interface with your computational environment. Chats returned by
#' this function have access to the tools:
#'
#' `r .docs_list_tools()`
#'
#' @param ... Optional names of tools or tool groups to include when registering
#'   tools. By default all btw tools are included. For example, use `"docs"` to
#'   include only the documentation related tools, or `"env", "docs",
#'   "session"` for the collection of environment, documentation and session
#'   tools, and so on.
#'
#'   The names provided can be:
#'
#'   1. The name of a tool, such as `"btw_tool_env_describe_data_frame"`.
#'   2. The name of a tool group, such as `"env"`, which will include all tools
#'      in that group.
#'   3. The tool name without the `btw_tool_` prefix, such as
#'      `"env_describe_data_frame"`.
#'
#' @returns Registers the tools with `chat`, updating the `chat` object in
#'   place. The `chat` input is returned invisibly.
#'
#' @examples
#' # requires an ANTHROPIC_API_KEY
#' \dontrun{
#' ch <- ellmer::chat_anthropic()
#'
#' # register all of the available tools
#' ch$set_tools(btw_tools())
#'
#' # or register only the tools related to fetching documentation
#' ch$set_tools(btw_tools("docs"))
#'
#' # ensure that the current tools persist
#' ch$set_tools(c(ch$get_tools(), btw_tools()))
#' }
#'
#' @family Tools
#' @export
btw_tools <- function(...) {
  tools <- c(...)
  check_character(tools, allow_null = TRUE)

  if (length(tools) == 0) {
    return(as_ellmer_tools(.btw_tools))
  }

  tool_names <- map_chr(.btw_tools, function(x) x$name)
  tool_groups <- map_chr(.btw_tools, function(x) x$group)

  allowed <- c(
    tool_groups,
    tool_names,
    sub("btw_tool_", "", tool_names, fixed = TRUE)
  )
  allowed <- unique(allowed)

  tools <- tryCatch(
    arg_match(tools, allowed[!grepl("^btw_", allowed)], multiple = TRUE),
    error = function(err_short) {
      tryCatch(
        arg_match(tools, allowed, multiple = TRUE),
        error = function(err_long) {
          class(err_short) <- c("btw_unmatched_tool_error", class(err_short))
          cnd_signal(err_short)
        }
      )
    }
  )

  tools_to_keep <- map_lgl(.btw_tools, is_tool_match, tools)
  res <- .btw_tools[tools_to_keep]

  as_ellmer_tools(res)
}

is_tool_match <- function(tool, labels = NULL) {
  if (is.null(labels)) {
    return(TRUE)
  }
  if (tool$name %in% labels) {
    return(TRUE)
  }
  if (tool$group %in% labels) {
    return(TRUE)
  }
  if (sub("btw_tool_", "", tool$name) %in% labels) {
    return(TRUE)
  }
  FALSE
}

# Convert from .btw_tools (or a filtered version of it)
# to a format compatible with `client$set_tools()`
as_ellmer_tools <- function(x) {
  tools <- compact(map(x, function(.x) .x$tool()))
  map(tools, wrap_with_intent)
}

wrap_with_intent <- function(tool) {
  if ("intent" %in% names(tool@arguments@properties)) {
    return(tool)
  }

  tool_fun <- tool@fun
  wrapped_tool <- new_function(
    c(fn_fmls(tool_fun), list(intent = "")),
    fn_body(tool_fun)
  )
  tool@fun <- wrapped_tool
  tool@arguments@properties$intent <- ellmer::type_string(
    paste(
      "The intent of the tool call that describes why you called this tool.",
      "This should be a single, short phrase that explains this tool call to the user."
    )
  )

  tool
}

# nocov start
.docs_list_tools <- function() {
  x <- map(.btw_tools, function(tool) {
    desc <- strsplit(S7::prop(tool$tool(), "description"), "[.]( |\n)")
    desc <- desc[[1]][1]

    if (!grepl("[.]$", desc)) {
      desc <- paste0(desc, ".")
    }
    data.frame(
      Name = tool$name,
      Group = tool$group,
      Description = desc,
      stringsAsFactors = FALSE,
      row.names = FALSE
    )
  })
  x <- do.call(rbind, x)
  x <- x[order(x$Group, x$Name), ]
  md_table(x)
}
# nocov end
