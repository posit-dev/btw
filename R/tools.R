#' Tools: Register tools from btw
#'
#' @description
#' The `btw_tools()` function provides a list of tools that can be registered
#' with an ellmer chat via `chat$set_tools()` that equip the chat to 
#' interface with your computational environment. Chats returned by this 
#' function have access to the tools:
#'
#' `r .docs_list_tools()`
#'
#' @param tools Optional names of tools or tool groups to include when
#'   registering tools. By default all btw tools are included. For example, use
#'   `tools = "docs"` to include only the documentation related tools, or
#'   `tools = c("env", "docs", "session")`, etc.
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
#' ch$set_tools(btw_tools(tools = "docs"))
#' }
#'
#' @family Tools
#' @export
btw_tools <- function(tools = NULL) {
  has_tools <- !is.null(tools)

  if (!has_tools) {
    return(as_ellmer_tools(.btw_tools))
  }

  tool_names <- map_chr(.btw_tools, function(x) x$name)
  tool_groups <- map_chr(.btw_tools, function(x) x$group)

  allowed <- c(
    tool_names,
    tool_groups,
    sub("btw_tool_", "", tool_names, fixed = TRUE)
  )

  tools <- arg_match(tools, allowed, multiple = TRUE)

  tools_to_keep <- vapply(.btw_tools, tool_matches, logical(1), tools)
  res <- .btw_tools[tools_to_keep]

  as_ellmer_tools(res)
}

tool_matches <- function(tool, labels = NULL) {
  if (is.null(labels)) return(TRUE)
  if (tool$name %in% labels) return(TRUE)
  if (tool$group %in% labels) return(TRUE)
  if (sub("btw_tool_", "", tool$name) %in% labels) return(TRUE)
  FALSE
}

# Convert from .btw_tools (or a filtered version of it)
# to a format compatible with `client$set_tools()`
as_ellmer_tools <- function(x) {
  res <- lapply(x, function(.x) {.x$tool()})
  is_null <- vapply(res, is.null, logical(1))
  res <- res[!is_null]
  set_names(res, names(x)[!is_null])
}

# nocov start
.docs_list_tools <- function() {
  x <- map(.btw_tools, function(tool) {
    desc <- strsplit(S7::prop(tool$tool(), "description"), ". ", fixed = TRUE)
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
