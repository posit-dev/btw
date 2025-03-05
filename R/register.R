#' Tools: Register tools from btw
#'
#' @description
#' The `btw_register_tools()` function equips an ellmer chat to interface with
#' your computational environment. Chats returned by this function have access
#' to the tools:
#'
#' `r .docs_list_tools()`
#'
#' @param chat An ellmer `Chat` object.
#' @param include Names of tools or tool groups to include when registering
#'   tools, e.g. `include = "docs"` to include only the documentation related
#'   tools, or `include = c("data", "docs", "environment")`, etc.
#'
#' @returns Registers the tools with `chat`, updating the `chat` object in
#'   place. The `chat` input is returned invisibly.
#'
#' @examples
#' # requires an ANTHROPIC_API_KEY
#' \dontrun{
#' ch <- ellmer::chat_claude()
#'
#' btw_register_tools(ch)
#' }
#'
#' @family Tools
#' @export
btw_register_tools <- function(chat, include = NULL) {
  check_inherits(chat, "Chat")

  tool_names <- map_chr(.btw_tools, function(x) x$name)
  tool_groups <- map_chr(.btw_tools, function(x) x$group)

  allowed <- c(
    tool_names,
    tool_groups,
    sub("btw_tool_", "", tool_names, fixed = TRUE)
  )

  has_include <- !is.null(include)

  if (has_include) {
    include <- arg_match(include, allowed, multiple = TRUE)
  }

  for (tool in .btw_tools) {
    if (has_include && !tool_matches(tool, include)) next
    if (has_include) {
      cli::cli_alert_success("Registered tool {.field {tool$name}}")
    }
    chat$register_tool(tool$tool())
  }

  invisible(chat)
}

tool_matches <- function(tool, labels = NULL) {
  if (is.null(labels)) return(TRUE)
  if (tool$name %in% labels) return(TRUE)
  if (tool$group %in% labels) return(TRUE)
  if (sub("btw_tool_", "", tool$name) %in% labels) return(TRUE)
  FALSE
}

# nocov start
.docs_list_tools <- function() {
  x <- map(.btw_tools, function(tool) {
    desc <- strsplit(tool$tool()@description, ". ", fixed = TRUE)[[1]][1]
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
