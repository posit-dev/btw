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
#' btw_register_tools(ch)
#' }
#'
#' @family Tools
#' @export
btw_register_tools <- function(chat, tools = NULL) {
  check_inherits(chat, "Chat")

  tool_names <- map_chr(.btw_tools, function(x) x$name)
  tool_groups <- map_chr(.btw_tools, function(x) x$group)

  allowed <- c(
    tool_names,
    tool_groups,
    sub("btw_tool_", "", tool_names, fixed = TRUE)
  )

  has_tools <- !is.null(tools)

  if (has_tools) {
    tools <- arg_match(tools, allowed, multiple = TRUE)
  }

  for (tool in .btw_tools) {
    if (has_tools && !tool_matches(tool, tools)) next
    if (has_tools) {
      cli::cli_alert_success("Registered tool {.field {tool$name}}")
    }

    # Tools are stored as functions to avoid creating them at build-time
    tool <- tool$tool()

    # and some may return `NULL` if disabled or contextually unavailable
    if (is.null(tool)) next

    chat$register_tool(tool)
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
