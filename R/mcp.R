#' Start a Model Context Protocol server with btw tools
#'
#' @description
#' `btw_mcp_server()` starts an MCP server with tools from [btw_tools()], which
#' can provide MCP clients like Claude Desktop or Claude Code with additional
#' context. The function will block the R process it's called in and isn't
#' intended for interactive use.
#'
#' To give the MCP server access to a specific R session, run `btw_mcp_session()`
#' in that session. If there are no sessions configured, the server will run
#' the tools in its own session, meaning that e.g. the
#' `btw_tools(tools = "env")` tools will describe R objects in _that_ R
#' environment.
#'
#' @seealso
#' These functions use [acquaint::mcp_server()] and [acquaint::mcp_session()]
#' under the hood. To configure arbitrary tools with an MCP client, see the
#' documentation of those functions.
#'
#' @section Configuration:
#' To configure this server with MCP clients, use the command `Rscript` and the
#' args `-e "btw::btw_mcp_server()"`. For example, in Claude Desktop's
#' configuration format:
#'
#' ```json
#' {
#'   "mcpServers": {
#'     "r-acquaint": {
#'       "command": "Rscript",
#'       "args": ["-e", "btw::btw_mcp_server()"]
#'     }
#'   }
#' }
#' ```
#'
#' Or, with Claude Code:
#'
#' ```bash
#' claude mcp add -s "user" r-acquaint -- Rscript -e "btw::btw_mcp_server()"
#' ```
#'
#' @param tools A list of [ellmer::tool()]s to use in the MCP server, defaults
#'   to the tools provided by [btw_tools()]. Use [btw_tools()] to subset to
#'   specific list of \pkg{btw} tools that can be augmented with additional
#'   tools. Alternatively, you can pass a path to an R script that returns a
#'   list of tools as supported by [acquaint::mcp_server()].
#'
#' @examples
#' # Should only be run non-interactively, and
#' # will block the current R process once called.
#' if (FALSE) {
#'   # To start a server with btw tools:
#'   btw_mcp_server()
#'
#'   # To only do so with a subset of btw's tools, e.g. those
#'   # that fetch package documentation:
#'   btw_mcp_server(tools = btw_tools("docs"))
#' }
#'
#' # To allow the server to access variables in specific
#' # sessions, call `btw_mcp_session()` in that session:
#' btw_mcp_session()
#'
#' @name mcp
#' @export
btw_mcp_server <- function(tools = btw_tools()) {
  # If given a path to an R script, we'll pass it on to mcp_server()
  is_likely_r_file <-
    is.character(tools) &&
    file.exists(tools) &&
    grepl("[.]r$", tools, ignore.case = TRUE)

  if (!is_likely_r_file) {
    tools <- flatten_and_check_tools(tools)
  }

  acquaint::mcp_server(tools = tools)
}

#' @rdname mcp
#' @export
btw_mcp_session <- function() {
  acquaint::mcp_session()
}
