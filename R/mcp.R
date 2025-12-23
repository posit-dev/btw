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
#' These functions use [mcptools::mcp_server()] and [mcptools::mcp_session()]
#' under the hood. To configure arbitrary tools with an MCP client, see the
#' documentation of those functions.
#'
#' @section Configuration:
#' To configure this server with MCP clients, use the command `Rscript` and the
#' args `-e "btw::btw_mcp_server()"`. For example, in [Claude Desktop's
#' configuration
#' format](https://code.claude.com/docs/en/mcp#add-mcp-servers-from-json-configuration):
#'
#' ```json
#' {
#'   "mcpServers": {
#'     "r-btw": {
#'       "command": "Rscript",
#'       "args": ["-e", "btw::btw_mcp_server()"]
#'     }
#'   }
#' }
#' ```
#'
#' For [Claude Code](https://code.claude.com/docs/en/overview):
#'
#' ```bash
#' claude mcp add -s "user" r-btw -- Rscript -e "btw::btw_mcp_server()"
#' ```
#'
#' For [Continue](https://www.continue.dev/), include the following in your
#' [config file](https://docs.continue.dev/customize/deep-dives/configuration):
#'
#' ```json
#' "experimental": {
#'   "modelContextProtocolServers": [
#'     {
#'       "transport": {
#'         "name": "r-btw",
#'         "type": "stdio",
#'         "command": "Rscript",
#'         "args": [
#'           "-e",
#'           "btw::btw_mcp_server()"
#'         ]
#'       }
#'     }
#'   ]
#' }
#' ```
#'
#' @section Additional Examples:
#'
#' `btw_mcp_server()` should only be run non-interactively, as it will block the
#' current R process once called.
#'
#' To start a server with btw tools:
#'
#' ```r
#' btw_mcp_server()
#' ```
#'
#' Or to only do so with a subset of btw's tools, e.g. those that fetch package
#' documentation:
#'
#' ```r
#' btw_mcp_server(tools = btw_tools("docs"))
#' ```
#'
#' To allow the server to access variables in specific sessions, call
#' `btw_mcp_session()` in that session:
#'
#' ```r
#' btw_mcp_session()
#' ```
#'
#' @examples
#' # btw_mcp_server() and btw_mcp_session() are only intended to be run in
#' # non-interactive R sessions, e.g. when started by an MCP client like
#' # Claude Desktop or Claude Code. Therefore, we don't run these functions
#' # in examples.
#'
#' # See above for more details and examples.
#'
#' @param tools A list of [ellmer::tool()]s to use in the MCP server, defaults
#'   to the tools provided by [btw_tools()]. Use [btw_tools()] to subset to
#'   specific list of \pkg{btw} tools that can be augmented with additional
#'   tools. Alternatively, you can pass a path to an R script that returns a
#'   list of tools as supported by [mcptools::mcp_server()].
#'
#' @return Returns the result of [mcptools::mcp_server()] or
#'   [mcptools::mcp_session()].
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

  mcptools::mcp_server(tools = tools)
}

#' @rdname mcp
#' @export
btw_mcp_session <- function() {
  mcptools::mcp_session()
}
