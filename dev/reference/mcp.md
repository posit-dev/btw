# Start a Model Context Protocol server with btw tools

`btw_mcp_server()` starts an MCP server with tools from
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md),
which can provide MCP clients like Claude Desktop or Claude Code with
additional context. The function will block the R process it's called in
and isn't intended for interactive use.

To give the MCP server access to a specific R session, run
`btw_mcp_session()` in that session. If there are no sessions
configured, the server will run the tools in its own session, meaning
that e.g. the `btw_tools(tools = "env")` tools will describe R objects
in *that* R environment.

## Usage

``` r
btw_mcp_server(tools = NULL)

btw_mcp_session()
```

## Arguments

- tools:

  A list of
  [`ellmer::tool()`](https://ellmer.tidyverse.org/reference/tool.html)s
  to use in the MCP server, defaults to the tools provided by
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).
  Use
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
  to subset to specific list of btw tools that can be augmented with
  additional tools. Alternatively, you can pass a path to an R script
  that returns a list of tools as supported by
  [`mcptools::mcp_server()`](https://posit-dev.github.io/mcptools/reference/server.html).
  In that case the script provides the complete set of tools – btw's
  tools are not included unless the script itself adds them with
  [`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md).

## Value

Returns the result of
[`mcptools::mcp_server()`](https://posit-dev.github.io/mcptools/reference/server.html)
or
[`mcptools::mcp_session()`](https://posit-dev.github.io/mcptools/reference/server.html).

## Choosing Tool Groups

When using btw with a coding agent that already has built-in tools for
file operations, code execution, or other tasks, you may want to select
a subset of btw's tools to avoid overlap. A recommended lightweight
configuration for R package development is:

    btw_mcp_server(btw_tools("docs", "pkg"))

This gives the agent access to R documentation (help pages, vignettes,
news) and package development tools (testing, checking, documenting)
without duplicating file or code execution capabilities the agent may
already have.

Depending on your workflow, you may also want to include:

- `"env"`: Tools to inspect R objects and data frames in the session

- `"sessioninfo"`: Tools to check installed packages and platform
  details

- `"cran"`: Tools to search for and describe CRAN packages

    btw_mcp_server(list("docs", "pkg", "env", "sessioninfo", "cran"))

See
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
for a complete list of available tool groups and their contents.

To combine btw's tools with your own custom tools, we recommend writing
a small R script that returns the combined list of tools and passing its
path to `btw_mcp_server()`.

MCP servers are usually launched by your coding harness, e.g. via
`Rscript -e "btw::btw_mcp_server(...)"` in a client configuration file.
Composing the tool list in an R script is much easier to read and
maintain than cramming the same code into an inline `Rscript` command.
The script below combines btw's tools with your own custom tools:

    # tools.R
    my_custom_tool <- ellmer::tool(
      function() R.version.string,
      "Report the version of R running the MCP server"
    )

    c(btw_tools("docs", "pkg"), list(my_custom_tool))

The script is responsible for the complete list of tools, so include
[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)
calls in the script to pull in btw's tools.

Use an absolute path to the script in most cases, e.g.
`btw_mcp_server("/path/to/tools.R")`. Relative paths are resolved
against the working directory of the process launched by the coding
harness, which is usually the project directory but isn't guaranteed. A
project-specific tools script in the project directory is a reasonable
use of a relative path, but absolute paths are the safest choice
otherwise.

## Configuration

To configure this server with MCP clients, use the command `Rscript` and
the args `-e "btw::btw_mcp_server()"`. We recommend customizing the tool
set as described above to avoid overlap with your client's built-in
capabilities and to choose the tools that make the most sense for your
workflow.

For [Claude Desktop's configuration
format](https://code.claude.com/docs/en/mcp#add-mcp-servers-from-json-configuration):

    {
      "mcpServers": {
        "r-btw": {
          "command": "Rscript",
          "args": ["-e", "btw::btw_mcp_server(list('docs', 'pkg', 'env', 'sessioninfo', 'cran'))"]
        }
      }
    }

For [Claude Code](https://code.claude.com/docs/en/overview):

    claude mcp add -s "user" r-btw -- Rscript -e "btw::btw_mcp_server(list('docs', 'pkg', 'env', 'sessioninfo', 'cran'))"

For [Positron](https://positron.posit.co) or [VS
Code](https://code.visualstudio.com/docs/copilot/chat/mcp-servers), add
the following to `.vscode/mcp.json` (for workspace configuration) or to
your user profile's `mcp.json` (for global configuration, accessible via
Command Palette \> **MCP: Open User Configuration**):

    {
      "servers": {
        "r-btw": {
          "type": "stdio",
          "command": "Rscript",
          "args": ["-e", "btw::btw_mcp_server(list('docs', 'pkg', 'env', 'sessioninfo', 'cran'))"]
        }
      }
    }

Alternatively, run the **MCP: Add Server** command from the Command
Palette, choose **stdio**, then choose **Workspace** or **Global** to
add the server configuration interactively.

For [Continue](https://continue.dev/), include the following in your
[config
file](https://docs.continue.dev/customize/deep-dives/configuration):

    "experimental": {
      "modelContextProtocolServers": [
        {
          "transport": {
            "name": "r-btw",
            "type": "stdio",
            "command": "Rscript",
            "args": [
              "-e",
              "btw::btw_mcp_server(list('docs', 'pkg', 'env', 'sessioninfo', 'cran'))"
            ]
          }
        }
      ]
    }

## Additional Examples

`btw_mcp_server()` should only be run non-interactively, as it will
block the current R process once called.

To start a server with btw tools:

    btw_mcp_server()

Or to only do so with a subset of btw's tools, e.g. those that fetch
package documentation:

    btw_mcp_server(btw_tools("docs"))

    # alternatively a bare list
    btw_mcp_server(list("docs"))

    # or a list of btw tools and custom tools
    btw_mcp_server(list("docs", my_custom_tool))

To allow the server to access variables in specific sessions, call
`btw_mcp_session()` in that session:

    btw_mcp_session()

## See also

These functions use
[`mcptools::mcp_server()`](https://posit-dev.github.io/mcptools/reference/server.html)
and
[`mcptools::mcp_session()`](https://posit-dev.github.io/mcptools/reference/server.html)
under the hood. To configure arbitrary tools with an MCP client, see the
documentation of those functions.

## Examples

``` r
# btw_mcp_server() and btw_mcp_session() are only intended to be run in
# non-interactive R sessions, e.g. when started by an MCP client like
# Claude Desktop or Claude Code. Therefore, we don't run these functions
# in examples.

# See above for more details and examples.
```
