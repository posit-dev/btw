# btw

> A complete toolkit for connecting R and LLMs

## Overview

btw helps R users work with Large Language Models, whether you’re
pasting context into ChatGPT, chatting with an AI assistant in your IDE,
or building LLM-powered applications.

The challenge: LLMs need context about your R environment to be
helpful—your data structures, the packages you’re using, relevant
documentation.

btw provides a flexible toolkit that works across different workflows:

- **Copy-paste to external LLMs:** Quickly gather context from your R
  session and copy it to your clipboard for pasting into ChatGPT,
  Claude, or any other chat interface.
- **Interactive chat in R:** Launch a full-featured AI assistant
  directly in your IDE that can explore your environment, read
  documentation, and help you write code.
- **Build LLM-powered tools:** Integrate btw’s capabilities into your
  own applications, whether you’re creating custom chat interfaces or
  connecting R to coding agents.

## Quick Start

### Copy-paste workflow

Use [`btw()`](https://posit-dev.github.io/btw/reference/btw.md) to
gather context from your R session and copy it to your clipboard:

[`library`](https://rdrr.io/r/base/library.html)`(`[`btw`](https://github.com/posit-dev/btw)`)`` `` ``# Describe a data frame`` `[`btw`](https://posit-dev.github.io/btw/reference/btw.md)`(``mtcars``)`` `` ``# Include package or function documentation`` `[`btw`](https://posit-dev.github.io/btw/reference/btw.md)`(``"{dplyr}"``, ``?``dplyr``::`[`across`](https://dplyr.tidyverse.org/reference/across.html)`)`` `` ``# Combine multiple pieces of context`` `[`btw`](https://posit-dev.github.io/btw/reference/btw.md)`(``mtcars``, ``"{dplyr}"``, ``"How do I calculate the mean mpg by cylinder?"``)`

The context is copied to your clipboard, ready to paste into ChatGPT,
Claude, or any LLM chat interface.

### Interactive chat in your IDE

Launch a chat interface with
[`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md):

[`btw_app`](https://posit-dev.github.io/btw/reference/btw_client.md)`(``)`

![Screenshot of btw_app() in action. In the sidebar, there is a list of
tools that can be toggled on and off, and in the main panel a chat
interface. In the chat we can see several tool calls have been made to
read files in the current project.](reference/figures/btw-app.png)

For persistent project context, create a `btw.md` file with
[`use_btw_md()`](https://posit-dev.github.io/btw/reference/use_btw_md.md).
This creates a project-specific configuration file where you can define
your preferred LLM provider, model, and custom instructions that apply
to all conversations in your project.

### Building with btw

btw supercharges [ellmer](https://ellmer.tidyverse.org/)! Use
[`btw_client()`](https://posit-dev.github.io/btw/reference/btw_client.md)
for a pre-configured chat client, the same client used by
[`btw_app()`](https://posit-dev.github.io/btw/reference/btw_client.md).

`# Uses provider, model, tools and instructions from btw.md`` ``chat`` ``<-`` `[`btw_client`](https://posit-dev.github.io/btw/reference/btw_client.md)`(``)`` ``chat``$``chat``(``"Help me write documentation for..."``)`

Or use
[`btw_tools()`](https://posit-dev.github.io/btw/reference/btw_tools.md)
to get a list of tools you can register with any ellmer chat client.

[`library`](https://rdrr.io/r/base/library.html)`(`[`ellmer`](https://ellmer.tidyverse.org)`)`` `` ``chat`` ``<-`` `[`chat_anthropic`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)`(``)`` ``# or chat_openai(), chat_ollama(), etc.`` ``chat``$``register_tools``(`[`btw_tools`](https://posit-dev.github.io/btw/reference/btw_tools.md)`(``)``)`` `` ``chat``$``chat``(``"What data frames are in my environment?"``)`

Pick and choose which tools you use with friendly group names

`# Only provide documentation and file tools`` ``chat``$``register_tools``(`[`btw_tools`](https://posit-dev.github.io/btw/reference/btw_tools.md)`(`[`c`](https://rdrr.io/r/base/c.html)`(``"docs"``, ``"files"``)``)``)`

or expose btw tools to external coding agents via the [Model Context
Protocol](https://modelcontextprotocol.io/) using
[mcptools](https://posit-dev.github.io/mcptools/).

`# Run as a background process or in a separate R session`` `[`btw_mcp_server`](https://posit-dev.github.io/btw/reference/mcp.md)`(``)`

You can [configure the MCP
server](https://posit-dev.github.io/btw/reference/mcp.html) in Claude
Desktop, Continue, or other MCP-compatible tools to give them access to
your R environment.

## Installation

You can install btw from CRAN:

[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"btw"``)`

To install the latest development version, you can install from
[posit-dev.r-universe.dev](https://posit-dev.r-universe.dev/):

`# install.packages("pak")`` `` ``pak``::`[`repo_add`](https://pak.r-lib.org/reference/repo_add.html)`(``"https://posit-dev.r-universe.dev"``)`` ``pak``::`[`pak`](https://pak.r-lib.org/reference/pak.html)`(``"btw"``)`

Or you can install the development version from
[GitHub](https://github.com/posit-dev/btw):

`# install.packages("pak")`` ``pak``::`[`pak`](https://pak.r-lib.org/reference/pak.html)`(``"posit-dev/btw"``)`

## Learn More

- 🌐 [Package website](https://posit-dev.github.io/btw/)
- 📚 [Function reference](https://posit-dev.github.io/btw/reference/)
- 💻 [GitHub repository](https://github.com/posit-dev/btw)

For questions or issues, please [open an issue on
GitHub](https://github.com/posit-dev/btw/issues).
