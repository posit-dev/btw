# NA

## Overview

btw is an R package that helps humans and LLMs work together with R by
providing utilities to describe R objects, package documentation, and
workspace state in LLM-friendly plain text. The package offers a
flexible collection of tools that can be used interactively (copy-paste
workflows), programmatically (direct function calls), or as enhanced
chat clients (via ellmer or MCP servers).

The primary goal is creating a collection of tools useful to both LLMs
and humans when working together with R, with an emphasis on flexibility
of usage across different workflows and platforms.

## Quick Reference

- **Project type:** R Package
- **Language:** R (≥ 4.1.0)
- **Key frameworks:** ellmer (LLM chat integration), mcptools (Model
  Context Protocol), shiny and shinychat (chat app)

## Purpose and Design Philosophy

btw prioritizes flexibility of usage through multiple entry points:

- **[`btw()`](https://posit-dev.github.io/btw/dev/reference/btw.md)** -
  Interactive copy-paste workflow: gather context from R and paste into
  any chat interface
- **[`btw_tools()`](https://posit-dev.github.io/btw/dev/reference/btw_tools.md)** -
  Register tools with ellmer chat clients for custom applications
- **[`btw_client()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)
  /
  [`btw_app()`](https://posit-dev.github.io/btw/dev/reference/btw_client.md)** -
  Batteries-included chat clients with your preferred LLM provider,
  model, and project context
- **MCP server** - Expose tools to third-party coding agents like Claude
  Desktop or Continue via
  [`btw_mcp_server()`](https://posit-dev.github.io/btw/dev/reference/mcp.md)

Project configuration via `btw.md` files provides conversation stability
across sessions by defining default provider, model, tools, and
project-specific instructions. These files are treated as instructions
for coding assistants and help avoid repeating context.

btw also serves as a laboratory for discovering best practices in LLM
tool design - output formats and approaches evolve based on
experimentation with what works best across different models.
