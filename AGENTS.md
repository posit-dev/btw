# btw

> Bridge R computational environments and Large Language Models

## Overview

btw is an R package that helps humans and LLMs work together with R by providing utilities to describe R objects, package documentation, and workspace state in LLM-friendly plain text. The package offers a flexible collection of tools that can be used interactively (copy-paste workflows), programmatically (direct function calls), or as enhanced chat clients (via ellmer or MCP servers).

The primary goal is creating a collection of tools useful to both LLMs and humans when working together with R, with an emphasis on flexibility of usage across different workflows and platforms.

## Quick Reference

- **Project type:** R Package
- **Language:** R (≥ 4.1.0)
- **Key frameworks:** ellmer (LLM chat integration), mcptools (Model Context Protocol), shiny and shinychat (chat app)

## Purpose and Design Philosophy

btw prioritizes flexibility of usage through multiple entry points:

- **`btw()`** - Interactive copy-paste workflow: gather context from R and paste into any chat interface
- **`btw_tools()`** - Register tools with ellmer chat clients for custom applications
- **`btw_client()` / `btw_app()`** - Batteries-included chat clients with your preferred LLM provider, model, and project context
- **MCP server** - Expose tools to third-party coding agents like Claude Desktop or Continue via `btw_mcp_server()`

Project configuration via `btw.md` files provides conversation stability across sessions by defining default provider, model, tools, and project-specific instructions. These files are treated as instructions for coding assistants and help avoid repeating context.

btw also serves as a laboratory for discovering best practices in LLM tool design - output formats and approaches evolve based on experimentation with what works best across different models.

## Key Design Decisions

- **Multiple entry points for flexibility** - Support interactive, programmatic, and agent-based workflows without forcing users into a single pattern
- **S3 dispatch via `btw_this()`** - Extensible backend for describing different R object types, with `btw_this.character()` handling command aliases for non-object concepts
- **Tool grouping for safety and clarity** - Tools organized into categories (docs, env, files, ide, search, session, web) to reduce LLM cognitive load and provide granular safety controls
- **Project configuration via `btw.md`** - Enables stable, repeatable conversations across sessions with project-specific context and preferred settings
- **S7 interoperability** - Uses S7 classes where required by ellmer's API while keeping most package code in familiar S3
- **Experimental format optimization** - Continuous testing of output formats (JSON for data, markdown for display, plain text for docs) to find what LLMs handle best

## How It Works

### Core Architecture

The package has three main layers:

1. **User-facing API** - `btw()` function accepts any R objects, help topics, file paths, or special commands (e.g., `"@platform_info"`, `"{dplyr}"`) and assembles them into LLM-ready context
2. **Description System** - `btw_this()` S3 generic dispatches to type-specific methods that know how to describe data frames, functions, environments, packages, etc.
3. **Tool System** - ~20 individual tools that can be registered with ellmer chats or MCP servers to give LLMs active capabilities beyond passive context
4. **Agents** - `btw_task_*()` functions provide guided workflows for common tasks and are primarily intended for interactive use, but can also be used as sub-agents via tool calls.

### Entry Points

**Interactive Copy-Paste (`btw()`)**
```r
btw(mtcars, "{dplyr}", ?dplyr::across)
# ✔ btw copied to the clipboard!
```
Assembles context and copies to clipboard for pasting into any chat interface.

**Programmatic Tool Registration (`btw_tools()`)**
```r
chat <- ellmer::chat_anthropic()
chat$set_tools(btw_tools("docs", "env"))  # Only doc and environment tools
```
Register specific tool groups with ellmer chat clients.

**Batteries-Included Chat (`btw_client()` / `btw_app()`)**
```r
chat <- btw_client(mtcars)  # Starts with mtcars in context
chat$chat("Summarize this data")

btw_app()  # Launches Shiny chat interface
```
Pre-configured chat clients with btw tools, project context from `btw.md`, and initial objects.

**MCP Server (`btw_mcp_server()`)**
```r
btw_mcp_server()  # Blocks; run non-interactively
```
Exposes btw tools to external MCP clients. Configure in Claude Desktop or other MCP-compatible tools.

### Tool System Design

Tools are defined in `R/tool-*.R` files following a consistent pattern:

1. **Implementation function** (`btw_tool_*_impl()`) - Does the actual work, returns a tool result object
2. **Exported wrapper** (`btw_tool_*()`) - Thin wrapper with roxygen docs for users
3. **Tool registration** - Called via `.btw_add_to_tools()` to register with ellmer

Tools are grouped by capability:
- **docs** - Package documentation, help pages, vignettes, NEWS
- **env** - Describe data frames and environments
- **files** - Read, write, list files; search code
- **ide** - Read current editor/selection in RStudio/Positron
- **search** - Search CRAN packages
- **session** - Platform info, installed packages
- **web** - Read web pages as markdown

### The btw_this() Dispatch System

`btw_this()` is the extensible backend that powers `btw()`:

- `btw_this.data.frame()` - Uses skimr to create JSON summaries
- `btw_this.function()` - Captures function source code
- `btw_this.environment()` - Lists and describes objects
- `btw_this.character()` - **Command dispatcher** for special strings:
  - `"./path"` → read file or list directory
  - `"{pkgName}"` or `"@pkg pkgName"` → package help topics + intro vignette
  - `"?help_topic"` or `"@help topic"` → help page lookup (supports `@help pkg::topic` and `@help pkg topic` formats)
  - `"@git status|diff|log"` → git repository information
  - `"@issue #number"` or `"@pr #number"` → GitHub issues/PRs with auto-repo detection
  - `"@platform_info"` → session/platform details
  - `"@current_file"` → read active editor
  - And more (see `?btw_this.character`)

### Project Configuration

`btw.md` or `AGENTS.md` files provide project-specific context:

```markdown
---
client:
  provider: claude
  model: claude-3-7-sonnet-20250219
tools: [data, docs, environment]
---

Follow these style rules: ...
```

btw looks for these files in:
1. Current working directory or its parents (project-specific)
2. `~/.config/btw/btw.md` or `~/btw.md` (global defaults)

Content becomes part of the system prompt. Use `<!-- HIDE -->` / `<!-- /HIDE -->` comments to exclude sections from the prompt.

### Task System (Emerging Pattern)

The `btw_task_*()` pattern provides guided workflows:

- `btw_task_create_btw_md()` - Interactive assistant for creating a `btw.md` file in a new project
- `btw_task_create_readme()` - Interactive assistant to help create a README for end users
- Future tasks will follow similar patterns for common workflows

## Technical Details

### Core Dependencies

- **ellmer** (≥ 0.3.0) - LLM chat client framework; provides base `Chat` class and tool system
- **mcptools** - Model Context Protocol implementation for exposing tools to external agents
- **S7** - Object-oriented system used by ellmer; btw uses it for interoperability
- **skimr** - Data frame summaries with statistical distributions
- **pkgsearch** - Search and retrieve CRAN package information
- **sessioninfo** - Detailed R session information
- **rstudioapi** - IDE integration (works with RStudio and Positron)

### Supporting Dependencies

- **cli** - User-facing messages and formatting
- **clipr** - Clipboard operations
- **dplyr** - Data manipulation
- **fs** - File system operations
- **jsonlite** - JSON serialization
- **rmarkdown** - YAML frontmatter parsing and vignette rendering
- **xml2** - HTML/XML parsing for web content

### Suggested Tools

- **shinychat** (≥ 0.2.0) - Powers `btw_app()` chat interface
- **chromote** - Headless browser for reading dynamic web pages
- **pandoc** - Document conversion (vignettes, help pages)

## Directory Structure

```
btw/
├── R/                          # Package source code
│   ├── btw.R                   # Main user-facing btw() function
│   ├── btw_this.R              # S3 generic and default method
│   ├── btw_client.R            # Enhanced chat client creation
│   ├── btw_client_app.R        # Shiny app interface
│   ├── tools.R                 # Tool registration (btw_tools())
│   ├── aaa-tools.R             # Tool registry infrastructure
│   ├── tool-*.R                # Individual tool implementations
│   ├── task_*.R                # Guided workflow tasks (new pattern)
│   ├── mcp.R                   # MCP server integration
│   ├── utils-*.R               # Utility functions
│   └── import-standalone-*.R   # Vendored rlang utilities
│
├── inst/
│   ├── prompts/                # System prompts for LLMs
│   │   ├── btw-init.md         # Project documentation task prompt
│   │   ├── btw-system_session.md
│   │   ├── btw-system_tools.md
│   │   └── btw-system_project.md
│   ├── icons/                  # SVG icons for tool categories
│   └── js/                     # JavaScript utilities
│
├── man/                        # Generated documentation
├── tests/testthat/             # Test suite
│   ├── _snaps/                 # Snapshot tests for tool outputs
│   ├── fixtures/               # Test data
│   ├── helpers-*.R             # Test utilities and mocks
│   └── test-*.R                # Test files
│
├── docs/                       # pkgdown website
├── pkgdown/                    # pkgdown configuration
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions
└── README.Rmd                  # Source for README.md
```

## Development Workflow

### Getting Started

```r
# Install development version
pak::pak("posit-dev/btw")

# Load package
library(btw)

# Try interactive usage
btw(mtcars)

# Or start a chat
chat <- btw_client()
chat$chat("What tools do you have access to?")
```

### Running Tests

```r
# Run all tests
devtools::test()

# Run specific test file
devtools::test_active_file()

# Update snapshots when tool output changes (verify changes first!)
testthat::snapshot_accept()
```

### Common Tasks

**Add a new tool:**

1. Create implementation in `R/tool-<category>.R`:
```r
btw_tool_example_impl <- function(arg) {
  # Implementation
  tool_result(output, extra = list(display = list(...)))
}
```

2. Add thin exported wrapper:
```r
#' @export
btw_tool_example <- function(arg, `_intent`) {}
```

3. Register with tool system:
```r
.btw_add_to_tools(
  name = "btw_tool_example",
  group = "category",
  tool = function() {
    ellmer::tool(
      btw_tool_example_impl,
      name = "btw_tool_example",
      description = "...",
      arguments = list(arg = ellmer::type_string("..."))
    )
  }
)
```

**Add a btw_this() method:**

```r
#' @export
btw_this.new_class <- function(x, ...) {
  # Describe the object as character vector
  as_btw_capture(output_lines)
}
```

**Update system prompts:**

Edit files in `inst/prompts/`. These are assembled in `btw_client()` based on configuration.

## Code Conventions & Standards

- **Use tidyverse patterns** for data manipulation and consistency
- **S3 for package objects**, S7 only where required by ellmer API
- **Tool functions return `tool_result()` objects** with value and optional display metadata
- **All tool wrappers accept `_intent` parameter** (added automatically by `wrap_with_intent()`)
- **Snapshot tests for tool outputs** to catch formatting changes
- **Use `check_*()` functions** argument validation, described below

### Type Check Functions

#### Scalars
- `check_bool()` - Validates single TRUE/FALSE value
- `check_string()` - Validates single string (allows empty by default)
- `check_name()` - Validates non-empty string for use as name
- `check_number_decimal()` - Validates single numeric value (allows decimals)
- `check_number_whole()` - Validates single integer value
- `check_symbol()` - Validates R symbol/name object
- `check_arg()` - Validates argument name (symbol)
- `check_call()` - Validates defused call object
- `check_environment()` - Validates environment object
- `check_function()` - Validates function (any type)
- `check_closure()` - Validates R function (not primitive/builtin)
- `check_formula()` - Validates formula object

#### Vectors
- `check_character()` - Validates character vector
- `check_logical()` - Validates logical vector
- `check_data_frame()` - Validates data frame

#### Common Parameters
- All functions support `allow_null` parameter
- Most scalars support `allow_na`
- Number checkers support `min`/`max` bounds and `allow_infinite`
- All use `arg`/`call` for error context

## Important Notes

- **Experimental package** - APIs may change as best practices for LLM tools evolve
- **Tool output formats are fluid** - btw intentionally experiments with different formats (JSON, markdown, plain text) to discover what works best with different models
- **clipboard operations may fail** in non-interactive contexts or without clipboard support
- **IDE tools** (e.g., `@current_file`) require RStudio or Positron with rstudioapi support
- **Git and GitHub commands** - `@git` and `@issue`/`@pr` commands require gert and gh packages respectively, plus appropriate repository access
- **MCP server blocks** the R process - run non-interactively
- **Test snapshots require frequent updates** as output formatting evolves

## Resources

- **GitHub:** https://github.com/posit-dev/btw
- **Website:** https://posit-dev.github.io/btw/
- **ellmer package:** https://github.com/tidyverse/ellmer
- **mcptools package:** https://github.com/posit-dev/mcptools/
- **AGENTS.md spec:** https://agents.md/

