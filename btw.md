---
client:
  sonnet:
    provider: aws_bedrock
    model: us.anthropic.claude-sonnet-4-6
    api_args:
      additionalModelRequestFields:
        thinking:
          type: enabled
          budget_tokens: 4000
  haiku:
    provider: aws_bedrock
    model: us.anthropic.claude-haiku-4-5-20251001-v1:0
    api_args:
      additionalModelRequestFields:
        thinking:
          type: enabled
          budget_tokens: 4000
  opus:
    provider: aws_bedrock
    model: us.anthropic.claude-opus-4-5-20251101-v1:0
    api_args:
      additionalModelRequestFields:
        thinking:
          type: enabled
          budget_tokens: 4000

  sonnet-4.5:
    provider: aws_bedrock
    model: us.anthropic.claude-sonnet-4-5-20250929-v1:0
  sonnet-4:
    provider: aws_bedrock
    model: us.anthropic.claude-sonnet-4-20250514-v1:0
  opus-4.5:
    provider: aws_bedrock
    model: us.anthropic.claude-opus-4-5-20251101-v1:0

  gemma4:
    provider: openai_compatible
    model: google/gemma-4-26b-a4b
    # base_url: http://127.0.0.1:1234/v1
    base_url: http://remy.local:1234/v1
    name: "LM Studio"

  qwen3.6:
    provider: openai_compatible
    model: qwen/qwen3.6-35b-a3b
    base_url: http://remy.local:1234/v1
    name: "LM Studio"
    preserve_thinking: true

  qwen-3-5-35b:
    provider: openai_compatible
    model: qwen/qwen3.5-35b-a3b
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"

  qwen-3-5-9b:
    provider: openai_compatible
    model: qwen/qwen3.5-9b
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"
    params:
      reasoning_effort: medium

  glm-4-7-flash:
    provider: openai_compatible
    model: zai-org/glm-4.7-flash
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"

  nemotron-3-nano-4b:
    provider: openai_compatible
    model: nvidia/nemotron-3-nano-4b
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"

  lm-glm4v:
    provider: openai_compatible
    model: zai-org/glm-4.6v-flash
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"
  qwen3:
    provider: openai_compatible
    model: qwen/qwen3-vl-30b
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"
  gpt-oss-20b:
    provider: openai_compatible
    model: openai/gpt-oss-20b
    base_url: http://127.0.0.1:1234/v1
    name: "LM Studio"

tools:
  # - agent
  - skills
  - files_list
  - files_read
  - files_write
  # - files_edit
  - files_replace
  - docs_help_page
  - docs_package_help_topics

options:
  skills:
    paths: ["~/.agents/skills"]
  subagent:
    # client: openai/gpt-5.4-mini
    tools_allowed: [docs, files_search, files_list]
---

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