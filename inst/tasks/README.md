# btw Task Files

This directory contains pre-defined task files for use with `btw_task()`.

## Usage

Tasks are defined in markdown files with YAML frontmatter for configuration and a markdown body containing the task instructions:

```r
# Run a task with template variables
chat <- btw_task(
  system.file("tasks/analyze-package.md", package = "btw"),
  package_name = "dplyr",
  mode = "console"
)

# Include additional context (unnamed arguments)
chat <- btw_task(
  system.file("tasks/data-summary.md", package = "btw"),
  dataset_name = "mtcars",  # Named - template variable
  mtcars,                   # Unnamed - additional context
  mode = "app"
)
```

## Available Tasks

### analyze-package.md
Analyzes an R package and provides a comprehensive summary including purpose, key functions, dependencies, use cases, and comparisons to similar packages.

**Template variables:**
- `package_name`: Name of the package to analyze

### code-review.md
Reviews code files and provides detailed feedback on code quality, best practices, performance, documentation, and testing considerations.

**Template variables:**
- `file_path`: Path to the file to review

### data-summary.md
Creates comprehensive summaries of datasets including basic statistics, data quality assessment, and insights.

**Template variables:**
- `dataset_name`: Name of the dataset to summarize

## Task File Format

```yaml
---
client:                    # Optional: LLM client configuration
  provider: anthropic
  model: claude-sonnet-4
tools: [docs, files]      # Optional: btw tools to include
---

# Your task prompt here

Use {{ variable_name }} syntax for template variables that can be
interpolated when running the task.
```

## Execution Modes

Tasks can be run in four different modes:

- **app**: Launch interactive Shiny app (default)
- **console**: Interactive console chat
- **client**: Return configured chat client
- **tool**: Return an ellmer tool object for programmatic use

## Creating Custom Tasks

1. Create a markdown file with YAML frontmatter and task instructions
2. Use `{{ variable }}` syntax for template placeholders
3. Run with `btw_task()` providing the path and template values

Example custom task:

```markdown
---
tools: [files, git]
---

Review the recent changes in {{ file_path }} and suggest improvements.
Focus on {{ focus_area }} aspects of the code.
```

```r
chat <- btw_task(
  "my-review-task.md",
  file_path = "R/my-function.R",
  focus_area = "performance",
  mode = "console"
)
```