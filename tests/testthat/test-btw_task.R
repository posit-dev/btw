local_enable_tools()
withr::local_options(btw.client.quiet = TRUE)

describe("btw_task()", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "test-key"))

  it("reads and parses task files correctly", {
    # Create a temporary task file
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "client:",
        "  provider: anthropic",
        "  model: claude-sonnet-4",
        "tools: [docs, files]",
        "---",
        "",
        "Analyze the {{ package_name }} package.",
        "Focus on {{ focus_area }}."
      )
    )

    # Test client mode
    chat <- btw_task(
      task_file,
      package_name = "dplyr",
      focus_area = "data manipulation",
      mode = "client"
    )

    expect_s3_class(chat, "Chat")

    # Check that interpolation worked
    sys_prompt <- chat$get_system_prompt()
    expect_match(sys_prompt, "Analyze the dplyr package", fixed = TRUE)
    expect_match(sys_prompt, "Focus on data manipulation", fixed = TRUE)

    # Should not contain template markers
    expect_no_match(sys_prompt, "{{", fixed = TRUE)
    expect_no_match(sys_prompt, "}}", fixed = TRUE)
  })

  it("handles mixed named and unnamed arguments", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "tools: [env]",
        "---",
        "",
        "Analyze {{ dataset_name }}."
      )
    )

    chat <- btw_task(
      task_file,
      dataset_name = "mtcars", # Named - template var
      mode = "client"
    )

    sys_prompt <- chat$get_system_prompt()
    expect_match(sys_prompt, "Analyze mtcars", fixed = TRUE)
  })

  it("appends unnamed arguments as additional context", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "tools: false",
        "---",
        "",
        "Analyze the data."
      )
    )

    chat <- btw_task(
      task_file,
      mtcars, # Unnamed - additional context
      mode = "client"
    )

    sys_prompt <- chat$get_system_prompt()
    expect_match(sys_prompt, "# Additional Context", fixed = TRUE)
  })

  it("creates a working tool", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "tools: false",
        "---",
        "",
        "Simple task: {{ action }}"
      )
    )

    tool <- btw_task(
      task_file,
      action = "test",
      mode = "tool"
    )

    expect_s3_class(tool, "ellmer::ToolDef")

    # Check tool properties
    expect_match(tool@name, "btw_task_")
    expect_type(tool@description, "character")

    # Tool arguments are stored as an ArgumentSchema object
    # Just check that the tool has arguments defined
    expect_false(is.null(tool@arguments))
  })

  it("uses valid task file names for tool identifiers", {
    task_dir <- withr::local_tempdir()
    task_file <- file.path(task_dir, "my-task-file.md")
    writeLines(
      con = task_file,
      c(
        "---",
        "tools: false",
        "---",
        "",
        "Simple task"
      )
    )

    tool <- btw_task(task_file, mode = "tool")
    expect_identical(tool@name, "btw_task_my-task-file")
  })

  it("errors on invalid task names in frontmatter", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "name: task.with.dot",
        "tools: false",
        "---",
        "",
        "Simple task"
      )
    )

    expect_error(
      btw_task(task_file, mode = "tool"),
      "Invalid task name"
    )
  })

  it("warns on invalid icon and falls back to NULL", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "tools: false",
        "icon: ''",
        "---",
        "",
        "Simple task"
      )
    )

    expect_warning(
      tool <- btw_task(task_file, mode = "tool"),
      "Invalid icon in task file"
    )
    expect_s3_class(tool, "ellmer::ToolDef")
    expect_null(tool@annotations$icon)
  })

  it("errors on missing file", {
    expect_error(
      btw_task("nonexistent.md", mode = "client"),
      "Task file not found"
    )
  })

  it("errors on empty task prompt", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "tools: [docs]",
        "---",
        "" # Empty body
      )
    )

    expect_error(
      btw_task(task_file, mode = "client"),
      "must contain a prompt"
    )
  })

  it("handles task files without frontmatter", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "Simple task without config.",
        "Analyze {{ target }}."
      )
    )

    # Should work with defaults
    chat <- btw_task(
      task_file,
      target = "the data",
      mode = "client"
    )

    expect_s3_class(chat, "Chat")
    sys_prompt <- chat$get_system_prompt()
    expect_match(sys_prompt, "Analyze the data", fixed = TRUE)
  })

  it("respects client override", {
    task_file <- withr::local_tempfile(fileext = ".md")
    writeLines(
      con = task_file,
      c(
        "---",
        "client:",
        "  provider: openai",
        "  model: gpt-4",
        "---",
        "",
        "Task content"
      )
    )

    # Override with different client
    custom_client <- ellmer::chat_anthropic(
      system_prompt = "Custom prompt"
    )

    chat <- btw_task(
      task_file,
      client = custom_client,
      mode = "client"
    )

    # Should use the provided client
    expect_identical(chat$get_provider()@name, "Anthropic")
  })
})

describe("btw_task() task files", {
  withr::local_envvar(list(ANTHROPIC_API_KEY = "test-key"))

  it("can load a task file with template variables", {
    task_file <- testthat::test_path("fixtures/test-analyze-package.md")
    expect_no_error(
      btw_task(task_file, package_name = "base", mode = "client")
    )
  })
})
