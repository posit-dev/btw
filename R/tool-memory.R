#' Tool: Project Context Memory - Add context
#'
#' Appends content to a specific key in the project context section of the
#' btw-memory.yaml file. If the key doesn't exist, it creates it. If it exists
#' and contains an array, the content is appended to the array.
#'
#' @param key Character string specifying which project context key to update.
#'   Must be one of: "problem_description", "objectives", "constraints", or
#'   "business_context".
#' @param content Character vector of content to add. For "problem_description",
#'   only the first element is used and replaces existing content. For other keys,
#'   all elements are appended to the existing array.
#' @param ... Ignored, used for future feature expansion and compatibility.
#' @param path Character string specifying the path to the memory file. If
#'   `NULL`, uses the default (`btw-memory.yaml` in the project root).
#'
#' @return Invisibly returns the updated memory data
#'
#' @family Memory tools
#' @export
btw_tool_memory_project_context_add <- function(
  key,
  content,
  ...,
  path = NULL
) {
  key <- arg_match(key, btw_memory_keys_project_context(), )
  check_character(content)
  path <- path_find_btw_memory(path, must_exist = FALSE)

  if (fs::file_exists(path)) {
    mem <- read_btw_memory_yaml(path)
  } else {
    mem <- as_btw_memory(list())
  }

  if (is.null(mem$project_context)) {
    mem$project_context <- list()
  }

  if (key == "problem_description") {
    mem$project_context[[key]] <- paste(
      c(mem$project_context[[key]], content),
      collapse = "\n\n"
    )
  } else {
    mem$project_context[[key]] <- c(mem$project_context[[key]], content)
  }

  write_btw_memory_yaml(mem, path)
  invisible(mem)
}

btw_memory_keys_project_context <- function() {
  c("problem_description", "objectives", "constraints", "business_context")
}

.btw_add_to_tools(
  "btw_tool_memory_project_context_add",
  group = "memory",
  tool = function() {
    ellmer::tool(
      function(key, content, ...) {
        btw_tool_memory_project_context_add(key, content)
        BtwToolResult("Success.")
      },
      .name = "btw_tool_memory_project_context_add",
      .annotations = ellmer::tool_annotations(
        title = "Add project context to memory",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        destructive_hint = FALSE
      ),
      .description = "Store or append information about the project.

Use this when the user provides new information that won't change during the project lifecycle about:

- **problem_description**: The main business problem or research question (replaces existing)
- **objectives**: Specific analysis goals or questions to answer (appends to list)
- **constraints**: Limitations, requirements, or restrictions (appends to list)
- **business_context**: Domain knowledge, organizational background (appends to list)

**When to use:** User mentions project goals, business requirements, analysis objectives, domain constraints, or background context that should be remembered across sessions.

**Examples:**
- User says \"I need to predict customer churn\" -> store in `problem_description` as \"Identify customers at risk of churning\"
- User explains \"We need three risk levels\" -> add to `objectives` as \"Segment customers into low, medium, high risk\"
- User states \"We can't use personal data\" -> add to `constraints` as \"No personal data allowed in analysis\"
- User provides background on company goals -> add to `business_context` as \"Recent product changes have affected customer retention\"",
      key = ellmer::type_enum(
        description = "The project context key to update",
        values = btw_memory_keys_project_context()
      ),
      content = ellmer::type_array(
        description = "Content to append to the memory.",
        items = ellmer::type_string()
      )
    )
  }
)


#' Tool: Project Context Memory - Read Context
#'
#' Reads the project context section from the btw memory. Can return the entire
#' project context or a specific set of keys within it.
#'
#' @inheritParams btw_tool_memory_project_context_add
#'
#' @return The requested project context data, or `NULL` if no memory exists
#'   for the project context or selected keys yet.
#'
#' @family Memory tools
#' @export
btw_tool_memory_project_context_read <- function(key = NULL, ..., path = NULL) {
  path <- path_find_btw_memory(path)
  if (!is.null(key)) {
    key <- arg_match(key, btw_memory_keys_project_context(), multiple = TRUE)
  }

  memory_data <- read_btw_memory_yaml(path)

  if (is.null(memory_data$project_context)) {
    return(NULL)
  }

  keys <- key %||% names(memory_data$project_context)
  memory <- memory_data$project_context[keys]

  res <- c()
  for (key in names(memory)) {
    title <- switch(
      key,
      problem_description = "Problem Description",
      objectives = "Objectives",
      constraints = "Constraints",
      business_context = "Business Context"
    )

    value <- memory[[key]]
    if (length(value) > 1) {
      value <- paste(sprintf("* %s", value), collapse = "\n")
    }

    res <- c(res, if (length(res)) "", paste0("### ", title), "", value)
  }

  btw_tool_result(
    value = paste(res, collapse = "\n"),
    data = memory
  )
}

.btw_add_to_tools(
  "btw_tool_memory_project_context_read",
  group = "memory",
  tool = function() {
    ellmer::tool(
      function(key, ...) {
        if (!is.null(key) && "all" %in% key) {
          key <- NULL
        }
        btw_tool_memory_project_context_read(key)
      },
      .name = "btw_tool_memory_project_context_read",
      .annotations = ellmer::tool_annotations(
        title = "Read project context from memory",
        read_only_hint = TRUE,
        open_world_hint = FALSE
      ),
      .description = "Read project memory.

Retrieves stored project context to understand the analysis requirements and provide contextually appropriate responses.

**When to use:**
- At the start of conversations to understand the project scope
- When making analysis recommendations to align with stated objectives
- When the user asks about project goals or requirements
- Before suggesting approaches to ensure they fit within constraints
- When you need to recall business context to interpret results appropriately

**Key scenarios:**
- User asks \"What are we trying to accomplish?\" -> read `objectives`
- User wants analysis suggestions -> read constraints and `success_criteria`
- User asks about project background -> read `business_context`
- You need full context for recommendations -> read without specifying key

**Best practice:** Check project context early in conversations to provide more relevant and targeted assistance.",
      key = ellmer::type_array(
        description = "The project context keys to read. If empty, reads the entire project context.",
        items = ellmer::type_enum(
          values = c("all", btw_memory_keys_project_context()),
          description = "
* all:  All project context keys
* problem_description: High-level description of the business problem to solve
* objectives: Specific analysis goals and questions to answer
* success_criteria: How to measure if the analysis was successful
* constraints: Limitations, requirements, or restrictions for the analysis
* business_context: Domain knowledge, organizational context, or background information"
        ),
        required = FALSE
      )
    )
  }
)


#' Tool: Project Context Memory - Replace Context
#'
#' Replaces content in the project context section of the btw memory.
#'
#' @inheritParams btw_tool_memory_project_context_add
#'
#' @return Invisibly returns the updated memory data
#'
#' @family Memory tools
#' @export
btw_tool_memory_project_context_replace <- function(
  key,
  content,
  ...,
  path = NULL
) {
  path <- path_find_btw_memory(path, must_exist = FALSE)
  key <- arg_match(key, btw_memory_keys_project_context())
  check_character(content)

  if (fs::file_exists(path)) {
    memory_data <- read_btw_memory_yaml(path)
  } else {
    memory_data <- as_btw_memory(list())
  }

  if (key == "problem_description") {
    memory_data$project_context[[key]] <- paste(contents, collapse = "\n\n")
  } else {
    memory_data$project_context[[key]] <- as.character(contents)
  }

  write_btw_memory_yaml(memory_data, path)
  invisible(memory_data)
}

.btw_add_to_tools(
  "btw_tool_memory_project_context_replace",
  group = "memory",
  tool = function() {
    ellmer::tool(
      function(key, contents, ...) {
        btw_tool_memory_project_context_replace(key, contents)
        BtwToolResult("Success.")
      },
      .name = "btw_tool_memory_project_context_replace",
      .annotations = ellmer::tool_annotations(
        title = "Replace project context in memory",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        destructive_hint = TRUE
      ),
      .description = "Replace or correct existing project memory.

**When to use:**
- User corrects previously stated objectives or requirements
- Project scope changes significantly
- User provides more accurate problem description
- Need to reorganize or consolidate scattered information
- User explicitly asks to \"update\" or \"change\" stored project information

**Key scenarios:**
- \"Actually, the goal is prediction, not classification\" -> replace objectives
- \"I was wrong about the constraints\" -> replace constraints
- User provides detailed problem statement to replace vague initial description
- Consolidating multiple business_context entries into organized list

**Caution:** Use sparingly - prefer `add` for new information. Only use `replace` when existing information is wrong, outdated, or needs reorganization.
You may need to use `btw_tool_memory_project_context_read()` to check current content before replacing.",
      key = ellmer::type_enum(
        description = "The project context key to update",
        values = btw_memory_keys_project_context()
      ),
      content = ellmer::type_array(
        description = "Content to replace the memory.",
        items = ellmer::type_string()
      )
    )
  }
)

#' Read btw memory YAML file
#'
#' Reads the btw-memory.yaml file and returns a btw_memory object.
#' If the file doesn't exist, returns an empty btw_memory structure.
#'
#' @param path Path to the YAML file. Defaults to "btw-memory.yaml"
#'
#' @return A list with class "btw_memory" containing `project_context` and
#'   `data_sources`
#'
#' @noRd
read_btw_memory_yaml <- function(path = NULL) {
  path <- path_find_btw_memory(path)

  as_btw_memory(yaml::read_yaml(path) %||% list())
}

as_btw_memory <- function(x) {
  structure(x, class = "btw_memory")
}

#' Write btw memory YAML file
#'
#' Writes a btw_memory object to a YAML file.
#'
#' @param x A btw_memory object (list with project_context and data_sources)
#' @param path Path to the YAML file. Defaults to "btw-memory.yaml"
#' @noRd
write_btw_memory_yaml <- function(x, path = NULL) {
  if (!inherits(x, "btw_memory")) {
    stop("Object must have class 'btw_memory'")
  }

  data <- unclass(x)
  path <- path_find_btw_memory(path, must_exist = FALSE)

  if (identical(compact(data), list())) {
    cli::cli_inform(
      "No memory to write, creating an empty file at {.path {path}}."
    )
    fs::file_touch(path)
    return(invisible(x))
  }

  # Project Context ----
  if (!is.null(data$project_context)) {
    # problem_description should be a single string
    if (!is.null(data$project_context$problem_description)) {
      data$project_context$problem_description <- paste(
        data$project_context$problem_description,
        collapse = " "
      )
    }
  }

  # Data Sources ----
  if (!is.null(data$data_sources) && length(data$data_sources) > 0) {
    for (i in seq_along(data$data_sources)) {
      ds <- data$data_sources[[i]]

      # These should be single strings
      for (field in c("name", "description", "source", "code")) {
        if (!is.null(ds[[field]])) {
          ds[[field]] <- paste(ds[[field]], collapse = "\n")
        }
      }

      # notes should be an array of strings
      if (!is.null(ds$notes)) {
        ds$notes <- as.character(ds$notes)
      }

      # Variables ----
      if (!is.null(ds$variables) && length(ds$variables) > 0) {
        for (j in seq_along(ds$variables)) {
          var <- ds$variables[[j]]

          # name should be a single string
          if (!is.null(var$name)) {
            var$name <- paste(var$name, collapse = "")
          }

          # notes should be an array of strings
          if (!is.null(var$notes)) {
            var$notes <- as.character(var$notes)
          }

          ds$variables[[j]] <- var
        }
      }

      data$data_sources[[i]] <- ds
    }
  }

  yaml::write_yaml(data, path, indent.mapping.sequence = TRUE, indent = 2)

  invisible(x)
}


path_find_btw_memory <- function(path = NULL, must_exist = TRUE) {
  check_string(path, allow_null = TRUE, call = caller_env())
  check_bool(must_exist, call = caller_env())

  if (!is.null(path)) {
    if (must_exist && !file.exists(path)) {
      cli::cli_abort("File {.path {path}} does not exist.")
    }
    return(path)
  }

  root <- path_find_project_root()

  if (is.null(root)) {
    cli::cli_abort(c(
      "Could not find the project root. Are you in a project directory?",
      "i" = "The project root is typically the directory containing a {.file DESCRIPTION} file, a {.file .git} directory, or a {.field VS Code} or {.field RStudio} project.",
      "i" = "Create an empty {.file btw-memory.yaml} file in the project root to store your memory."
    ))
  }

  paths <- fs::path(root, c("btw-memory.yaml", "btw-memory.yml"))
  paths <- paths[file.exists(paths)]

  if (length(paths) > 0) {
    return(paths[1])
  }

  if (must_exist) {
    cli::cli_abort(
      "No {.pkg btw} memory file found in the project directory {.path {root}}."
    )
  }

  fs::path(root, "btw-memory.yaml")
}
