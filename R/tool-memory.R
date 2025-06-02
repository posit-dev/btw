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

  as_btw_memory(yaml::read_yaml(path))
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

  yaml::write_yaml(data, path)

  invisible(x)
}


path_find_btw_memory <- function(path = NULL, must_exist = TRUE) {
  check_string(path, allow_null = TRUE, call = caller_env())
  check_bool(must_exist, call = caller_env())

  if (!is.null(path)) {
    if (must_exist && !file.exists(path)) {
      cli::cli_abort("File '{file}' does not exist.")
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
