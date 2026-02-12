#' @include tool-result.R
NULL

hashline <- function(line) {
  substr(rlang::hash(substr(trimws(line), 1, 80)), 1, 3)
}

format_hashlines <- function(lines, start_line = 1L) {
  hashes <- vapply(lines, hashline, character(1), USE.NAMES = FALSE)
  line_nums <- seq(start_line, length.out = length(lines))
  paste0(line_nums, ":", hashes, "|", lines)
}

#' Tool: List files
#'
#' @examples
#' withr::with_tempdir({
#'   write.csv(mtcars, "mtcars.csv")
#'
#'   btw_tool_files_list(type = "file")
#' })
#'
#' @param path Path to a directory or file for which to get information. The
#'   `path` must be in the current working directory. If `path` is a directory,
#'   we use [fs::dir_info()] to list information about files and directories in
#'   `path` (use `type` to pick only one or the other). If `path` is a file, we
#'   show information about that file.
#' @param type File type(s) to return, one of `"any"` or `"file"` or
#'   `"directory"`.
#' @inheritParams fs::dir_ls
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character table of file information.
#'
#' @family files tools
#' @export
btw_tool_files_list <- function(path, type, regexp, `_intent`) {}

btw_tool_files_list_impl <- function(
  path = NULL,
  type = c("any", "file", "directory"),
  regexp = "",
  check_within_wd = TRUE
) {
  path <- path %||% getwd()
  type <- type %||% "any"
  check_string(path) # one path a time, please

  type <- arg_match(type, multiple = TRUE)
  if (identical(type, c("any", "file", "directory"))) {
    type <- c("file", "directory", "symlink")
  }

  regexp <- if (nzchar(regexp)) regexp

  if (check_within_wd) {
    check_path_within_current_wd(path, call = parent.frame())
  }

  info <-
    if (fs::is_file(path)) {
      if (!fs::file_exists(path)) {
        cli::cli_abort(
          "The path {.path {path}} does not exist. Did you use a relative path?"
        )
      }
      fs::file_info(path)
    } else {
      fs::dir_info(path, type = type, regexp = regexp, recurse = TRUE)
    }

  info <- info[!is_common_ignorable_files(info$path), ]

  if (nrow(info) == 0) {
    return(sprintf("No %s found in %s", paste(type, collapse = "/"), path))
  }

  info$path <- fs::path_rel(info$path)

  fields <- c("path", "type", "size", "modification_time")

  md_res <- md_table(info[fields])

  btw_tool_result(
    md_res,
    data = info[fields],
    display = list(markdown = md_res)
  )
}

.btw_add_to_tools(
  name = "btw_tool_files_list",
  group = "files",
  alias_name = "btw_tool_files_list_files",
  tool = function() {
    ellmer::tool(
      function(path = NULL, type = c("any", "file", "directory"), regexp = "") {
        btw_tool_files_list_impl(
          path = path,
          type = type,
          regexp = regexp,
          # LLM tool calls should be restricted to the working directory
          check_within_wd = TRUE
        )
      },
      name = "btw_tool_files_list",
      description = r"---(List files or directories in the project.

WHEN TO USE:
* Use this tool to discover the file structure of a project.
* When you want to understand the project structure, use `type = "directory"` to list all directories.
* When you want to find a specific file, use `type = "file"` and `regexp` to filter files by name or extension.

CAUTION: Do not list all files in a project, instead prefer listing files in a specific directory with a `regexp` to filter to files of interest.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Project Files",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        path = ellmer::type_string(
          paste(
            "The relative path to a folder or file.",
            "If `path` is a directory, all files or directories (see `type`) are listed.",
            'Use `"."` to refer to the current working directory.',
            "If `path` is a file, information for just the selected file is listed."
          ),
          required = FALSE
        ),
        type = ellmer::type_enum(
          "Whether to list files, directories or any file type, default is `any`.",
          values = c("any", "file", "directory"),
          required = FALSE
        ),
        regexp = ellmer::type_string(
          paste(
            'A regular expression to use to identify files, e.g. `regexp="[.]csv$"` to find files with a `.csv` extension.',
            "Note that it's best to be as general as possible to find the file you want."
          ),
          required = FALSE
        )
      )
    )
  }
)

#' Tool: Read a file
#'
#' @examples
#' withr::with_tempdir({
#'   write.csv(mtcars, "mtcars.csv")
#'
#'   btw_tool_files_read("mtcars.csv", line_end = 5)
#' })
#'
#' @param path Path to a file for which to get information. The `path` must be
#'   in the current working directory.
#' @param line_start Starting line to read, defaults to 1 (starting from the
#'   first line).
#' @param line_end Ending line to read, defaults to 1000. Change only this value
#'   if you want to read more or fewer lines. Use in combination with
#'   `line_start` to read a specific line range of the file.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a character vector of lines from the file.
#'
#' @family files tools
#' @export
btw_tool_files_read <- function(
  path,
  line_start,
  line_end,
  `_intent`
) {}

btw_tool_files_read_impl <- function(
  path,
  line_start = 1,
  line_end = 1000,
  check_within_wd = TRUE
) {
  if (check_within_wd) {
    check_path_within_current_wd(path, call = parent.frame())
  }

  if (!fs::is_file(path) || !fs::file_exists(path)) {
    cli::cli_abort(
      "Path {.path {path}} is not a file or does not exist. Check the path and ensure that it is provided as a relative path."
    )
  }

  if (!isTRUE(is_text_file(path))) {
    cli::cli_abort(
      "Path {.path {path}} appears to be a binary file or cannot be read as text.",
      call = parent.frame()
    )
  }

  contents <- read_lines(path, n = line_end)
  contents <- contents[seq(max(line_start, 1), min(line_end, length(contents)))]

  # Model-facing output: hashline-annotated, no code fences
  hashline_output <- format_hashlines(contents, start_line = max(line_start, 1))
  value <- paste(hashline_output, collapse = "\n")

  # User-facing display: clean code block with syntax highlighting (unchanged)
  display_md <- paste(md_code_block(fs::path_ext(path), contents), collapse = "\n")

  BtwTextFileToolResult(
    value,
    extra = list(
      path = fs::path_rel(path),
      display = list(
        markdown = display_md,
        title = HTML(title_with_open_file_button("Read", path))
      )
    )
  )
}

title_with_open_file_button <- function(verb, path) {
  path_file <- fs::path_file(path)

  icon <- tool_icon("codicons/go-to-file")

  if (rstudioapi::hasFun("navigateToFile")) {
    res <- glue_(
      r"(
      {{verb}}
      <code>{{path_file}}</code>
      <bslib-tooltip placement="top">
        <template>Go to file</template>
        <button class="btw-open-file btn btn-sm border-0"
         data-path="{{path}}"
         aria-label="Go to {{path_file}} in your IDE"
         style="display: var(--_display, none);"
        >{{ icon }}</button>
      </bslib-tooltip>
      )"
    )
  } else {
    res <- glue_('{{verb}} <code>{{path_file}}</code>')
  }
  HTML(res)
}

BtwTextFileToolResult <- S7::new_class(
  "BtwTextFileToolResult",
  parent = BtwToolResult
)

.btw_add_to_tools(
  name = "btw_tool_files_read",
  group = "files",
  alias_name = "btw_tool_files_read_text_file",
  tool = function() {
    ellmer::tool(
      function(path, line_start = 1, line_end = 1000) {
        btw_tool_files_read_impl(
          path = path,
          line_start = line_start,
          line_end = line_end,
          # LLM tool calls should be restricted to the working directory
          check_within_wd = TRUE
        )
      },
      name = "btw_tool_files_read",
      description = paste(
        "Read a text file. Returns content with hashline annotations.",
        "",
        "OUTPUT FORMAT:",
        "Each line is prefixed with `line_number:hash|` where:",
        "- `line_number` is the 1-based line number in the file",
        "- `hash` is a 3-character content hash for edit validation",
        "- `|` separates the prefix from the line content",
        "",
        "Example output:",
        "  1:a3f|function hello() {",
        "  2:b1c|    return(\"world\")",
        "  3:d4e|}",
        "",
        "Use these line references with `btw_tool_files_edit` to make",
        "targeted edits without rewriting the entire file.",
        sep = "\n"
      ),
      annotations = ellmer::tool_annotations(
        title = "Read File",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        path = ellmer::type_string(
          "The relative path to a file that can be read as text, such as a CSV, JSON, HTML, markdown file, etc.",
        ),
        line_start = ellmer::type_number(
          "Starting line to read, defaults to 1 (starting from the first line).",
          required = FALSE
        ),
        line_end = ellmer::type_number(
          paste(
            "Ending line to read, defaults to 1000.",
            "Change only this value if you want to read more or fewer lines.",
            "Use in combination with `line_start` to read a specific line range of the file."
          ),
          required = FALSE
        )
      )
    )
  }
)

is_text_file <- function(file_path) {
  # Note: this function was written by claude-3.7-sonnet.
  # Try to read the first chunk of the file as binary
  tryCatch(
    {
      # Read first 8KB of the file
      con <- file(file_path, "rb")
      bytes <- readBin(con, what = "raw", n = 8192)
      close(con)

      # If file is empty, consider it text
      if (length(bytes) == 0) {
        return(TRUE)
      }

      # Check for NULL bytes (common in binary files)
      if (any(bytes == as.raw(0))) {
        return(FALSE)
      }

      # Count control characters (excluding common text file control chars)
      # Allow: tab (9), newline (10), carriage return (13)
      allowed_control <- as.raw(c(9, 10, 13))
      control_chars <- bytes[bytes < as.raw(32) & !(bytes %in% allowed_control)]

      # If more than 10% of the first 8KB are control characters, likely binary
      if (length(control_chars) / length(bytes) > 0.1) {
        return(FALSE)
      }

      # Check for high proportion of extended ASCII or non-UTF8 characters
      extended_chars <- bytes[bytes > as.raw(127)]
      if (length(extended_chars) / length(bytes) > 0.3) {
        # Try to interpret as UTF-8
        text <- rawToChar(bytes)
        if (Encoding(text) == "unknown" && !validUTF8(text)) {
          return(FALSE)
        }
      }

      # If we've made it this far, it's likely a text file
      return(TRUE)
    },
    error = function(e) {
      warning("Error reading file: ", e$message)
      return(NA)
    }
  )
}

check_path_exists <- function(path) {
  if (!fs::file_exists(path) && !fs::dir_exists(path)) {
    cli::cli_abort("The path {.path {path}} does not exist.")
  }
}

check_path_within_current_wd <- function(path, call = parent.frame()) {
  if (!fs::path_has_parent(path, getwd())) {
    cli::cli_abort(
      "You are not allowed to list or read files outside of the project directory. Make sure that `path` is relative to the current working directory.",
      call = call
    )
  }
}

is_common_ignorable_files <- function(paths) {
  ignorable_files <- c(".DS_Store", "Thumbs.db")

  ignorable_dir <- c(
    # Version control
    ".git",
    ".svn",
    ".hg",
    ".bzr",

    # Package management
    "node_modules",
    "bower_components",
    "jspm_packages",

    # Python
    ".venv",
    "venv",
    "__pycache__",
    ".pytest_cache",
    "eggs",
    ".eggs",
    ".tox",
    ".nox",
    "*.egg-info",
    "*.egg",

    # R specific
    "renv/library",
    ".Rproj.user",
    "packrat/lib",
    "packrat/src",

    # JavaScript/TypeScript
    "out",
    ".next",
    ".nuxt",
    ".cache",

    # Docker
    ".docker",

    # Documentation builds
    "_site",
    "site",
    "docs/_build",
    "docs/build",
    "public"
  )
  is_ignorable_file <- fs::path_file(paths) %in% ignorable_files
  ignorable_dir_combo <- grep("/", ignorable_dir, fixed = TRUE, value = TRUE)
  ignorable_dir_simple <- setdiff(ignorable_dir, ignorable_dir_combo)

  is_in_ignorable_dir <- map_lgl(
    fs::path_split(fs::path_dir(paths)),
    function(path_parts) {
      some(path_parts, function(part) part %in% ignorable_dir_simple) ||
        # R Markdown built files
        any(grepl("_files$", path_parts)) ||
        some(
          ignorable_dir_combo,
          function(id) grepl(id, fs::path_join(path_parts), fixed = TRUE)
        )
    }
  )

  is_ignorable_file | is_in_ignorable_dir
}

#' Tool: Write a text file
#'
#' @examples
#' withr::with_tempdir({
#'   btw_tool_files_write("example.txt", "Hello\nWorld!")
#'   readLines("example.txt")
#' })
#'
#' @param path Path to the file to write. The `path` must be in the current
#'   working directory.
#' @param content The text content to write to the file. This should be the
#'   complete content as the file will be overwritten.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a message confirming the file was written.
#'
#' @family files tools
#' @export
btw_tool_files_write <- function(path, content, `_intent`) {}

btw_tool_files_write_impl <- function(path, content) {
  check_string(path)
  check_string(content)
  check_path_within_current_wd(path)

  if (fs::is_dir(path)) {
    cli::cli_abort(
      "Path {.path {path}} is a directory, not a file. Please provide a file path."
    )
  }

  # Ensure the directory exists
  dir_path <- fs::path_dir(path)
  if (dir_path != "." && !fs::dir_exists(dir_path)) {
    fs::dir_create(dir_path, recurse = TRUE)
  }

  previous_content <- if (fs::file_exists(path)) read_file(path)

  write_file(content, path)

  BtwWriteFileToolResult(
    "Success",
    extra = list(
      path = path,
      content = content,
      previous_content = previous_content,
      display = list(
        markdown = md_code_block(fs::path_ext(path), content),
        title = HTML(title_with_open_file_button("Write", path)),
        show_request = FALSE,
        icon = tool_icon("file-save")
      )
    )
  )
}

BtwWriteFileToolResult <- S7::new_class(
  "BtwWriteFileToolResult",
  parent = BtwToolResult
)

S7::method(contents_shinychat, BtwWriteFileToolResult) <- function(content) {
  res <- shinychat::contents_shinychat(
    S7::super(content, ellmer::ContentToolResult)
  )

  if (!is_installed("diffviewer")) {
    cli::cli_warn(
      "Install the {.pkg diffviewer} package for rich file diffs in {.fn btw::btw_app}: {.run install.packages('diffviewer')}",
      call = quote(btw_tool_files_write_text_file),
      .frequency = "once",
      .frequency_id = "btw-tool-files-write-text-file-diffviewer"
    )
    return(res)
  }

  new <- content@extra$content
  old <- content@extra$previous_content

  dir <- withr::local_tempdir()
  path_ext <- fs::path_ext(content@extra$path)
  path_file <- fs::path_ext_remove(fs::path_file(content@extra$path))

  path_old <- fs::path(dir, sprintf("%s", path_file), ext = path_ext)
  path_new <- fs::path(dir, sprintf("%s.new", path_file), ext = path_ext)

  write_file(old %||% "", path_old)
  write_file(new %||% "", path_new)

  res$value <- diffviewer::visual_diff(path_old, path_new)
  res$value_type <- "html"
  res$class <- "btw-tool-result-write-file"
  res
}

.btw_add_to_tools(
  name = "btw_tool_files_write",
  group = "files",
  alias_name = "btw_tool_files_write_text_file",
  tool = function() {
    ellmer::tool(
      btw_tool_files_write_impl,
      name = "btw_tool_files_write",
      description = 'Write content to a text file.

If the file doesn\'t exist, it will be created, along with any necessary parent directories.

WHEN TO USE:
Use this tool only when the user has explicitly asked you to write or create a file.
Do not use for temporary or one-off content; prefer direct responses for those cases.
Consider checking with the user to ensure that the file path is correct and that they want to write to a file before calling this tool.

CAUTION:
This completely overwrites any existing file content.
To modify an existing file, first read its content using `btw_tool_files_read`, make your changes to the text, then write back the complete modified content.
',
      annotations = ellmer::tool_annotations(
        title = "Write File",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = TRUE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        path = ellmer::type_string(
          "The relative path to the file to write. The file will be created if it doesn't exist, or overwritten if it does."
        ),
        content = ellmer::type_string(
          "The complete text content to write to the file."
        )
      )
    )
  }
)

# --- Edit Tool ---

# Parse a single line reference "N:hash" into list(line = N, hash = "abc")
parse_line_ref <- function(ref) {
  parts <- strsplit(ref, ":", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    cli::cli_abort("Invalid line reference: {.val {ref}}. Expected format: 'line_number:hash'.")
  }
  line_num <- suppressWarnings(as.integer(parts[1]))
  if (is.na(line_num)) {
    cli::cli_abort("Invalid line number in reference: {.val {ref}}.")
  }
  list(line = line_num, hash = parts[2])
}

# Parse the `line` field from an edit, which may be "N:hash" or "N:hash,M:hash"
parse_edit_line_field <- function(line_str) {
  refs <- strsplit(line_str, ",", fixed = TRUE)[[1]]
  if (length(refs) == 1) {
    start <- parse_line_ref(refs[1])
    list(start = start, end = start)
  } else if (length(refs) == 2) {
    list(start = parse_line_ref(refs[1]), end = parse_line_ref(refs[2]))
  } else {
    cli::cli_abort("Invalid line field: {.val {line_str}}. Expected 'N:hash' or 'N:hash,M:hash'.")
  }
}

validate_edit_hashes <- function(edits_parsed, file_lines) {
  n_lines <- length(file_lines)
  mismatches <- character()

  for (i in seq_along(edits_parsed)) {
    edit <- edits_parsed[[i]]

    # Validate start line hash (skip for line 0 / insert at top)
    if (edit$start$line > 0) {
      if (edit$start$line > n_lines) {
        mismatches <- c(mismatches, sprintf(
          "Edit %d: line %d does not exist (file has %d lines).",
          i, edit$start$line, n_lines
        ))
      } else {
        expected_hash <- hashline(file_lines[edit$start$line])
        if (expected_hash != edit$start$hash) {
          mismatches <- c(mismatches, sprintf(
            "Edit %d: hash mismatch on line %d (expected '%s', got '%s'). File may have changed since last read.",
            i, edit$start$line, expected_hash, edit$start$hash
          ))
        }
      }
    }

    # Validate end line hash (for replace_range)
    if (edit$action == "replace_range" && edit$end$line != edit$start$line) {
      if (edit$end$line > n_lines) {
        mismatches <- c(mismatches, sprintf(
          "Edit %d: end line %d does not exist (file has %d lines).",
          i, edit$end$line, n_lines
        ))
      } else {
        expected_hash <- hashline(file_lines[edit$end$line])
        if (expected_hash != edit$end$hash) {
          mismatches <- c(mismatches, sprintf(
            "Edit %d: hash mismatch on end line %d (expected '%s', got '%s').",
            i, edit$end$line, expected_hash, edit$end$hash
          ))
        }
      }
    }
  }

  if (length(mismatches) > 0) {
    cli::cli_abort(c(
      "Edit validation failed. Re-read the file to get updated line references.",
      set_names(mismatches, rep("x", length(mismatches)))
    ))
  }
}

check_edit_overlaps <- function(edits_parsed) {
  # Compute affected line ranges for each edit
  ranges <- lapply(edits_parsed, function(edit) {
    switch(edit$action,
      "replace" = c(edit$start$line, edit$start$line),
      "insert_after" = c(edit$start$line, edit$start$line),
      "replace_range" = c(edit$start$line, edit$end$line)
    )
  })

  # Check each pair for overlaps (only for replace/replace_range, not inserts)
  for (i in seq_along(ranges)) {
    for (j in seq_along(ranges)) {
      if (i >= j) next
      ri <- ranges[[i]]
      rj <- ranges[[j]]

      # Two replace/replace_range operations overlap if their ranges intersect
      # insert_after doesn't "occupy" a range, so skip overlap check for inserts
      if (edits_parsed[[i]]$action == "insert_after" ||
          edits_parsed[[j]]$action == "insert_after") {
        next
      }

      if (ri[1] <= rj[2] && rj[1] <= ri[2]) {
        cli::cli_abort(
          "Edits {i} and {j} have overlapping line ranges ({ri[1]}-{ri[2]} and {rj[1]}-{rj[2]})."
        )
      }
    }
  }
}

apply_edits <- function(file_lines, edits_parsed) {
  # Sort edits by start line in DESCENDING order (bottom-to-top)
  # For same line, process replace before insert_after
  order_keys <- vapply(edits_parsed, function(e) {
    # Primary: line number descending (negate for descending sort)
    # Secondary: replace/replace_range before insert_after
    priority <- if (e$action == "insert_after") 0 else 1
    e$start$line * 10 + priority
  }, numeric(1))

  edits_parsed <- edits_parsed[order(order_keys, decreasing = TRUE)]

  for (edit in edits_parsed) {
    file_lines <- switch(edit$action,
      "replace" = {
        n <- edit$start$line
        before <- if (n > 1) file_lines[seq_len(n - 1)] else character()
        after <- if (n < length(file_lines)) file_lines[seq(n + 1, length(file_lines))] else character()
        c(before, edit$content, after)
      },
      "insert_after" = {
        n <- edit$start$line
        if (n == 0) {
          # Insert at top of file
          c(edit$content, file_lines)
        } else {
          before <- file_lines[seq_len(n)]
          after <- if (n < length(file_lines)) file_lines[seq(n + 1, length(file_lines))] else character()
          c(before, edit$content, after)
        }
      },
      "replace_range" = {
        start_n <- edit$start$line
        end_n <- edit$end$line
        before <- if (start_n > 1) file_lines[seq_len(start_n - 1)] else character()
        after <- if (end_n < length(file_lines)) file_lines[seq(end_n + 1, length(file_lines))] else character()
        c(before, edit$content, after)
      }
    )
  }

  file_lines
}

#' Tool: Edit a text file
#'
#' @param path Path to the file to edit. The `path` must be in the current
#'   working directory.
#' @param edits A list of edit operations. Each edit is a named list with
#'   `action`, `line`, and `content` fields.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a message confirming the edits were applied.
#'
#' @family files tools
#' @export
btw_tool_files_edit <- function(path, edits, `_intent`) {}

btw_tool_files_edit_impl <- function(path, edits) {
  check_string(path)
  check_path_within_current_wd(path)

  if (!fs::is_file(path) || !fs::file_exists(path)) {
    cli::cli_abort(
      "Path {.path {path}} is not a file or does not exist."
    )
  }

  if (!is.list(edits) || length(edits) == 0) {
    cli::cli_abort("The `edits` parameter must be a non-empty list of edit operations.")
  }

  # Read current file
  file_lines <- read_lines(path)
  previous_content <- paste(file_lines, collapse = "\n")

  # Parse all edits
  edits_parsed <- lapply(edits, function(edit) {
    action <- edit$action %||% cli::cli_abort("Each edit must have an 'action' field.")
    line_str <- edit$line %||% cli::cli_abort("Each edit must have a 'line' field.")
    content <- edit$content %||% character()
    # Ensure content is a character vector
    content <- as.character(content)

    if (!action %in% c("replace", "insert_after", "replace_range")) {
      cli::cli_abort("Invalid action: {.val {action}}. Must be 'replace', 'insert_after', or 'replace_range'.")
    }

    parsed <- parse_edit_line_field(line_str)

    if (action == "replace_range" && parsed$start$line >= parsed$end$line) {
      cli::cli_abort("For 'replace_range', start line must be less than end line. Got: {.val {line_str}}.")
    }

    list(
      action = action,
      start = parsed$start,
      end = parsed$end,
      content = content
    )
  })

  # Validate hashes against current file state
  validate_edit_hashes(edits_parsed, file_lines)

  # Check for overlapping edits
  check_edit_overlaps(edits_parsed)

  # Apply edits
  new_lines <- apply_edits(file_lines, edits_parsed)
  new_content <- paste(new_lines, collapse = "\n")

  # Write back
  write_file(new_content, path)

  BtwEditFileToolResult(
    sprintf("Successfully applied %d edit(s) to %s.", length(edits), path),
    extra = list(
      path = path,
      content = new_content,
      previous_content = previous_content,
      display = list(
        markdown = md_code_block(fs::path_ext(path), new_content),
        title = HTML(title_with_open_file_button("Edit", path)),
        show_request = FALSE,
        icon = tool_icon("file-save")
      )
    )
  )
}

BtwEditFileToolResult <- S7::new_class(
  "BtwEditFileToolResult",
  parent = BtwToolResult
)

S7::method(contents_shinychat, BtwEditFileToolResult) <- function(content) {
  res <- shinychat::contents_shinychat(
    S7::super(content, ellmer::ContentToolResult)
  )

  if (!is_installed("diffviewer")) {
    return(res)
  }

  new <- content@extra$content
  old <- content@extra$previous_content

  dir <- withr::local_tempdir()
  path_ext <- fs::path_ext(content@extra$path)
  path_file <- fs::path_ext_remove(fs::path_file(content@extra$path))

  path_old <- fs::path(dir, sprintf("%s", path_file), ext = path_ext)
  path_new <- fs::path(dir, sprintf("%s.new", path_file), ext = path_ext)

  write_file(old %||% "", path_old)
  write_file(new %||% "", path_new)

  res$value <- diffviewer::visual_diff(path_old, path_new)
  res$value_type <- "html"
  res$class <- "btw-tool-result-edit-file"
  res
}

.btw_add_to_tools(
  name = "btw_tool_files_edit",
  group = "files",
  tool = function() {
    ellmer::tool(
      btw_tool_files_edit_impl,
      name = "btw_tool_files_edit",
      description = 'Edit a text file using hashline references from btw_tool_files_read.

WHEN TO USE:
Use this tool to make targeted edits to a file after reading it with btw_tool_files_read.
Each edit references lines by their `line_number:hash` identifier from the read output.

ACTIONS:
- "replace": Replace a single line. Use `content: []` to delete a line.
- "insert_after": Insert new lines after the referenced line. Use `line: "0:000"` to insert at the top of the file.
- "replace_range": Replace a range of lines. Use `line: "start_line:hash,end_line:hash"`.

IMPORTANT:
- Always read the file first with btw_tool_files_read to get current line hashes.
- If an edit fails due to hash mismatch, re-read the file to get updated hashes.
- The `content` array contains the new lines (one string per line, no trailing newlines).
- Multiple edits in one call are applied atomically (all or nothing).
',
      annotations = ellmer::tool_annotations(
        title = "Edit File",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        path = ellmer::type_string(
          "The relative path to the file to edit. Must have been previously read with btw_tool_files_read."
        ),
        edits = ellmer::type_array(
          description = "Array of edit operations to apply to the file.",
          items = ellmer::type_object(
            .description = "A single edit operation.",
            action = ellmer::type_enum(
              values = c("replace", "insert_after", "replace_range"),
              description = 'The edit action: "replace" (single line), "insert_after" (insert new lines after reference), or "replace_range" (replace a range of lines).'
            ),
            line = ellmer::type_string(
              'Line reference from btw_tool_files_read output. Format: "line_number:hash" for replace/insert_after, or "start_line:hash,end_line:hash" for replace_range. Use "0:000" with insert_after to insert at the top of the file.'
            ),
            content = ellmer::type_array(
              description = "Array of new line strings. Each element is one line of content. Use an empty array [] to delete lines.",
              items = ellmer::type_string()
            )
          )
        )
      )
    )
  }
)
