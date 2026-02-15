#' @include tool-result.R
NULL

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
  check_within_wd = TRUE,
  include_hashline = FALSE
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

  display_md <- paste(
    md_code_block(fs::path_ext(path), contents),
    collapse = "\n"
  )

  if (include_hashline) {
    value <- paste(
      format_hashlines(contents, start_line = max(line_start, 1)),
      collapse = "\n"
    )
  } else {
    value <- display_md
  }

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
          check_within_wd = TRUE,
          include_hashline = TRUE
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

# --- Shared helpers used by multiple tool-files-*.R files ---

hashline <- function(line) {
  substr(rlang::hash(substr(trimws(line), 1, 80)), 1, 3)
}

format_hashlines <- function(lines, start_line = 1L) {
  hashes <- vapply(lines, hashline, character(1), USE.NAMES = FALSE)
  line_nums <- seq(start_line, length.out = length(lines))
  paste0(line_nums, ":", hashes, "|", lines)
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
