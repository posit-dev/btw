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
        "Read the contents of a text file.",
        "",
        "OUTPUT FORMAT:",
        "Each line is prefixed with `line_number:hash|` where:",
        "- `line_number` is the 1-based line number",
        "- `hash` is a 3-character content hash used for edit validation",
        "- `|` separates the prefix from the actual line content",
        "",
        "Example output:",
        "  1:a3f|library(dplyr)",
        "  2:b1c|data <- read_csv('input.csv')",
        "  3:d4e|result <- data |> filter(x > 0)",
        "",
        "USAGE NOTES:",
        "- Use line references (e.g., '2:b1c') with btw_tool_files_edit to make targeted edits.",
        "- For large files, use line_start and line_end to read specific sections.",
        "- Binary files (images, compiled code, etc.) cannot be read with this tool.",
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
          "Relative path to a text file (e.g., 'R/utils.R', 'data/input.csv'). Must be within the current working directory."
        ),
        line_start = ellmer::type_number(
          "First line to read (1-based, inclusive). Defaults to 1.",
          required = FALSE
        ),
        line_end = ellmer::type_number(
          "Last line to read (1-based, inclusive). Defaults to 1000. Adjust to read more lines or combine with line_start to read a specific range.",
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
  con <- NULL # avoid "no visible binding" note for con in with_connection()

  tryCatch(
    {
      # Read first 8KB of the file
      bytes <- withr::with_connection(list(con = file(file_path, "rb")), {
        readBin(con, what = "raw", n = 8192)
      })

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

      # Trim any incomplete UTF-8 sequence at the end of the buffer
      # before validating (the buffer may have split a multi-byte character)
      bytes <- trim_incomplete_trailing_utf8(bytes)

      # Validate as UTF-8
      text <- rawToChar(bytes)
      if (Encoding(text) == "unknown" && !validUTF8(text)) {
        return(FALSE)
      }

      return(TRUE)
    },
    error = function(e) {
      warning("Error reading file: ", e$message)
      return(NA)
    }
  )
}


# Trim an incomplete UTF-8 multi-byte sequence from the end of a raw vector.
#
# When readBin() reads a fixed number of bytes, it can split a UTF-8 character
# at the buffer boundary. This can make validUTF8() reject an otherwise valid
# stream. To avoid that, this function inspects only the last few bytes:
#
# * UTF-8 code points are at most 4 bytes, so only the last 4 bytes matter.
# * Starting at the end, it walks backward through trailing continuation bytes.
# * If it finds a valid lead byte (2/3/4-byte lead, defined in LEADS), it checks
#   whether enough bytes remain to complete that sequence.
# * If incomplete, it trims from that lead byte to the end; otherwise keeps input.
# * If it encounters an invalid trailing byte, it trims that invalid suffix.
# * If the suffix is continuation-only with no lead in the inspection window,
#   it trims that suffix as incomplete.
trim_incomplete_trailing_utf8 <- function(bytes) {
  n <- length(bytes)
  if (n == 0L) {
    return(bytes)
  }

  # UTF-8 byte-class constants
  ASCII_MAX <- 0x7FL
  CONT <- list(min = 0x80L, max = 0xBFL)
  LEADS <- list(
    list(min = 0xC2L, max = 0xDFL, width = 2L), # 2-byte lead
    list(min = 0xE0L, max = 0xEFL, width = 3L), # 3-byte lead
    list(min = 0xF0L, max = 0xF4L, width = 4L) # 4-byte lead
  )
  MAX_TAIL_SPAN <- 3L

  in_range <- function(b, r) {
    b >= r$min && b <= r$max
  }

  # If sequence starting at i needs `width` bytes, trim if incomplete
  keep_or_trim <- function(i, width) {
    if (i + width - 1L > n) bytes[seq_len(i - 1L)] else bytes
  }

  i <- n
  lower <- max(1L, n - MAX_TAIL_SPAN)

  while (i >= lower) {
    b <- as.integer(bytes[i])

    if (b <= ASCII_MAX) {
      return(bytes) # ASCII tail cannot be incomplete UTF-8
    }

    for (lead in LEADS) {
      if (in_range(b, lead)) {
        return(keep_or_trim(i, lead$width))
      }
    }

    if (in_range(b, CONT)) {
      i <- i - 1L
      next
    }

    # Invalid byte near end: trim it
    return(bytes[seq_len(i - 1L)])
  }

  # Continuation-only suffix with no visible lead in window
  bytes[seq_len(lower - 1L)]
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
