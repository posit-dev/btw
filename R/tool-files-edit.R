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
    cli::cli_abort(
      "The `edits` parameter must be a non-empty list of edit operations."
    )
  }

  # Read current file
  file_lines <- read_lines(path)
  previous_content <- paste(file_lines, collapse = "\n")

  # Parse all edits
  edits_parsed <- lapply(edits, function(edit) {
    action <- edit$action %||%
      cli::cli_abort("Each edit must have an 'action' field.")
    line_str <- edit$line %||%
      cli::cli_abort("Each edit must have a 'line' field.")
    content <- edit$content %||% character()
    # Ensure content is a character vector
    content <- as.character(content)

    if (!action %in% c("replace", "insert_after", "replace_range")) {
      cli::cli_abort(
        "Invalid action: {.val {action}}. Must be 'replace', 'insert_after', or 'replace_range'."
      )
    }

    parsed <- parse_edit_line_field(line_str)

    if (action == "replace_range" && parsed$start$line >= parsed$end$line) {
      cli::cli_abort(
        "For 'replace_range', start line must be less than end line. Got: {.val {line_str}}."
      )
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

  # Build model-facing response with updated hashlines and shift hints
  response_value <- build_edit_response(
    path = path,
    new_lines = new_lines,
    old_line_count = length(file_lines),
    edits_parsed = edits_parsed
  )

  BtwFileDiffToolResult(
    response_value,
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

.btw_add_to_tools(
  name = "btw_tool_files_edit",
  group = "files",
  tool = function() {
    ellmer::tool(
      btw_tool_files_edit_impl,
      name = "btw_tool_files_edit",
      description = r"---(Edit a text file using hashline references for precise, targeted modifications.

WHEN TO USE:
Use this tool when you need to modify specific lines in a file. It uses hashline
references from btw_tool_files_read (e.g., "42:a3f") to ensure you're editing
exactly the lines you intend, even if the file has been modified.

EDIT ACTIONS:
- "replace": Replace a single line with new content. Use `content: []` to delete the line.
- "insert_after": Insert new lines after a reference. Use `line: "0:000"` to insert at the start of the file.
- "replace_range": Replace multiple consecutive lines. Format: `line: "start:hash,end:hash"`.

RESPONSE FORMAT:
Returns updated hashlines for edited regions plus surrounding context (1 line before/after).
Includes shift hints when line numbers change, allowing you to chain multiple edits
without re-reading the entire file.

WORKFLOW:
1. Read the file with btw_tool_files_read to get hashlines.
2. Make edits using the line references from the read response.
3. For follow-up edits, use hashlines from the edit response and apply any shift hints.
4. If a hash mismatch error occurs, re-read the file to get fresh references.

NOTES:
- Multiple edits in one call are applied atomically (all succeed or all fail).
- Edits must not have overlapping line ranges.
- Each element in `content` is one line; do not include trailing newlines.
---)",
      annotations = ellmer::tool_annotations(
        title = "Edit File",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      convert = FALSE, # avoid having edits be converted into a data.frame
      arguments = list(
        path = ellmer::type_string(
          "Relative path to the file to edit. Must be within the current working directory."
        ),
        edits = ellmer::type_array(
          description = "Array of edit operations to apply. All edits are validated and applied atomically.",
          items = ellmer::type_object(
            .description = "A single edit operation with action, line reference, and content.",
            action = ellmer::type_enum(
              values = c("replace", "insert_after", "replace_range"),
              description = "The type of edit: 'replace' (single line), 'insert_after' (add lines after reference), or 'replace_range' (replace consecutive lines)."
            ),
            line = ellmer::type_string(
              "Hashline reference specifying where to edit. Format: 'line_number:hash' for replace/insert_after, or 'start:hash,end:hash' for replace_range. Use '0:000' with insert_after to prepend to the file."
            ),
            content = ellmer::type_array(
              description = "New content as an array of strings, one per line. Use [] to delete lines.",
              items = ellmer::type_string()
            )
          )
        )
      )
    )
  }
)

# --- Edit tool helpers ---

# Parse a single line reference "N:hash" into list(line = N, hash = "abc")
parse_line_ref <- function(ref) {
  parts <- strsplit(ref, ":", fixed = TRUE)[[1]]
  if (length(parts) != 2) {
    cli::cli_abort(
      "Invalid line reference: {.val {ref}}. Expected format: 'line_number:hash'."
    )
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
    cli::cli_abort(
      "Invalid line field: {.val {line_str}}. Expected 'N:hash' or 'N:hash,M:hash'."
    )
  }
}

validate_one_hash <- function(
  line_num,
  provided_hash,
  file_lines,
  label,
  edit_index
) {
  n_lines <- length(file_lines)
  if (line_num > n_lines) {
    return(sprintf(
      "Edit %d: %s %d does not exist (file has %d lines).",
      edit_index,
      label,
      line_num,
      n_lines
    ))
  }
  expected <- hashline(file_lines[line_num])
  if (expected != provided_hash) {
    return(sprintf(
      "Edit %d: hash mismatch on %s %d. File may have changed since last read.",
      edit_index,
      label,
      line_num
    ))
  }
  NULL
}

validate_edit_hashes <- function(edits_parsed, file_lines) {
  mismatches <- character()

  for (i in seq_along(edits_parsed)) {
    edit <- edits_parsed[[i]]

    # Validate start line hash (skip for line 0 / insert at top)
    if (edit$start$line > 0) {
      msg <- validate_one_hash(
        edit$start$line,
        edit$start$hash,
        file_lines,
        "line",
        i
      )
      if (!is.null(msg)) mismatches <- c(mismatches, msg)
    }

    # Validate end line hash (for replace_range)
    if (edit$action == "replace_range" && edit$end$line != edit$start$line) {
      msg <- validate_one_hash(
        edit$end$line,
        edit$end$hash,
        file_lines,
        "end line",
        i
      )
      if (!is.null(msg)) mismatches <- c(mismatches, msg)
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
  # Only check replace/replace_range ops (inserts don't occupy ranges)
  replace_edits <- Filter(function(e) e$action != "insert_after", edits_parsed)
  if (length(replace_edits) < 2) {
    return(invisible())
  }

  ranges <- lapply(replace_edits, function(e) c(e$start$line, e$end$line))

  for (i in seq_len(length(ranges) - 1)) {
    for (j in seq(i + 1, length(ranges))) {
      ri <- ranges[[i]]
      rj <- ranges[[j]]
      if (ri[1] <= rj[2] && rj[1] <= ri[2]) {
        cli::cli_abort(
          "Edits {i} and {j} have overlapping line ranges ({ri[1]}-{ri[2]} and {rj[1]}-{rj[2]})."
        )
      }
    }
  }
}

sort_edits_ascending <- function(edits_parsed) {
  start_lines <- vapply(edits_parsed, function(e) e$start$line, numeric(1))
  edits_parsed[order(start_lines)]
}

sort_edits_descending <- function(edits_parsed) {
  # Primary: start line descending. Secondary: replace/replace_range before

  # insert_after (so a replace on the same line is applied before an insert).
  order_keys <- vapply(
    edits_parsed,
    function(e) {
      priority <- if (e$action == "insert_after") 0 else 1
      e$start$line * 10 + priority
    },
    numeric(1)
  )
  edits_parsed[order(order_keys, decreasing = TRUE)]
}

splice_lines <- function(file_lines, start, end, replacement) {
  before <- if (start > 1) file_lines[seq_len(start - 1)] else character()
  after <- if (end < length(file_lines)) {
    file_lines[seq(end + 1, length(file_lines))]
  } else {
    character()
  }
  c(before, replacement, after)
}

apply_edits <- function(file_lines, edits_parsed) {
  # Sort edits bottom-to-top so splicing doesn't shift later edit positions
  edits_parsed <- sort_edits_descending(edits_parsed)

  for (edit in edits_parsed) {
    n <- edit$start$line
    file_lines <- switch(
      edit$action,
      "replace" = splice_lines(file_lines, n, n, edit$content),
      "insert_after" = {
        if (n == 0) {
          c(edit$content, file_lines)
        } else {
          splice_lines(file_lines, n + 1, n, edit$content)
        }
      },
      "replace_range" = splice_lines(
        file_lines,
        n,
        edit$end$line,
        edit$content
      )
    )
  }

  file_lines
}

# Compute where each edit's content lands in the new file.
# Returns list(regions = list of region info, total_delta = integer).
compute_edit_regions <- function(edits_parsed) {
  if (length(edits_parsed) == 0) {
    return(list(regions = list(), total_delta = 0L))
  }

  # Sort by original start line ascending
  edits_sorted <- sort_edits_ascending(edits_parsed)

  cumulative_delta <- 0L
  regions <- vector("list", length(edits_sorted))

  for (i in seq_along(edits_sorted)) {
    edit <- edits_sorted[[i]]
    orig_start <- edit$start$line
    orig_end <- edit$end$line

    # How many original lines does this edit consume?
    orig_lines <- switch(
      edit$action,
      "replace" = 1L,
      "insert_after" = 0L,
      "replace_range" = orig_end - orig_start + 1L
    )

    new_lines_count <- length(edit$content)
    delta <- as.integer(new_lines_count - orig_lines)

    # Where does this edit's new content start in the new file?
    new_start <- if (edit$action == "insert_after") {
      if (orig_start == 0L) 1L else orig_start + cumulative_delta + 1L
    } else {
      orig_start + cumulative_delta
    }

    # Where does this edit's new content end?
    new_end <- if (new_lines_count > 0L) {
      new_start + new_lines_count - 1L
    } else {
      new_start - 1L # Zero-width region for deletions
    }

    # The old line number of the first unmodified line after this edit
    old_line_after <- if (edit$action == "insert_after") {
      orig_start + 1L
    } else {
      orig_end + 1L
    }

    cumulative_delta <- cumulative_delta + delta

    regions[[i]] <- list(
      new_start = new_start,
      new_end = new_end,
      delta = delta,
      cumulative_delta_after = cumulative_delta,
      old_line_after = old_line_after
    )
  }

  list(regions = regions, total_delta = cumulative_delta)
}

# Expand edit regions by context lines, then merge regions within
# gap_threshold lines of each other.
merge_edit_regions <- function(
  regions,
  n_new_lines,
  gap_threshold = 10L,
  context_lines = 1L
) {
  if (length(regions) == 0 || n_new_lines == 0L) {
    return(list())
  }

  # Expand each region by context lines
  expanded <- lapply(regions, function(r) {
    if (r$new_end >= r$new_start) {
      # Normal region with content
      ctx_start <- max(1L, r$new_start - context_lines)
      ctx_end <- min(n_new_lines, r$new_end + context_lines)
    } else {
      # Zero-width region (deletion): show context around the deletion point
      ctx_start <- max(1L, r$new_start - context_lines)
      ctx_end <- min(n_new_lines, r$new_start + context_lines - 1L)
    }

    list(
      start = ctx_start,
      end = ctx_end,
      cumulative_delta_after = r$cumulative_delta_after,
      old_line_after = r$old_line_after
    )
  })

  # Merge regions within gap_threshold
  merged <- list(expanded[[1]])

  for (i in seq_along(expanded)[-1]) {
    last <- merged[[length(merged)]]
    curr <- expanded[[i]]

    if (curr$start - last$end <= gap_threshold) {
      # Merge: extend end, update cumulative delta to the later region's
      merged[[length(merged)]]$end <- curr$end
      merged[[length(merged)]]$cumulative_delta_after <-
        curr$cumulative_delta_after
      merged[[length(merged)]]$old_line_after <- curr$old_line_after
    } else {
      merged <- c(merged, list(curr))
    }
  }

  merged
}

# Build the model-facing response string for an edit operation.
build_edit_response <- function(path, new_lines, old_line_count, edits_parsed) {
  n_new <- length(new_lines)
  n_old <- old_line_count
  total_delta <- n_new - n_old

  edit_result <- compute_edit_regions(edits_parsed)
  merged <- merge_edit_regions(edit_result$regions, n_new_lines = n_new)

  # Header
  if (total_delta == 0L) {
    header <- sprintf(
      "Applied %d edit(s) to %s (%d lines).",
      length(edits_parsed),
      path,
      n_new
    )
  } else {
    header <- sprintf(
      "Applied %d edit(s) to %s (now %d lines, previously %d).",
      length(edits_parsed),
      path,
      n_new,
      n_old
    )
  }

  parts <- header

  for (i in seq_along(merged)) {
    region <- merged[[i]]

    # Output hashlines for this region
    if (region$start <= region$end && region$end <= n_new) {
      region_lines <- new_lines[seq(region$start, region$end)]
      hashlines <- format_hashlines(region_lines, start_line = region$start)
      parts <- c(parts, "", hashlines)
    }

    # Shift hint (only when there's a non-zero shift and lines below)
    cum_delta <- region$cumulative_delta_after
    if (cum_delta != 0L && region$end < n_new) {
      old_example <- region$old_line_after
      new_example <- old_example + cum_delta

      offset_str <- sprintf("%+d", cum_delta)

      if (i < length(merged)) {
        hint <- sprintf(
          paste0(
            "Content between here and the next edit region was not ",
            "modified.\nCached hashes are still valid \u2014 update ",
            "line numbers by %s (old line %d \u2192 new line %d)."
          ),
          offset_str,
          old_example,
          new_example
        )
      } else {
        hint <- sprintf(
          paste0(
            "Content below line %d was not modified.\nCached hashes ",
            "are still valid \u2014 update line numbers by %s ",
            "(old line %d \u2192 new line %d)."
          ),
          region$end,
          offset_str,
          old_example,
          new_example
        )
      }
      parts <- c(parts, "", hint)
    }
  }

  paste(parts, collapse = "\n")
}
