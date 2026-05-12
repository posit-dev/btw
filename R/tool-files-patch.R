#' Tool: Apply a patch to files
#'
#' @description
#' Applies a structured diff-like patch envelope to one or more files. Unlike
#' [btw_tool_files_edit()] (which requires hashline references from a prior
#' read) or [btw_tool_files_replace()] (which requires exact strings), the
#' patch tool uses context-matching hunks so models can produce edits without
#' first reading the file.
#'
#' A single patch envelope can add, update, delete, and move files atomically:
#' either all operations succeed or none are applied.
#'
#' ## Patch syntax
#'
#' A patch is a text envelope that begins with `*** Begin Patch` and ends with
#' `*** End Patch`. Inside the envelope, each operation starts with a header:
#'
#'     ** Begin Patch
#'     ** Add File: docs/example.md
#'     Hello
#'     World
#'     ** Update File: src/main.py
#'     @@@
#'     context line
#'     old line
#'     new line
#'     ** Delete File: old.txt
#'     ** Update File: src/old.ts
#'     ** Move to: src/new.ts
#'     @@@
#'     context line
#'     export const oldName = 1
#'     export const newName = 1
#'     ** End Patch
#'
#' ### Headers
#'
#' * `*** Add File: <path>` -- create a new file (must not exist).
#' * `*** Update File: <path>` -- modify an existing file.
#' * `*** Delete File: <path>` -- remove an existing file.
#' * `*** Move to: <path>` -- sub-header inside an `Update File` block; renames
#'   the file to `<path>` after applying any hunks. Destination must not exist.
#'
#' ### Hunk lines (inside `Update File`)
#'
#' * `@@` -- hunk boundary; any trailing text on this line is informational and
#'   ignored.
#' * ` <text>` (space prefix) -- context line that must match the file exactly.
#' * `-<text>` -- line to delete; must match the file exactly at this position.
#' * `+<text>` -- line to insert.
#'
#' Every hunk must include at least one context or delete line to anchor the
#' edit; pure-insert hunks are rejected. Use `*** Add File` for new files.
#'
#' ### Add File body
#'
#' All body lines under `*** Add File` must start with `+`; the file content is
#' the text after each `+`.
#'
#' @param patch The full patch text in the wire format described below. Must
#'   begin with `*** Begin Patch` and end with `*** End Patch`.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a summary of the operations applied.
#'
#' @seealso [btw_tool_files_edit()] for hashline-based targeted edits,
#'   [btw_tool_files_replace()] for exact find-and-replace edits.
#'
#' @family files tools
#' @export
btw_tool_files_patch <- function(patch, `_intent`) {}

btw_tool_files_patch_impl <- function(patch) {
  ops <- parse_patch(patch)
  validate_patch_ops(ops)
  results <- apply_patch_ops(ops)

  n <- length(ops)
  lines <- vapply(
    ops,
    function(op) {
      switch(
        op$op,
        "add" = paste0("  - Added: ", op$path),
        "delete" = paste0("  - Deleted: ", op$path),
        "update" = if (!is.null(op$move_to)) {
          paste0("  - Moved: ", op$path, " -> ", op$move_to)
        } else {
          paste0("  - Updated: ", op$path)
        }
      )
    },
    character(1)
  )

  value <- paste(
    c(
      sprintf("Applied patch: %d operation%s.", n, if (n != 1) "s" else ""),
      lines
    ),
    collapse = "\n"
  )

  display_md <- paste(
    c(
      "**Patch**",
      md_code_block("diff", patch),
      "",
      "**Results**",
      value
    ),
    collapse = "\n"
  )

  # Use BtwToolResult rather than BtwFileDiffToolResult: a patch can touch
  # multiple files, so the single-file diff viewer shape doesn't apply cleanly.
  btw_tool_result(
    value,
    display = list(
      markdown = display_md,
      show_request = FALSE,
      icon = tool_icon("file-save")
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_files_patch",
  group = "files",
  tool = function() {
    ellmer::tool(
      btw_tool_files_patch_impl,
      name = "btw_tool_files_patch",
      description = r"---(Apply a patch envelope that adds, updates, deletes, or moves files atomically.

WHEN TO USE:
Use this tool when you want to make structured edits without reading the file first,
or when a single operation must touch multiple files atomically. Context lines in hunks
anchor the edit location so no prior hashline read is needed.

WIRE FORMAT:
  *** Begin Patch
  *** Add File: docs/example.md
  +Hello
  +World
  *** Update File: src/main.py
  @@
  -print("Hi")
  +print("Hello")
  *** Delete File: old.txt
  *** Update File: src/old.ts
  *** Move to: src/new.ts
  @@
  -export const oldName = 1
  +export const newName = 1
  *** End Patch

HEADERS:
- "*** Begin Patch" / "*** End Patch" -- required envelope
- "*** Add File: <path>"    -- create a new file (must not exist)
- "*** Update File: <path>" -- modify an existing file (must exist)
- "*** Delete File: <path>" -- remove a file (must exist)
- "*** Move to: <path>"     -- sub-header inside Update File; rename destination

HUNK LINES (inside Update File):
- "@@"         -- hunk boundary (trailing text is informational, ignored)
- " <text>"    -- context line: must match the file exactly (space prefix)
- "-<text>"    -- delete this line
- "+<text>"    -- insert this line

ADD FILE BODY:
Every body line must start with "+"; content follows the "+".

ATOMICITY:
All operations are validated (hunk matching, path checks, filesystem preconditions)
before any file is written. If any operation fails, no changes are applied.

NOTES:
- Context and delete lines must match the file exactly (case-sensitive, whitespace-significant).
- Each element of `content` in hunks is one line; do not include trailing newlines in the patch.
- Paths must be relative to the current working directory.
    )---",
      annotations = ellmer::tool_annotations(
        title = "Patch Files",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      convert = FALSE,
      arguments = list(
        patch = ellmer::type_string(
          "The full patch envelope as a single string. Must start with '*** Begin Patch' and end with '*** End Patch'. Lines are separated by newlines."
        )
      )
    )
  }
)

# --- Patch tool helpers ---

# parse_patch: parse a patch string into a list of operation objects.
# Each op is: list(op, path, move_to, hunks, lines) as described in the spec.
# Hunk ops use a flat tagged-line list: list(list(type=..., line=...)) --
# this makes both matching and applying straightforward without separate decomposition.
parse_patch <- function(patch) {
  raw_lines <- strsplit(patch, "\n", fixed = TRUE)[[1]]

  state <- "outside"
  ops <- list()
  cur_op <- NULL
  cur_hunk <- NULL
  i <- 0L

  finalize_hunk <- function() {
    if (!is.null(cur_hunk)) {
      has_anchor <- any(vapply(
        cur_hunk$ops,
        function(o) o$type %in% c("context", "delete"),
        logical(1)
      ))
      if (!has_anchor) {
        cli::cli_abort(c(
          "Line {cur_hunk$start_line}: hunk has no context or delete lines to anchor the insertion.",
          i = "Add at least one context line (prefix ' ') or use '*** Add File' for new files."
        ))
      }
      cur_op$hunks[[length(cur_op$hunks) + 1L]] <<- cur_hunk
      cur_hunk <<- NULL
    }
  }

  finalize_op <- function() {
    if (!is.null(cur_op)) {
      finalize_hunk()
      ops[[length(ops) + 1L]] <<- cur_op
      cur_op <<- NULL
    }
  }

  for (raw_line in raw_lines) {
    i <- i + 1L

    if (state == "outside") {
      if (raw_line == "*** Begin Patch") {
        state <- "inside"
      } else if (startsWith(raw_line, "***")) {
        cli::cli_abort(
          "Line {i}: expected '*** Begin Patch' but got {.val {raw_line}}."
        )
      }
      next
    }

    if (state == "done") {
      next
    }

    # Inside envelope
    if (raw_line == "*** End Patch") {
      finalize_op()
      state <- "done"
      next
    }

    if (startsWith(raw_line, "*** Add File: ")) {
      finalize_op()
      path <- substring(raw_line, nchar("*** Add File: ") + 1L)
      cur_op <- list(op = "add", path = path, lines = character())
      state <- "add"
      next
    }

    if (startsWith(raw_line, "*** Update File: ")) {
      finalize_op()
      path <- substring(raw_line, nchar("*** Update File: ") + 1L)
      cur_op <- list(op = "update", path = path, move_to = NULL, hunks = list())
      state <- "update"
      next
    }

    if (startsWith(raw_line, "*** Delete File: ")) {
      finalize_op()
      path <- substring(raw_line, nchar("*** Delete File: ") + 1L)
      cur_op <- list(op = "delete", path = path)
      state <- "delete"
      next
    }

    if (startsWith(raw_line, "*** Move to: ")) {
      if (state != "update") {
        cli::cli_abort(
          "Line {i}: '*** Move to:' is only valid inside an Update File block."
        )
      }
      cur_op$move_to <- substring(raw_line, nchar("*** Move to: ") + 1L)
      next
    }

    if (startsWith(raw_line, "***")) {
      cli::cli_abort(
        "Line {i}: unknown patch header {.val {raw_line}}."
      )
    }

    if (state == "add") {
      if (!startsWith(raw_line, "+")) {
        cli::cli_abort(
          "Line {i}: Add File body lines must start with '+', got {.val {raw_line}}."
        )
      }
      cur_op$lines <- c(cur_op$lines, substring(raw_line, 2L))
      next
    }

    if (state == "delete") {
      cli::cli_abort(
        "Line {i}: unexpected content inside Delete File block: {.val {raw_line}}."
      )
    }

    if (state == "update") {
      if (startsWith(raw_line, "@@")) {
        finalize_hunk()
        cur_hunk <- list(ops = list(), start_line = i)
        next
      }

      if (is.null(cur_hunk)) {
        cli::cli_abort(
          "Line {i}: hunk line outside of '@@' boundary: {.val {raw_line}}."
        )
      }

      if (startsWith(raw_line, " ")) {
        cur_hunk$ops[[length(cur_hunk$ops) + 1L]] <-
          list(type = "context", line = substring(raw_line, 2L))
      } else if (startsWith(raw_line, "-")) {
        cur_hunk$ops[[length(cur_hunk$ops) + 1L]] <-
          list(type = "delete", line = substring(raw_line, 2L))
      } else if (startsWith(raw_line, "+")) {
        cur_hunk$ops[[length(cur_hunk$ops) + 1L]] <-
          list(type = "insert", line = substring(raw_line, 2L))
      } else {
        cli::cli_abort(
          "Line {i}: invalid hunk line (expected ' ', '-', or '+'): {.val {raw_line}}."
        )
      }
      next
    }

    # Should only reach here if state == "inside" with no active op
    if (state == "inside") {
      cli::cli_abort(
        "Line {i}: content outside any operation block: {.val {raw_line}}."
      )
    }
  }

  if (state == "outside") {
    cli::cli_abort("Patch is missing '*** Begin Patch'.")
  }
  if (state != "done") {
    cli::cli_abort("Patch is missing '*** End Patch'.")
  }

  ops
}

validate_patch_ops <- function(ops) {
  all_paths <- character()

  for (i in seq_along(ops)) {
    op <- ops[[i]]

    if (!nzchar(op$path)) {
      cli::cli_abort("Operation {i}: path must not be empty.")
    }
    check_path_within_current_wd(op$path)

    if (!is.null(op$move_to)) {
      if (!nzchar(op$move_to)) {
        cli::cli_abort("Operation {i}: move_to path must not be empty.")
      }
      check_path_within_current_wd(op$move_to)
    }

    switch(
      op$op,
      "add" = {
        if (fs::file_exists(op$path)) {
          cli::cli_abort(
            "Add File: {.path {op$path}} already exists. Use Update File to modify it."
          )
        }
      },
      "update" = {
        if (!fs::file_exists(op$path)) {
          cli::cli_abort(
            "Update File: {.path {op$path}} does not exist."
          )
        }
        if (length(op$hunks) == 0L) {
          cli::cli_abort(
            "Update File: {.path {op$path}} has no hunks."
          )
        }
        if (!is.null(op$move_to) && fs::file_exists(op$move_to)) {
          cli::cli_abort(
            "Move to: {.path {op$move_to}} already exists."
          )
        }
      },
      "delete" = {
        if (!fs::file_exists(op$path)) {
          cli::cli_abort(
            "Delete File: {.path {op$path}} does not exist."
          )
        }
      }
    )

    all_paths <- c(all_paths, op$path)
  }
}

# match_hunk: find where a hunk's context+delete lines appear in file_lines.
# Returns list(start = N, end = M) -- 1-based range covered by context+delete.
match_hunk <- function(hunk, file_lines, search_start, path) {
  match_seq <- Filter(
    function(x) x$type %in% c("context", "delete"),
    hunk$ops
  )

  seq_len_val <- length(match_seq)
  n_file <- length(file_lines)
  last_start <- n_file - seq_len_val + 1L

  if (search_start <= last_start) {
    for (start in seq(search_start, last_start)) {
      matched <- TRUE
      for (j in seq_len(seq_len_val)) {
        if (file_lines[start + j - 1L] != match_seq[[j]]$line) {
          matched <- FALSE
          break
        }
      }
      if (matched) {
        return(list(start = start, end = start + seq_len_val - 1L))
      }
    }
  }

  cli::cli_abort(c(
    "Hunk context not found in {.path {path}}.",
    "i" = "First context/delete line: {.val {match_seq[[1L]]$line}}"
  ))
}

# apply_hunk: given matched range and hunk ops, produce replacement lines.
# Drops delete lines, keeps context lines, inserts insert lines.
apply_hunk <- function(hunk) {
  out <- character()
  for (item in hunk$ops) {
    if (item$type == "context" || item$type == "insert") {
      out <- c(out, item$line)
    }
    # delete: skip
  }
  out
}

# apply_patch_ops: dry-run all ops, then commit atomically.
apply_patch_ops <- function(ops) {
  staged <- vector("list", length(ops))

  for (i in seq_along(ops)) {
    op <- ops[[i]]

    if (op$op == "add") {
      dir_path <- fs::path_dir(op$path)
      staged[[i]] <- list(
        type = "write",
        path = op$path,
        content = paste(op$lines, collapse = "\n"),
        dir = if (dir_path != ".") dir_path else NULL
      )
    } else if (op$op == "delete") {
      staged[[i]] <- list(type = "delete", path = op$path)
    } else if (op$op == "update") {
      file_lines <- read_lines(op$path)

      # Match all hunks top-down, enforcing monotonic search_start
      search_start <- 1L
      matches <- vector("list", length(op$hunks))
      for (j in seq_along(op$hunks)) {
        m <- match_hunk(op$hunks[[j]], file_lines, search_start, op$path)
        matches[[j]] <- m
        search_start <- m$end + 1L
      }

      # Apply hunks bottom-up to avoid line-shift issues
      new_lines <- file_lines
      for (j in rev(seq_along(op$hunks))) {
        replacement <- apply_hunk(op$hunks[[j]])
        m <- matches[[j]]
        new_lines <- splice_lines(new_lines, m$start, m$end, replacement)
      }

      new_content <- paste(new_lines, collapse = "\n")
      dest_path <- op$move_to %||% op$path

      staged[[i]] <- list(
        type = "update",
        path = op$path,
        dest_path = dest_path,
        content = new_content,
        move = !is.null(op$move_to)
      )
    }
  }

  # All dry-run succeeded -- commit to disk
  for (s in staged) {
    if (s$type == "write") {
      if (!is.null(s$dir)) {
        fs::dir_create(s$dir, recurse = TRUE)
      }
      write_file(s$content, s$path)
    } else if (s$type == "delete") {
      fs::file_delete(s$path)
    } else if (s$type == "update") {
      if (s$move) {
        dest_dir <- fs::path_dir(s$dest_path)
        if (dest_dir != ".") {
          fs::dir_create(dest_dir, recurse = TRUE)
        }
        write_file(s$content, s$dest_path)
        fs::file_delete(s$path)
      } else {
        write_file(s$content, s$path)
      }
    }
  }

  invisible(staged)
}
