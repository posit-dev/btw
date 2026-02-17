#' Tool: Replace exact strings in a text file
#'
#' @param path Path to the file to edit. The `path` must be in the current
#'   working directory.
#' @param old_string The exact string to find in the file. Must be unique unless
#'   `replace_all` is `TRUE`.
#' @param new_string The replacement string. Must differ from `old_string`.
#' @param replace_all If `TRUE`, replace all occurrences of `old_string`.
#'   Defaults to `FALSE`, which requires exactly one occurrence.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a message confirming the replacement was applied.
#'
#' @family files tools
#' @export
btw_tool_files_replace <- function(
  path,
  old_string,
  new_string,
  replace_all,
  `_intent`
) {}

btw_tool_files_replace_impl <- function(
  path,
  old_string,
  new_string,
  replace_all = FALSE
) {
  check_string(path)
  check_string(old_string, allow_empty = FALSE)
  check_string(new_string)
  check_bool(replace_all)
  check_path_within_current_wd(path)

  if (!fs::is_file(path) || !fs::file_exists(path)) {
    cli::cli_abort(
      "Path {.path {path}} is not a file or does not exist."
    )
  }

  if (old_string == new_string) {
    cli::cli_abort(
      "{.arg old_string} and {.arg new_string} must be different."
    )
  }

  previous_content <- read_file(path)

  match_positions <- gregexpr(old_string, previous_content, fixed = TRUE)[[1]]
  n_matches <- if (match_positions[1] == -1L) 0L else length(match_positions)

  if (n_matches == 0L) {
    cli::cli_abort(c(
      "{.arg old_string} was not found in {.path {path}}.",
      "i" = "Make sure the string matches exactly, including whitespace and indentation."
    ))
  }

  if (!replace_all && n_matches > 1) {
    cli::cli_abort(c(
      "{.arg old_string} appears {n_matches} times in {.path {path}}.",
      "i" = "Provide more surrounding context in {.arg old_string} to make it unique, or set {.arg replace_all} to {.code TRUE}."
    ))
  }

  new_content <- gsub(old_string, new_string, previous_content, fixed = TRUE)
  write_file(new_content, path)

  n_replaced <- if (replace_all) n_matches else 1L
  msg <- sprintf(
    "Replaced %d occurrence%s in %s.",
    n_replaced,
    if (n_replaced != 1) "s" else "",
    path
  )

  BtwFileDiffToolResult(
    msg,
    extra = list(
      path = path,
      content = new_content,
      previous_content = previous_content,
      display = list(
        markdown = md_code_block(fs::path_ext(path), new_content),
        title = HTML(title_with_open_file_button("Replace", path)),
        show_request = FALSE,
        icon = tool_icon("file-save")
      )
    )
  )
}

.btw_add_to_tools(
  name = "btw_tool_files_replace",
  group = "files",
  tool = function() {
    ellmer::tool(
      btw_tool_files_replace_impl,
      name = "btw_tool_files_replace",
      description = r"---(Find and replace exact string occurrences in a text file.

WHEN TO USE:
Use this tool for simple, exact text replacements when you know the precise string to
change. Ideal for renaming variables, updating values, or making repetitive changes.
For line-based structural edits, use btw_tool_files_edit instead.

HOW IT WORKS:
1. Searches for `old_string` as an exact literal match (not a regex).
2. By default, requires exactly one match to prevent unintended changes.
3. Use `replace_all: true` to replace all occurrences when intentional.

TIPS FOR SUCCESS:
- Include enough surrounding context in `old_string` to make it unique.
- Whitespace and indentation must match exactly.
- To delete text, use an empty string for `new_string`.
- If the match is ambiguous, add more context rather than using replace_all.
    )---",
      annotations = ellmer::tool_annotations(
        title = "Replace in File",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        path = ellmer::type_string(
          "Relative path to the file to edit. Must be within the current working directory."
        ),
        old_string = ellmer::type_string(
          "The exact string to find. Must match character-for-character, including whitespace. Must be unique in the file unless replace_all is true."
        ),
        new_string = ellmer::type_string(
          "The replacement text. Use an empty string (\"\") to delete the matched text."
        ),
        replace_all = ellmer::type_boolean(
          "Replace all occurrences instead of requiring exactly one match. Defaults to false.",
          required = FALSE
        )
      )
    )
  }
)
