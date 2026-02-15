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
      description = 'Replace exact string occurrences in a text file.

WHEN TO USE:
Use this tool to find and replace exact strings in a file. This is ideal for
targeted text changes when you know the exact content to replace. For
line-based edits using hashline references, use btw_tool_files_edit instead.

IMPORTANT:
- The `old_string` must match EXACTLY, including whitespace and indentation.
- By default, `old_string` must appear exactly once in the file. Provide
  enough surrounding context to make the match unique.
- Set `replace_all` to true to replace all occurrences.
- `old_string` and `new_string` must be different.
- Use an empty `new_string` ("") to delete the matched text.
',
      annotations = ellmer::tool_annotations(
        title = "Replace in File",
        read_only_hint = FALSE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() TRUE
      ),
      arguments = list(
        path = ellmer::type_string(
          "The relative path to the file to edit."
        ),
        old_string = ellmer::type_string(
          "The exact string to find in the file. Must be unique unless replace_all is true."
        ),
        new_string = ellmer::type_string(
          "The replacement string. Use an empty string to delete the matched text."
        ),
        replace_all = ellmer::type_boolean(
          "If true, replace all occurrences of old_string. Default is false, which requires exactly one match.",
          required = FALSE
        )
      )
    )
  }
)
