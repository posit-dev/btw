#' @include tool-files-read.R
NULL

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

  BtwFileDiffToolResult(
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

# --- Shared result class for write/edit/replace tools ---

BtwFileDiffToolResult <- S7::new_class(
  "BtwFileDiffToolResult",
  parent = BtwToolResult
)

S7::method(contents_shinychat, BtwFileDiffToolResult) <- function(content) {
  res <- shinychat::contents_shinychat(
    S7::super(content, ellmer::ContentToolResult)
  )

  if (!is_installed("diffviewer")) {
    cli::cli_warn(
      "Install the {.pkg diffviewer} package for rich file diffs in {.fn btw::btw_app}: {.run install.packages('diffviewer')}",
      .frequency = "once",
      .frequency_id = "btw-tool-files-diffviewer"
    )
    return(res)
  }

  new <- content@extra$content
  old <- content@extra$previous_content

  dir <- withr::local_tempdir()
  path_ext <- fs::path_ext(content@extra$path)
  path_file <- fs::path_ext_remove(fs::path_file(content@extra$path))

  path_old <- fs::path(dir, path_file, ext = path_ext)
  path_new <- fs::path(dir, sprintf("%s.new", path_file), ext = path_ext)

  write_file(old %||% "", path_old)
  write_file(new %||% "", path_new)

  res$value <- diffviewer::visual_diff(path_old, path_new)
  res$value_type <- "html"
  res$class <- "btw-tool-result-file-diff"
  res
}
