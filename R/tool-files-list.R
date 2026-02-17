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
