#' Tool: Code Search in Project
#'
#' Search through code files in the project directory for specific terms.
#'
#' ## Options
#'
#' You can configure which file extensions are included and which paths are
#' excluded from code search by using two options:
#'
#' * `btw.files_code_search.extensions`: A character vector of file extensions
#'   to search in (default includes R, Python, JavaScript, TypeScript, Markdown,
#'   SCSS, and CSS files).
#' * `btw.files_code_search.exclusions`: A character vector of gitignore-style
#'   patterns to exclude paths and directories from the search. The default
#'   value includes a set of common version control, IDE, and cache folders.
#'
#' Alternatively, you can also set these options in your `btw.md` file under the
#' `options` section, like this:
#'
#' ```markdown
#' ---
#' client:
#'   provider: anthropic
#' tools: [files_code_search]
#' options:
#'   files_code_search:
#'     extensions: ["R", "Rmd", "py", "qmd"]
#'     exclusions: ["DEFAULT", ".quarto/"]
#' ---
#' ```
#'
#' Include `"DEFAULT"` in the `exclusions` option to use btw's default
#' exclusions, which cover common directories like `.git/`, `.vscode/`.
#'
#' If the \pkg{gert} package is installed and the project is a Git repository,
#' the tool will also respect the `.gitignore` file and exclude any ignored
#' paths, regardless of the `btw.files_code_search.exclusions` option.
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' withr::with_tempdir({
#'   writeLines(state.name[1:25], "state_names_1.md")
#'   writeLines(state.name[26:50], "state_names_2.md")
#'
#'   tools <- btw_tools("files_code_search")
#'   tools$btw_tool_files_code_search(
#'     term = "kentucky",
#'     case_sensitive = FALSE,
#'     show_lines = TRUE
#'   )
#' })
#'
#' @param term The term to search for in the code files.
#' @param limit Maximum number of matching lines to return (between 1 and 1000,
#'   default 100).
#' @param case_sensitive Whether the search should be case-sensitive (default is
#'   `FALSE`).
#' @param use_regex Whether to interpret the search term as a regular expression
#'   (default is `FALSE`).
#' @param show_lines Whether to show the matching lines in the results. Defaults
#'   to `FALSE`, which means only the file names and count of matching lines
#'   are returned.
#' @inheritParams btw_tool_docs_package_news
#'
#' @return Returns a tool result with a data frame of search results, with
#'   columns for `filename`, `size`, `last_modified`, `content` and `line`.
#'
#' @family files tools
#' @export
btw_tool_files_code_search <- function(
  term,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE,
  show_lines = FALSE,
  .intent = ""
) {}


btw_tool_files_code_search_factory <- function(
  path = getwd(),
  extensions = files_code_search_extensions(),
  exclusions = files_code_search_exclusions()
) {
  check_path_exists(path)
  check_path_within_current_wd(path)
  path <- fs::path_rel(path)
  check_character(extensions, allow_na = FALSE)
  check_character(exclusions, allow_na = FALSE, allow_null = TRUE)

  .db_create_local_file_index <- function() {
    if (identical(Sys.getenv("TESTTHAT"), "true")) {
      # In testthat, we don't want to create a DuckDB database
      return(NULL)
    }
    rlang::check_installed("DBI")
    rlang::check_installed("duckdb")

    withr::local_options(cli.progress_handlers_only = "cli")
    cli::cli_progress_step(
      "Indexing files in {.path {fs::path_real(path)}} for code search"
    )
    db_create_local_files(path, extensions, exclusions)
  }

  function(
    term,
    limit = 100,
    case_sensitive = TRUE,
    use_regex = FALSE,
    show_lines = FALSE
  ) {
    check_string(term, allow_empty = FALSE)
    check_number_whole(limit, min = 1, max = 1000)
    check_bool(case_sensitive)
    check_bool(use_regex)

    compare_fn <- if (use_regex) "regexp_matches" else "contains"
    haystack <- if (case_sensitive) "content" else "lower(content)"
    needle <- if (case_sensitive) term else tolower(term)

    if (show_lines) {
      # truncate the content to 100 characters to avoid overly large results
      query_select <- "filename, size, last_modified, SUBSTR(content, 1, 100) AS content, line"
      query_group_by <- ""
      query_order_by <- "ORDER BY last_modified DESC, filename ASC, line ASC"
    } else {
      # size and last_modified are not in the GROUP BY clause
      query_select <- "filename, MAX(size) AS size, MAX(last_modified) AS last_modified, COUNT(*) AS n_matching_lines"
      query_group_by <- "GROUP BY filename"
      query_order_by <- "ORDER BY n_matching_lines DESC, last_modified DESC"
    }

    con <- .db_create_local_file_index()

    query <- sprintf(
      "SELECT %s FROM code_file_lines WHERE %s(%s, ?) %s %s LIMIT ?",
      query_select,
      compare_fn,
      haystack,
      query_group_by,
      query_order_by
    )

    max_display <- 20L

    res <- DBI::dbGetQuery(con, query, params = list(needle, limit))
    res$size <- fs::as_fs_bytes(res$size)

    BtwToolResult(
      res,
      extra = list(
        display = list(
          markdown = paste0(
            md_table(res[1:min(nrow(res), max_display), ]),
            if (nrow(res) > max_display) {
              paste0("\n\n... and ", nrow(res) - max_display, " more matches.")
            }
          )
        )
      )
    )
  }
}

.btw_add_to_tools(
  name = "btw_tool_files_code_search",
  group = "files",
  tool = function() {
    project_code_search <- btw_tool_files_code_search_factory()
    ellmer::tool(
      function(
        term,
        limit = 100,
        case_sensitive = TRUE,
        use_regex = FALSE,
        show_lines = FALSE
      ) {
        project_code_search(
          term,
          limit = limit,
          case_sensitive = case_sensitive,
          use_regex = use_regex,
          show_lines = show_lines
        )
      },
      name = "btw_tool_files_code_search",
      description = r"---(Search code files in the project.

Use this tool to find references to specific code or terms in the project.
The tool returns a list of files and lines of code that match the search `term`.
`term` is the only required argument, only adjust the arguments if necessary.

Use the `btw_tool_files_read_text_file` tool, if available, to read the full content of a file found in this search.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Code Search",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() {
          is_installed("duckdb") && is_installed("DBI")
        }
      ),
      arguments = list(
        term = ellmer::type_string(
          description = "The term to search for in the code files.",
          required = TRUE
        ),
        limit = ellmer::type_integer(
          description = "Maximum number of results to return (default is 100, max is 1000).",
          required = FALSE
        ),
        case_sensitive = ellmer::type_boolean(
          description = "Whether the search should be case-sensitive (default is true).",
          required = TRUE
        ),
        use_regex = ellmer::type_boolean(
          description = "Whether to interpret the search term as a regular expression (default is false).",
          required = FALSE
        ),
        show_lines = ellmer::type_boolean(
          description = "Whether to show the matching lines in the results (default is false).",
          required = FALSE
        )
      )
    )
  }
)

db_create_local_files <- function(
  path = getwd(),
  extensions = files_code_search_extensions(),
  exclusions = files_code_search_exclusions()
) {
  check_path_within_current_wd(path)
  check_character(extensions, allow_na = FALSE)
  check_character(exclusions, allow_na = FALSE, allow_null = TRUE)

  # Validate extensions contain only letters, numbers, underscore, or dash
  # and no regex-special characters. Throw if invalid.
  bad_ext <- !grepl("^[[:alnum:]_-]+$", extensions)
  if (any(bad_ext)) {
    cli::cli_abort(c(
      "Invalid file extensions: {.val {extensions[bad_ext]}}",
      "i" = "Only alphanumeric characters, underscores, and dashes are allowed."
    ))
  }

  ext_regex <- sprintf("[.](%s)$", paste(extensions, collapse = "|"))

  # Enumerate files with regex filter
  all_files <- fs::dir_ls(
    path,
    recurse = TRUE,
    type = "file",
    regexp = ext_regex,
    fail = FALSE
  )

  all_files <- filter_paths_with_gitignore(all_files, exclusions)

  git_repo <- path_find_in_project(".git", path)
  if (!is.null(git_repo) && is_installed("gert")) {
    discard <- map_lgl(
      all_files,
      gert::git_ignore_path_is_ignored,
      repo = git_repo
    )
    all_files <- all_files[!discard]
  }

  if (length(all_files) == 0) {
    cli::cli_warn(c(
      "No code files found in {.path {path}} with extensions {.val {extensions}}.",
      "i" = "Consider adjusting the search path or file extensions."
    ))
  }

  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "INSTALL fts")
  DBI::dbExecute(con, "LOAD fts")

  # Create the `code_files` table
  query <- sprintf(
    "CREATE TABLE code_files AS SELECT filename, size, last_modified, content FROM read_text([%s]);",
    paste(sprintf("'%s'", all_files), collapse = ", ")
  )

  DBI::dbExecute(con, query)

  # Create the `code_file_lines` table
  DBI::dbExecute(
    con,
    "
CREATE TABLE code_file_lines AS
SELECT
  code_files.filename,
  size,
  last_modified,
  unnest(lines) as content,
  generate_subscripts(lines, 1) as line
FROM
  code_files
JOIN (
    SELECT
      filename,
      STRING_SPLIT(REGEXP_REPLACE(content, '\r\n|\r', '\n'), '\n') as lines
    FROM code_files
  ) as code_lines
ON code_files.filename = code_lines.filename;"
  )

  invisible(con)
}

files_code_search_extensions <- function() {
  getOption(
    "btw.files_code_search.extensions",
    # fmt: skip
    c("R", "Rmd", "qmd", "py", "js", "ts", "md", "scss", "css")
  )
}

files_code_search_exclusions <- function() {
  # fmt: skip
  default <- c(
    # VCS / IDE / cache
    ".git/", ".github/", ".gitlab/", ".vscode/", ".idea/", ".cache/", ".DS_Store/",
    # JS/TS
    "node_modules/", "dist/", ".next/", ".nuxt/", ".pnpm-store/",
    # Python
    "venv/", ".venv/", "__pycache__/", ".pytest_cache/", ".mypy_cache/", ".ruff_cache/",
    # R
    "renv/", ".Rproj.user/", ".Rcheck/",
    # Other site/artifacts
    ".sass-cache/"
  )

  res <- getOption("btw.files_code_search.exclusions", default)

  if ("DEFAULT" %in% res) {
    idx <- which(res == "DEFAULT")
    res <- c(res[seq_len(idx - 1)], default, res[-seq_len(idx)])
  }

  res
}
