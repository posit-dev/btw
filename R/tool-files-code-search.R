#' Tool: Code Search in Project
#'
#' Search through code files in the project directory for specific terms.
#'
#' @param term The term to search for in the code files.
#' @param limit Maximum number of matching lines to return (between 1 and 1000,
#'   default 100).
#' @param case_sensitive Whether the search should be case-sensitive (default is
#'   `FALSE`).
#' @param use_regex Whether to interpret the search term as a regular expression
#'   (default is `FALSE`).
#'
#' @return Returns a tool result with a data frame of search results, with
#'   columns for `filename`, `size`, `last_modified`, `content` and `line`.
#'
#' @family Tools
#' @export
btw_tool_files_code_search <- function(
  term,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE,
  .intent = ""
) {}


btw_tool_files_search_factory <- function(
  path = getwd(),
  extensions = c("R", "Rmd", "qmd", "py", "js", "ts", "md", "scss", "css")
) {
  rlang::check_installed("DBI")
  rlang::check_installed("duckdb")

  check_path_within_current_wd(path)
  check_character(extensions, allow_na = FALSE)

  env <- rlang::current_env()
  con <- NULL

  delayedAssign("con", assign.env = env, {
    cli::cli_progress_step(
      "Creating DuckDB database for code search of {.path {path}}"
    )
    con <- db_create_local_files(path, extensions)
    cli::cli_process_done()
    con
  })

  function(term, limit = 100, case_sensitive = TRUE, use_regex = FALSE) {
    check_string(term, allow_empty = FALSE)
    check_number_whole(limit, min = 1, max = 1000)
    check_bool(case_sensitive)
    check_bool(use_regex)

    compare_fn <- if (use_regex) "regexp_matches" else "contains"
    haystack <- if (case_sensitive) "content" else "lower(content)"
    needle <- if (case_sensitive) term else tolower(term)

    query_search <- sprintf(
      "SELECT * FROM code_file_lines WHERE %s(%s, ?)",
      compare_fn,
      haystack
    )
    query <- paste(
      query_search,
      "ORDER BY last_modified DESC, filename ASC, line ASC",
      "LIMIT ?"
    )

    BtwToolResult(
      DBI::dbGetQuery(con, query, params = list(needle, limit)),
    )
  }
}

.btw_add_to_tools(
  name = "btw_tool_files_code_search",
  group = "files",
  tool = function() {
    project_code_search <- btw_tool_files_search_factory()
    ellmer::tool(
      project_code_search,
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
          description = "Whether the search should be case-sensitive (default is FALSE).",
          required = FALSE
        ),
        use_regex = ellmer::type_boolean(
          description = "Whether to interpret the search term as a regular expression (default is FALSE).",
          required = FALSE
        )
      )
    )
  }
)

db_create_local_files <- function(
  path = getwd(),
  extensions = c("R", "Rmd", "qmd", "py", "js", "ts", "md", "scss", "css")
) {
  check_path_within_current_wd(path)
  check_character(extensions, allow_na = FALSE)

  con <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con, "INSTALL fts")
  DBI::dbExecute(con, "LOAD fts")

  root_glob <- sprintf("'*.%s'", extensions)
  any_glob <- sprintf("'**/*.%s'", extensions)

  # https://duckdb.org/docs/stable/guides/file_formats/read_file.html
  sql <- sprintf(
    "CREATE TABLE code_files AS SELECT '%s/' || filename AS filename, size, last_modified, content FROM read_text([%s]);",
    getwd(),
    paste(c(root_glob, any_glob), collapse = ", ")
  )

  DBI::dbExecute(con, sql)

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

  con
}
