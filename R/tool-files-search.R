#' Tool: Code Search in Project
#'
#' Search through code files in the project directory for specific terms.
#'
#' ## Options
#'
#' You can configure which file extensions are included and which paths are
#' excluded from code search by using two options:
#'
#' * `btw.files_search.extensions`: A character vector of file extensions
#'   to search in (default includes R, Python, JavaScript, TypeScript, Markdown,
#'   SCSS, and CSS files).
#' * `btw.files_search.exclusions`: A character vector of gitignore-style
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
#' tools: [files_search]
#' options:
#'   files_search:
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
#' paths, regardless of the `btw.files_search.exclusions` option.
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' withr::with_tempdir({
#'   writeLines(state.name[1:25], "state_names_1.md")
#'   writeLines(state.name[26:50], "state_names_2.md")
#'
#'   tools <- btw_tools("files_search")
#'   tools$btw_tool_files_search(
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
btw_tool_files_search <- function(
  term,
  limit = 100,
  case_sensitive = TRUE,
  use_regex = FALSE,
  show_lines = FALSE,
  `_intent` = ""
) {}


btw_tool_files_search_factory <- function(
  path = getwd(),
  extensions = files_search_extensions(),
  exclusions = files_search_exclusions(),
  restrict_to_wd = TRUE
) {
  check_path_exists(path)
  if (restrict_to_wd) {
    check_path_within_current_wd(path)
  }
  check_character(extensions, allow_na = FALSE)
  check_character(exclusions, allow_na = FALSE, allow_null = TRUE)

  rlang::check_installed("DBI")
  rlang::check_installed("RSQLite", version = "2.2.2")

  project_path <- fs::path_norm(fs::path_abs(fs::path_expand(path)))
  state <- new.env(parent = emptyenv())
  state$con <- NULL
  state$last_refresh <- NULL
  state$db_path <- if (restrict_to_wd) btw_db_path() else fs::file_temp(ext = "sqlite3")
  reg.finalizer(
    state,
    function(state) {
      if (!is.null(state$con) && DBI::dbIsValid(state$con)) {
        DBI::dbDisconnect(state$con)
      }
    },
    onexit = TRUE
  )

  if (!restrict_to_wd) {
    cleanup_envir <- parent.frame()
    withr::defer(
      {
        if (!is.null(state$con) && DBI::dbIsValid(state$con)) {
          DBI::dbDisconnect(state$con)
        }
        db_files <- c(
          state$db_path,
          paste0(state$db_path, "-wal"),
          paste0(state$db_path, "-shm")
        )
        fs::file_delete(db_files[fs::file_exists(db_files)])
      },
      envir = cleanup_envir
    )
  }

  get_con <- function() {
    if (is.null(state$con) || !DBI::dbIsValid(state$con)) {
      state$con <- btw_db_open(state$db_path, .envir = state)
    }
    state$con
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

    con <- get_con()
    table_name <- btw_db_search_table_name(project_path)
    table_identifier <- as.character(DBI::dbQuoteIdentifier(con, table_name))

    tryCatch(
      btw_search_refresh_index(
        con = con,
        project_path = project_path,
        table_name = table_name,
        path = project_path,
        extensions = extensions,
        exclusions = exclusions,
        restrict_to_wd = restrict_to_wd
      ),
      error = function(cnd) {
        if (!btw_search_is_busy(cnd)) {
          stop(cnd)
        }
        btw_search_warn_busy()
      }
    )
    state$last_refresh <- Sys.time()

    max_display <- 20L
    res <- if (!DBI::dbExistsTable(con, table_name)) {
      btw_search_empty_results(show_lines)
    } else if (use_regex) {
      btw_search_regex(
        con,
        table_identifier,
        project_path,
        term,
        limit,
        case_sensitive,
        show_lines
      )
    } else {
      btw_search_literal(
        con,
        table_identifier,
        project_path,
        term,
        limit,
        case_sensitive,
        show_lines
      )
    }
    res$size <- fs::as_fs_bytes(res$size)

    BtwToolResult(
      as_json_rowwise(res),
      extra = list(
        display = list(
          title = "Searched code",
          markdown = paste0(
            md_table(res[1:min(nrow(res), max_display), ]),
            if (nrow(res) > max_display) {
              paste0("\n\n... and ", nrow(res) - max_display, " more matches.")
            }
          ),
          full_screen = TRUE
        )
      )
    )
  }
}

.btw_add_to_tools(
  name = "btw_tool_files_search",
  group = "files",
  alias_name = "btw_tool_files_code_search",
  can_register = function() {
    is_installed("RSQLite", version = "2.2.2") && is_installed("DBI")
  },
  tool = function() {
    project_code_search <- btw_tool_files_search_factory()
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
      name = "btw_tool_files_search",
      description = r"---(Search code files in the project.

Use this tool to find references to specific code or terms in the project.
The tool returns a list of files and lines of code that match the search `term`.
`term` is the only required argument, only adjust the arguments if necessary.

Use the `btw_tool_files_read` tool, if available, to read the full content of a file found in this search.
      )---",
      annotations = ellmer::tool_annotations(
        title = "Searching code",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE,
        btw_can_register = function() {
          is_installed("RSQLite", version = "2.2.2") && is_installed("DBI")
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

btw_search_candidate_files <- function(
  path,
  extensions,
  exclusions,
  restrict_to_wd
) {
  bad_ext <- !grepl("^[[:alnum:]_-]+$", extensions)
  if (any(bad_ext)) {
    cli::cli_abort(c(
      "Invalid file extensions: {.val {extensions[bad_ext]}}",
      "i" = "Only alphanumeric characters, underscores, and dashes are allowed."
    ))
  }

  ext_regex <- sprintf("[.](%s)$", paste(extensions, collapse = "|"))
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

  if (length(all_files) == 0) {
    return(data.frame(
      path = character(),
      source_path = character(),
      mtime = numeric(),
      size = numeric()
    ))
  }

  info <- fs::file_info(all_files)
  output_paths <- if (restrict_to_wd) {
    fs::path_rel(all_files, start = getwd())
  } else {
    fs::path_norm(all_files)
  }
  data.frame(
    path = output_paths,
    source_path = fs::path_norm(all_files),
    mtime = as.numeric(info$modification_time),
    size = as.numeric(info$size),
    stringsAsFactors = FALSE
  )
}

btw_search_refresh_index <- function(
  con,
  project_path,
  table_name,
  path,
  extensions,
  exclusions,
  restrict_to_wd
) {
  btw_search_prune_stale_indexes(con)
  table_name <- btw_db_create_search_table(con, project_path)
  table_identifier <- as.character(DBI::dbQuoteIdentifier(con, table_name))
  candidates <- btw_search_candidate_files(
    path,
    extensions,
    exclusions,
    restrict_to_wd
  )
  ledger <- DBI::dbGetQuery(
    con,
    "SELECT path, mtime, size FROM files WHERE project_path = ?",
    params = list(project_path)
  )

  removed <- setdiff(ledger$path, candidates$path)
  changed <- candidates[!candidates$path %in% ledger$path, , drop = FALSE]
  shared <- candidates[candidates$path %in% ledger$path, , drop = FALSE]
  if (nrow(shared) > 0) {
    old <- ledger[match(shared$path, ledger$path), , drop = FALSE]
    changed <- rbind(
      changed,
      shared[shared$mtime != old$mtime | shared$size != old$size, , drop = FALSE]
    )
  }
  deleted_files <- length(removed) + sum(changed$path %in% ledger$path)

  if (length(removed) > 0) {
    btw_search_delete_files(con, table_identifier, project_path, removed)
  }
  if (nrow(changed) > 0) {
    chunks <- split(
      seq_len(nrow(changed)),
      ceiling(seq_len(nrow(changed)) / 500)
    )
    for (chunk in chunks) {
      btw_search_refresh_files(
        con,
        table_identifier,
        project_path,
        changed[chunk, , drop = FALSE]
      )
    }
  }

  # Avoid FTS5 segment maintenance for ordinary, small file changes.
  optimize_after_deletions <- 100L
  if (deleted_files > optimize_after_deletions) {
    btw_search_optimize(con, table_identifier)
  }

  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO projects (path, label, last_opened_at, search_indexed_at) ",
      "VALUES (?, ?, ?, ?) ",
      "ON CONFLICT(path) DO UPDATE SET ",
      "label = excluded.label, ",
      "last_opened_at = excluded.last_opened_at, ",
      "search_indexed_at = excluded.search_indexed_at"
    ),
    params = list(
      project_path,
      fs::path_file(project_path),
      format(Sys.time(), tz = "UTC", usetz = TRUE),
      format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )

  invisible(NULL)
}

btw_search_optimize <- function(con, table_identifier) {
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO ", table_identifier,
      "(", table_identifier, ") VALUES (?)"
    ),
    params = list("optimize")
  )
}

btw_search_delete_files <- function(con, table_identifier, project_path, paths) {
  DBI::dbWithTransaction(con, {
    for (path in paths) {
      DBI::dbExecute(
        con,
        paste0("DELETE FROM ", table_identifier, " WHERE path = ?"),
        params = list(path)
      )
      DBI::dbExecute(
        con,
        "DELETE FROM files WHERE project_path = ? AND path = ?",
        params = list(project_path, path)
      )
    }
  })
}

btw_search_refresh_files <- function(
  con,
  table_identifier,
  project_path,
  files
) {
  DBI::dbWithTransaction(con, {
    for (i in seq_len(nrow(files))) {
      path <- files$path[[i]]
      source_path <- files$source_path[[i]]
      lines <- tryCatch(brio::read_lines(source_path), error = function(cnd) NULL)

      DBI::dbExecute(
        con,
        paste0("DELETE FROM ", table_identifier, " WHERE path = ?"),
        params = list(path)
      )
      DBI::dbExecute(
        con,
        "DELETE FROM files WHERE project_path = ? AND path = ?",
        params = list(project_path, path)
      )
      if (is.null(lines) || !fs::file_exists(source_path)) {
        next
      }

      if (length(lines) > 0) {
        for (line in seq_along(lines)) {
          DBI::dbExecute(
            con,
            paste0(
              "INSERT INTO ", table_identifier,
              " (path, line, content) VALUES (?, ?, ?)"
            ),
            params = list(path, line, lines[[line]])
          )
        }
      }
      DBI::dbExecute(
        con,
        "INSERT INTO files (project_path, path, mtime, size) VALUES (?, ?, ?, ?)",
        params = list(project_path, path, files$mtime[[i]], files$size[[i]])
      )
    }
  })
}

btw_search_literal <- function(
  con,
  table_identifier,
  project_path,
  term,
  limit,
  case_sensitive,
  show_lines
) {
  if (nchar(term, type = "chars") < 3) {
    where <- if (case_sensitive) {
      "instr(search.content, ?) > 0"
    } else {
      "instr(lower(search.content), lower(?)) > 0"
    }
    params <- list(project_path, term)
  } else {
    where <- if (case_sensitive) {
      "search.content MATCH ? AND instr(search.content, ?) > 0"
    } else {
      "search.content MATCH ? AND instr(lower(search.content), lower(?)) > 0"
    }
    params <- list(project_path, btw_search_match_term(term), term)
  }
  lines <- btw_search_query(con, table_identifier, where, params)
  if (nchar(term, type = "chars") >= 3 && nrow(lines) == 0) {
    fallback_where <- if (case_sensitive) {
      "instr(search.content, ?) > 0"
    } else {
      "instr(lower(search.content), lower(?)) > 0"
    }
    lines <- btw_search_query(
      con,
      table_identifier,
      fallback_where,
      list(project_path, term)
    )
  }
  btw_search_shape_results(lines, limit, show_lines)
}

btw_search_regex <- function(
  con,
  table_identifier,
  project_path,
  term,
  limit,
  case_sensitive,
  show_lines
) {
  lines <- btw_search_query(con, table_identifier, "1 = 1", list(project_path))
  matched <- grepl(term, lines$content, perl = TRUE, ignore.case = !case_sensitive)
  btw_search_shape_results(lines[matched, , drop = FALSE], limit, show_lines)
}

btw_search_query <- function(con, table_identifier, where, params) {
  DBI::dbGetQuery(
    con,
    paste0(
      "SELECT search.path AS filename, files.size, files.mtime AS last_modified, ",
      "search.content, search.line ",
      "FROM ", table_identifier, " AS search ",
      "JOIN files ON files.project_path = ? AND files.path = search.path ",
      "WHERE ", where
    ),
    params = params
  )
}

btw_search_shape_results <- function(lines, limit, show_lines) {
  if (nrow(lines) == 0) {
    return(btw_search_empty_results(show_lines))
  }
  lines <- lines[order(-lines$last_modified, lines$filename, lines$line), , drop = FALSE]

  if (show_lines) {
    lines$content <- substr(lines$content, 1, 100)
    return(utils::head(lines, limit))
  }

  counts <- stats::aggregate(line ~ filename, data = lines, FUN = length)
  names(counts)[[2]] <- "n_matching_lines"
  metadata <- lines[!duplicated(lines$filename), c("filename", "size", "last_modified")]
  res <- merge(counts, metadata, by = "filename", sort = FALSE)
  res <- res[order(-res$n_matching_lines, -res$last_modified), , drop = FALSE]
  utils::head(res[, c("filename", "size", "last_modified", "n_matching_lines")], limit)
}

btw_search_empty_results <- function(show_lines) {
  if (show_lines) {
    return(data.frame(
      filename = character(),
      size = numeric(),
      last_modified = numeric(),
      content = character(),
      line = integer()
    ))
  }
  data.frame(
    filename = character(),
    size = numeric(),
    last_modified = numeric(),
    n_matching_lines = integer()
  )
}

btw_search_match_term <- function(term) {
  paste0('"', gsub('"', '""', term, fixed = TRUE), '"')
}

btw_search_is_busy <- function(cnd) {
  grepl("SQLITE_BUSY|database is locked|database is busy", conditionMessage(cnd))
}

btw_search_warn_busy <- local({
  warned <- FALSE
  function() {
    if (!warned) {
      warned <<- TRUE
      cli::cli_warn(
        "Code-search index is busy; searching the last available index instead."
      )
    }
  }
})

btw_search_prune_stale_indexes <- function(con) {
  days <- suppressWarnings(as.numeric(Sys.getenv("BTW_SEARCH_INDEX_STALE_DAYS", "30")))
  if (length(days) != 1 || is.na(days)) {
    days <- 30
  }
  cutoff <- format(Sys.time() - days * 24 * 60 * 60, tz = "UTC", usetz = TRUE)
  stale <- DBI::dbGetQuery(
    con,
    paste0(
      "SELECT path FROM projects ",
      "WHERE search_indexed_at IS NOT NULL AND search_indexed_at < ?"
    ),
    params = list(cutoff)
  )$path
  if (length(stale) == 0) {
    return(invisible(NULL))
  }

  for (project_path in stale) {
    table_name <- btw_db_search_table_name(project_path)
    if (DBI::dbExistsTable(con, table_name)) {
      DBI::dbExecute(
        con,
        paste0("DROP TABLE ", DBI::dbQuoteIdentifier(con, table_name))
      )
    }
    DBI::dbExecute(
      con,
      "DELETE FROM files WHERE project_path = ?",
      params = list(project_path)
    )
    DBI::dbExecute(
      con,
      "UPDATE projects SET search_indexed_at = NULL WHERE path = ?",
      params = list(project_path)
    )
  }

  invisible(NULL)
}

files_search_extensions <- function() {
  # fmt: skip
  default <- c("R", "Rmd", "qmd", "py", "js", "ts", "md", "scss", "css")

  # Check new option name first, then fall back to deprecated name
  res <- getOption("btw.files_search.extensions")
  if (!is.null(res)) {
    return(res)
  }

  res <- getOption("btw.files_code_search.extensions")
  if (!is.null(res)) {
    lifecycle::deprecate_warn(
      "1.2.0",
      I("option `btw.files_code_search.extensions`"),
      I("option `btw.files_search.extensions`")
    )
    return(res)
  }

  default
}

files_search_exclusions <- function() {
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

  # Check new option name first, then fall back to deprecated name
  res <- getOption("btw.files_search.exclusions")
  if (is.null(res)) {
    res <- getOption("btw.files_code_search.exclusions")
    if (!is.null(res)) {
      lifecycle::deprecate_warn(
        "1.2.0",
        I("option `btw.files_code_search.exclusions`"),
        I("option `btw.files_search.exclusions`")
      )
    }
  }

  if (is.null(res)) {
    return(default)
  }

  if ("DEFAULT" %in% res) {
    idx <- which(res == "DEFAULT")
    res <- c(res[seq_len(idx - 1)], default, res[-seq_len(idx)])
  }

  res
}
