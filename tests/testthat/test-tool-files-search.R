local_file_search <- function(..., .env = caller_env()) {
  search <- btw_tool_files_search_factory(...)
  state <- environment(search)$state

  withr::defer(
    {
      if (!is.null(state$con) && DBI::dbIsValid(state$con)) {
        DBI::dbDisconnect(state$con)
      }
    },
    envir = .env
  )

  search
}

test_that("file search is not registered without RSQLite", {
  local_mocked_bindings(
    is_installed = function(package, version = NULL) package != "RSQLite",
    .package = "btw"
  )

  expect_false("btw_tool_files_search" %in% names(btw_tools("files")))
})

test_that("file search definition materializes without RSQLite", {
  local_mocked_bindings(
    is_installed = function(package, version = NULL) package != "RSQLite",
    .package = "btw"
  )

  expect_silent(as_ellmer_tools(
    .btw_tools["btw_tool_files_search"],
    force = TRUE
  ))
})

test_that("file search persists its index and refreshes changed files", {
  local_btw_db()
  search_dir <- fs::path_temp("search-persistence")
  fs::dir_create(search_dir)
  withr::local_dir(search_dir)
  writeLines("first_search_term <- TRUE", "code.R")

  first <- local_file_search()
  first_data <- jsonlite::fromJSON(S7::prop(
    first("first_search_term", show_lines = TRUE),
    "value"
  ))
  expect_equal(first_data$content, "first_search_term <- TRUE")

  con <- btw_db_open(.envir = environment())
  table_name <- btw_db_search_table_name(getwd())
  expect_true(DBI::dbExistsTable(con, table_name))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM files")$n, 1)

  writeLines("second_search_term <- TRUE", "code.R")
  second_data <- jsonlite::fromJSON(S7::prop(
    first("second_search_term", show_lines = TRUE),
    "value"
  ))
  expect_equal(second_data$content, "second_search_term <- TRUE")
  expect_equal(
    length(jsonlite::fromJSON(S7::prop(
      first("first_search_term", show_lines = TRUE),
      "value"
    ))),
    0
  )
})

test_that("file search optimizes only after many indexed-file deletions", {
  local_btw_db()
  search_dir <- fs::path_temp("search-optimize")
  fs::dir_create(search_dir)
  withr::local_dir(search_dir)
  writeLines("first_search_term <- TRUE", "code.R")
  search <- local_file_search()

  optimized <- 0L
  local_mocked_bindings(
    btw_search_optimize = function(...) {
      optimized <<- optimized + 1L
    },
    .package = "btw"
  )

  search("first_search_term")
  writeLines("second_search_term <- TRUE", "code.R")
  search("second_search_term")
  expect_equal(optimized, 0L)

  paths <- fs::path(sprintf("code-%03d.R", seq_len(101L)))
  purrr::walk(paths, writeLines, text = "many_search_terms <- TRUE")
  search("many_search_terms")
  fs::file_delete(paths)
  search("many_search_terms")
  expect_equal(optimized, 1L)
})

test_that("file search treats literal FTS syntax as literal text", {
  local_btw_db()
  search_dir <- fs::path_temp("search-literals")
  fs::dir_create(search_dir)
  withr::local_dir(search_dir)
  writeLines(c("on.exit(foo)", "call <- c(1,2)", "quote <- 'a\"b'"), "code.R")
  search <- local_file_search()

  expect_equal(
    nrow(jsonlite::fromJSON(S7::prop(search("on.exit", show_lines = TRUE), "value"))),
    1
  )
  expect_equal(
    nrow(jsonlite::fromJSON(S7::prop(search("c(1,2)", show_lines = TRUE), "value"))),
    1
  )
  expect_equal(
    nrow(jsonlite::fromJSON(S7::prop(search('a"b', show_lines = TRUE), "value"))),
    1
  )
})

test_that("file search falls back for short terms and preserves case semantics", {
  local_btw_db()
  search_dir <- fs::path_temp("search-case")
  fs::dir_create(search_dir)
  withr::local_dir(search_dir)
  writeLines(c("ab <- 1", "snake_case <- 2", "snakeCase <- 3"), "code.R")
  search <- local_file_search()

  expect_equal(
    nrow(jsonlite::fromJSON(S7::prop(search("ab", show_lines = TRUE), "value"))),
    1
  )
  insensitive <- jsonlite::fromJSON(S7::prop(
    search("SNAKE_CASE", case_sensitive = FALSE, show_lines = TRUE),
    "value"
  ))
  expect_equal(insensitive$content, "snake_case <- 2")
  expect_equal(
    length(jsonlite::fromJSON(S7::prop(
      search("SNAKE_CASE", case_sensitive = TRUE, show_lines = TRUE),
      "value"
    ))),
    0
  )
})

test_that("file search applies regular expressions in R", {
  local_btw_db()
  search_dir <- fs::path_temp("search-regex")
  fs::dir_create(search_dir)
  withr::local_dir(search_dir)
  writeLines(c("alpha_12 <- TRUE", "alpha_x <- FALSE"), "code.R")
  search <- local_file_search()

  data <- jsonlite::fromJSON(S7::prop(
    search("^alpha_[0-9]+", use_regex = TRUE, show_lines = TRUE),
    "value"
  ))
  expect_equal(data$content, "alpha_12 <- TRUE")
})

test_that("package-source factories use a temporary database", {
  local_btw_db()
  withr::local_tempdir()
  source_dir <- fs::path_temp("package-source")
  fs::dir_create(source_dir)
  writeLines("temporary_source_term <- TRUE", fs::path(source_dir, "source.R"))

  search <- local_file_search(source_dir, restrict_to_wd = FALSE)
  data <- jsonlite::fromJSON(S7::prop(
    search("temporary_source_term", show_lines = TRUE),
    "value"
  ))
  expect_equal(data$content, "temporary_source_term <- TRUE")

  con <- btw_db_open(.envir = environment())
  expect_false(DBI::dbExistsTable(con, btw_db_search_table_name(source_dir)))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM files")$n, 0)
})

test_that("stale search cleanup is limited to the idle project index", {
  local_btw_db()
  withr::local_envvar(BTW_SEARCH_INDEX_STALE_DAYS = "1")
  con <- btw_db_open(.envir = environment())
  idle_path <- fs::path_temp("idle-project")
  active_path <- fs::path_temp("active-project")
  idle_table <- btw_db_create_search_table(con, idle_path)
  active_table <- btw_db_create_search_table(con, active_path)
  old <- format(Sys.time() - 2 * 24 * 60 * 60, tz = "UTC", usetz = TRUE)
  now <- format(Sys.time(), tz = "UTC", usetz = TRUE)

  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO projects (path, label, last_opened_at, search_indexed_at) ",
      "VALUES (?, ?, ?, ?), (?, ?, ?, ?)"
    ),
    params = list(
      idle_path, "idle", old, old,
      active_path, "active", now, now
    )
  )
  DBI::dbExecute(
    con,
    "INSERT INTO files (project_path, path, mtime, size) VALUES (?, ?, ?, ?), (?, ?, ?, ?)",
    params = list(idle_path, "idle.R", 1, 1, active_path, "active.R", 1, 1)
  )
  DBI::dbExecute(
    con,
    "INSERT INTO conversations (scope, chat_id, id, data) VALUES (?, ?, ?, ?)",
    params = list("project", "chat", "conversation", "{}")
  )

  btw_search_prune_stale_indexes(con)

  expect_false(DBI::dbExistsTable(con, idle_table))
  expect_true(DBI::dbExistsTable(con, active_table))
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM files WHERE project_path = ?",
      params = list(idle_path)
    )$n,
    0
  )
  expect_equal(
    DBI::dbGetQuery(
      con,
      "SELECT COUNT(*) AS n FROM files WHERE project_path = ?",
      params = list(active_path)
    )$n,
    1
  )
  expect_equal(
    DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM conversations")$n,
    1
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT search_indexed_at FROM projects WHERE path = ?",
      params = list(idle_path)
    )$search_indexed_at,
    NA_character_
  )
})
