btw_db_path <- function() {
  path_btw_cache("btw.sqlite3")
}

btw_db_open <- function(path = btw_db_path(), .envir = parent.frame()) {
  fs::dir_create(fs::path_dir(path))

  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  withr::defer(DBI::dbDisconnect(con), envir = .envir)

  DBI::dbExecute(con, "PRAGMA busy_timeout = 2000")
  btw_db_enable_wal(con)
  btw_db_initialize(con)

  con
}

btw_db_enable_wal <- function(con) {
  mode <- DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  identical(tolower(mode$journal_mode[[1]]), "wal")
}

btw_db_initialize <- function(con) {
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS projects (
      path TEXT PRIMARY KEY,
      label TEXT,
      active_conversation_id TEXT,
      last_opened_at TEXT
    )"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS state (
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL
    )"
  )
  DBI::dbExecute(
    con,
    "INSERT OR IGNORE INTO state (key, value) VALUES (?, ?)",
    params = list("schema_version", "1")
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS conversations (
      scope TEXT NOT NULL,
      chat_id TEXT NOT NULL,
      id TEXT NOT NULL,
      title TEXT,
      created_at TEXT,
      updated_at TEXT,
      size_bytes REAL,
      data TEXT,
      PRIMARY KEY (scope, chat_id, id)
    )"
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS files (
      project_path TEXT NOT NULL,
      path TEXT NOT NULL,
      mtime REAL,
      size INTEGER,
      PRIMARY KEY (project_path, path)
    )"
  )

  invisible(NULL)
}

btw_db_search_table_name <- function(project_path) {
  project_path <- fs::path_norm(fs::path_abs(fs::path_expand(project_path)))
  paste0("search_", substr(rlang::hash(project_path), 1, 16))
}

btw_db_create_search_table <- function(con, project_path) {
  table_name <- btw_db_search_table_name(project_path)
  table_identifier <- as.character(DBI::dbQuoteIdentifier(con, table_name))

  DBI::dbExecute(
    con,
    paste0(
      "CREATE VIRTUAL TABLE IF NOT EXISTS ",
      table_identifier,
      " USING fts5(
        path UNINDEXED,
        line UNINDEXED,
        content,
        tokenize='trigram case_sensitive 0'
      )"
    )
  )

  table_name
}
