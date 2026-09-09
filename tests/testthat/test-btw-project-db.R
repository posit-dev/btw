skip_if_not_installed("DBI")
skip_if_not_installed("RSQLite")

test_that("btw_db_open() initializes the shared schema", {
  dir <- withr::local_tempdir()
  path <- fs::path(dir, "cache", "btw.sqlite3")
  con <- btw:::btw_db_open(path, .envir = environment())

  expect_true(fs::file_exists(path))

  tables <- DBI::dbGetQuery(
    con,
    "SELECT name FROM sqlite_master WHERE type = 'table'"
  )$name
  expect_setequal(
    c("projects", "state", "conversations", "files"),
    intersect(tables, c("projects", "state", "conversations", "files"))
  )
  expect_identical(
    DBI::dbGetQuery(
      con,
      "SELECT value FROM state WHERE key = ?",
      params = list("schema_version")
    )$value,
    "1"
  )
})

test_that("per-project search table names are deterministic and safe", {
  project <- "project; DROP TABLE projects; --"
  table_name <- btw:::btw_db_search_table_name(project)

  expect_identical(table_name, btw:::btw_db_search_table_name(project))
  expect_match(table_name, "^search_[a-f0-9]{16}$")

  dir <- withr::local_tempdir()
  con <- btw:::btw_db_open(fs::path(dir, "btw.sqlite3"), .envir = environment())
  expect_identical(btw:::btw_db_create_search_table(con, project), table_name)

  sql <- DBI::dbGetQuery(
    con,
    "SELECT sql FROM sqlite_master WHERE type = 'table' AND name = ?",
    params = list(table_name)
  )$sql
  expect_match(sql, "tokenize='trigram case_sensitive 0'", fixed = TRUE)
})

test_that("btw_db_open() disconnects when its calling frame exits", {
  dir <- withr::local_tempdir()
  path <- fs::path(dir, "nested", "btw.sqlite3")

  open_database <- function(path) {
    con <- btw:::btw_db_open(path)
    expect_true(DBI::dbIsValid(con))
    con
  }

  con <- open_database(path)
  expect_false(DBI::dbIsValid(con))
})
