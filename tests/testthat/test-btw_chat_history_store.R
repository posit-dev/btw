skip_if_not_installed("RSQLite")
skip_if_no_shinychat_v05()

history_record <- function(
  id,
  title = paste("Conversation", id),
  created_at = "2026-09-07T12:00:00Z",
  updated_at = created_at
) {
  list(
    schema_version = 1L,
    id = id,
    title = title,
    title_source = NULL,
    response_count = 1L,
    created_at = created_at,
    updated_at = updated_at,
    client_info = list(provider = "test", model = "test"),
    nodes = list(
      n1 = list(turns = list(list(role = "user", content = "hello")))
    ),
    current_leaf = "n1",
    values = list(x = 1, y = "a"),
    bookmark_state_id = NULL
  )
}

new_history_store <- function(.env = caller_env()) {
  db <- local_btw_db(.env)
  btw:::btw_conversation_store_sqlite$new(db)
}

test_that("btw_conversation_store_sqlite round-trips a record", {
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")
  record <- history_record("abc123")

  store$put(partition, record)
  expect_identical(store$get(partition, "abc123"), record)
})

test_that("list() returns conversation meta, newest first", {
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")

  store$put(
    partition,
    history_record("old", created_at = "2026-01-01T00:00:00Z")
  )
  store$put(
    partition,
    history_record("new", created_at = "2026-09-07T00:00:00Z")
  )

  metas <- store$list(partition)
  expect_length(metas, 2)
  expect_identical(
    vapply(metas, function(m) m$id, character(1)),
    c("new", "old")
  )
  expect_true(all(vapply(metas, function(m) m$size_bytes > 0, logical(1))))
})

test_that("put() upserts an existing conversation", {
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")

  store$put(partition, history_record("abc123", title = "Before"))
  store$put(partition, history_record("abc123", title = "After"))

  metas <- store$list(partition)
  expect_length(metas, 1)
  expect_identical(metas[[1]]$title, "After")
})

test_that("get() returns NULL for a missing conversation", {
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")

  expect_null(store$get(partition, "missing"))
})

test_that("conversations are isolated by scope and chat_id", {
  store <- new_history_store()
  scope_a <- list(chat_id = "chat", scope = "project-a")
  scope_b <- list(chat_id = "chat", scope = "project-b")
  chat_other <- list(chat_id = "other", scope = "project-a")

  store$put(scope_a, history_record("shared-id", title = "In A"))
  store$put(scope_b, history_record("shared-id", title = "In B"))
  store$put(chat_other, history_record("shared-id", title = "Other chat"))

  expect_identical(store$get(scope_a, "shared-id")$title, "In A")
  expect_identical(store$get(scope_b, "shared-id")$title, "In B")
  expect_identical(store$get(chat_other, "shared-id")$title, "Other chat")
  expect_length(store$list(scope_a), 1)
})

test_that("delete() removes a conversation and is a no-op for missing ids", {
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")

  store$put(partition, history_record("abc123"))
  expect_length(store$list(partition), 1)

  expect_invisible(store$delete(partition, "abc123"))
  expect_null(store$get(partition, "abc123"))
  expect_length(store$list(partition), 0)

  expect_invisible(store$delete(partition, "never-existed"))
})

test_that("put() prunes conversations older than the retention period", {
  withr::local_envvar(BTW_HISTORY_RETENTION_DAYS = "1")
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")
  old <- format(
    Sys.time() - 2 * 24 * 60 * 60,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  store$put(partition, history_record("old", created_at = old, updated_at = old))
  store$put(partition, history_record("new", created_at = now, updated_at = now))

  expect_null(store$get(partition, "old"))
  expect_identical(store$get(partition, "new")$id, "new")
})

test_that("history retention can disable recording or retain forever", {
  partition <- list(chat_id = "chat", scope = "project-a")
  old <- format(
    Sys.time() - 366 * 24 * 60 * 60,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )

  withr::local_envvar(BTW_HISTORY_RETENTION_DAYS = "0")
  expect_true(btw:::btw_app_history_options())
  disabled <- new_history_store()
  disabled$put(partition, history_record("disabled"))
  expect_null(disabled$get(partition, "disabled"))

  withr::local_envvar(BTW_HISTORY_RETENTION_DAYS = "-1")
  forever <- new_history_store()
  forever$put(partition, history_record("old", created_at = old, updated_at = old))
  expect_identical(forever$get(partition, "old")$id, "old")
})

test_that("search() and total_size() work via the base class defaults", {
  store <- new_history_store()
  partition <- list(chat_id = "chat", scope = "project-a")

  store$put(partition, history_record("one", title = "Data cleaning"))
  store$put(partition, history_record("two", title = "Model fitting"))

  expect_length(store$search(partition, "cleaning"), 1)
  expect_identical(store$search(partition, "cleaning")[[1]]$id, "one")
  expect_equal(
    store$total_size(partition),
    sum(vapply(store$list(partition), function(m) m$size_bytes, double(1)))
  )
})

test_that("btw_app_history_project_dir() resolves from path_btw", {
  path <- withr::local_tempfile(lines = "# btw")
  expect_identical(
    btw:::btw_app_history_project_dir(path),
    as.character(fs::path_norm(fs::path_dir(fs::path_abs(path))))
  )

  dir <- withr::local_tempdir()
  expect_identical(
    btw:::btw_app_history_project_dir(dir),
    as.character(fs::path_norm(fs::path_abs(dir)))
  )
})

test_that("btw_app_history_options() returns SQLite-backed history options", {
  options <- btw:::btw_app_history_options()
  expect_true(inherits(options$store, "ConversationStore"))
  expect_identical(
    options$scope,
    btw:::btw_app_history_project_dir()
  )
})
