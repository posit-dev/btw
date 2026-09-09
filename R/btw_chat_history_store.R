# nocov start

# Persistent conversation history for btw_app(). When duckdb is installed,
# shinychat's chat history is stored in a single database in the btw user
# cache directory, keyed by project directory
# (btw_app_history_project_dir()).

btw_chat_history_db_path <- function() {
  path_btw_cache("chat_history.duckdb")
}

# Connect to a duckdb database, retrying while the file is locked by another
# process (duckdb allows only one writer process at a time). The connection
# and its driver are closed when the calling frame exits, even on error.
btw_duckdb_connect <- function(
  path,
  timeout = 10,
  wait = 0.5,
  max_delay = 2,
  .envir = parent.frame()
) {
  fs::dir_create(fs::path_dir(path))

  deadline <- Sys.time() + timeout
  delay <- wait

  repeat {
    drv <- NULL
    connected <- tryCatch(
      {
        drv <- duckdb::duckdb(dbdir = path)
        list(con = DBI::dbConnect(drv), drv = drv)
      },
      error = function(err) {
        if (!is.null(drv)) {
          duckdb::duckdb_shutdown(drv)
        }
        err
      }
    )

    if (!inherits(connected, "error")) {
      break
    }

    if (Sys.time() >= deadline) {
      cli::cli_abort(c(
        "Could not connect to {.path {path}} within {timeout} seconds.",
        "i" = "The database may be locked by another process."
      ))
    }

    Sys.sleep(delay)
    delay <- min(delay * 2, max_delay)
  }

  withr::defer(
    {
      DBI::dbDisconnect(connected$con)
      duckdb::duckdb_shutdown(connected$drv)
    },
    envir = .envir
  )

  connected$con
}

btw_duckdb_init <- function(con) {
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS conversations (
      scope VARCHAR NOT NULL,
      chat_id VARCHAR NOT NULL,
      id VARCHAR NOT NULL,
      title VARCHAR,
      created_at VARCHAR,
      updated_at VARCHAR,
      size_bytes DOUBLE,
      data VARCHAR,
      PRIMARY KEY (scope, chat_id, id)
    )"
  )
  invisible(NULL)
}

btw_conversation_store_duckdb <- R6::R6Class(
  "btw_conversation_store_duckdb",
  inherit = shinychat_conversation_store(),
  private = list(
    db_path = NULL,

    # Run `fn(con)` on a fresh connection to the store database, degrading
    # gracefully (warn + `fallback`) if the database can't be reached.
    with_db = function(op, fallback, fn) {
      tryCatch(
        {
          con <- btw_duckdb_connect(private$db_path)
          btw_duckdb_init(con)
          fn(con)
        },
        error = function(err) {
          cli::cli_warn(c(
            "Failed to {.emph {op}} chat history in {.path {private$db_path}}.",
            conditionMessage(err)
          ))
          fallback
        }
      )
    }
  ),
  public = list(
    initialize = function(db_path = NULL) {
      private$db_path <- db_path %||% btw_chat_history_db_path()
    },

    list = function(partition) {
      private$with_db(
        "list",
        list(),
        function(con) {
          rows <- DBI::dbGetQuery(
            con,
            "SELECT id, title, created_at, updated_at, size_bytes
             FROM conversations
             WHERE scope = ? AND chat_id = ?
             ORDER BY created_at DESC",
            params = list(partition$scope, partition$chat_id)
          )
          if (nrow(rows) == 0) {
            return(list())
          }
          lapply(seq_len(nrow(rows)), function(i) {
            list(
              id = rows$id[[i]],
              title = rows$title[[i]],
              created_at = rows$created_at[[i]],
              updated_at = rows$updated_at[[i]],
              size_bytes = rows$size_bytes[[i]]
            )
          })
        }
      )
    },

    get = function(partition, id) {
      private$with_db(
        "read",
        NULL,
        function(con) {
          rows <- DBI::dbGetQuery(
            con,
            "SELECT data FROM conversations
             WHERE scope = ? AND chat_id = ? AND id = ?",
            params = list(partition$scope, partition$chat_id, id)
          )
          if (nrow(rows) == 0) {
            return(NULL)
          }
          jsonlite::unserializeJSON(rows$data[[1]])
        }
      )
    },

    put = function(partition, record) {
      data <- jsonlite::serializeJSON(record, digits = 17)
      size_bytes <- as.double(nchar(data, type = "bytes"))

      private$with_db(
        "save",
        invisible(NULL),
        function(con) {
          DBI::dbExecute(
            con,
            "INSERT INTO conversations
               (scope, chat_id, id, title, created_at, updated_at, size_bytes, data)
             VALUES (?, ?, ?, ?, ?, ?, ?, ?)
             ON CONFLICT (scope, chat_id, id) DO UPDATE SET
               title = excluded.title,
               created_at = excluded.created_at,
               updated_at = excluded.updated_at,
               size_bytes = excluded.size_bytes,
               data = excluded.data",
            params = list(
              partition$scope,
              partition$chat_id,
              record$id,
              record$title,
              record$created_at,
              record$updated_at,
              size_bytes,
              data
            )
          )
          invisible(NULL)
        }
      )
    },

    delete = function(partition, id) {
      private$with_db(
        "delete",
        invisible(NULL),
        function(con) {
          DBI::dbExecute(
            con,
            "DELETE FROM conversations
             WHERE scope = ? AND chat_id = ? AND id = ?",
            params = list(partition$scope, partition$chat_id, id)
          )
          invisible(NULL)
        }
      )
    }
  )
)

btw_app_history_project_dir <- function(path_btw = NULL) {
  if (!is.null(path_btw)) {
    path <- fs::path_abs(fs::path_expand(path_btw))
    if (fs::dir_exists(path)) {
      return(as.character(fs::path_norm(path)))
    }
    return(as.character(fs::path_norm(fs::path_dir(path))))
  }

  marker <-
    path_find_in_project("DESCRIPTION") %||%
    path_find_in_project(".git")

  if (is.null(marker)) {
    return(normalizePath(getwd(), winslash = "/"))
  }

  as.character(fs::path_norm(fs::path_dir(marker)))
}

btw_app_history_options <- function(path_btw = NULL) {
  if (!rlang::is_installed("duckdb")) {
    cli::cli_inform(
      c(
        "Chat history: conversations aren't saved between {.fn btw_app} sessions.",
        "i" = "Install the {.pkg duckdb} R package to keep your conversation history in a local database: {.code install.packages(\"duckdb\")}."
      ),
      .frequency = "once",
      .frequency_id = "btw_app_history_duckdb"
    )
    return(TRUE)
  }

  shinychat_history_options(
    store = btw_conversation_store_duckdb$new(),
    scope = btw_app_history_project_dir(path_btw)
  )
}

# nocov end
