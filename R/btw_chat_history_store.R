# nocov start

# Persistent conversation history for btw_app(). When RSQLite is installed,
# shinychat's chat history is stored in the shared btw database, keyed by
# project directory (btw_app_history_project_dir()).

btw_history_retention_days <- function() {
  value <- Sys.getenv("BTW_HISTORY_RETENTION_DAYS", unset = "")
  if (!nzchar(value)) {
    return(365L)
  }

  if (!grepl("^-?[0-9]+$", value)) {
    return(365L)
  }

  value <- suppressWarnings(as.integer(value))
  if (is.na(value) || value < -1L) {
    return(365L)
  }

  value
}

btw_conversation_store_sqlite <- function() {
  rlang::check_installed("R6")

  R6::R6Class(
    "btw_conversation_store_sqlite",
    inherit = shinychat_conversation_store(),
    private = list(
    db_path = NULL,
    retention_days = NULL,

    # Run `fn(con)` on a fresh connection to the store database, degrading
    # gracefully (warn + `fallback`) if the database can't be reached.
    with_db = function(op, fallback, fn, retry_busy = FALSE) {
      tryCatch(
        {
          retries <- if (retry_busy) 3L else 0L
          attempt <- 0L

          repeat {
            run <- function() {
              con <- btw_db_open(private$db_path, .envir = environment())
              fn(con)
            }
            result <- tryCatch(
              run(),
              error = identity
            )

            if (!inherits(result, "error")) {
              return(result)
            }

            attempt <- attempt + 1L
            if (
              !retry_busy ||
                !private$is_sqlite_busy(result) ||
                attempt > retries
            ) {
              stop(result)
            }

            Sys.sleep(0.2 * 2 ^ (attempt - 1L))
          }
        },
        error = function(err) {
          cli::cli_warn(c(
            "Failed to {.emph {op}} chat history in {.path {private$db_path}}.",
            conditionMessage(err)
          ))
          fallback
        }
      )
    },

    is_sqlite_busy = function(err) {
      message <- conditionMessage(err)
      grepl("SQLITE_BUSY", message, fixed = TRUE, ignore.case = TRUE) ||
        grepl(
          "\\b(database|database table|database schema) (is )?locked\\b",
          message,
          ignore.case = TRUE,
          perl = TRUE
        )
    },

    prune = function(con) {
      if (private$retention_days < 0L) {
        return(invisible(NULL))
      }

      cutoff <- format(
        Sys.time() - private$retention_days * 24 * 60 * 60,
        "%Y-%m-%dT%H:%M:%SZ",
        tz = "UTC"
      )
      DBI::dbExecute(
        con,
        "DELETE FROM conversations WHERE updated_at < ?",
        params = list(cutoff)
      )
      invisible(NULL)
    }
    ),
    public = list(
    initialize = function(db_path = NULL) {
      private$db_path <- db_path %||% btw_db_path()
      private$retention_days <- btw_history_retention_days()
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
      if (private$retention_days == 0L) {
        return(invisible(NULL))
      }

      data <- jsonlite::serializeJSON(record, digits = 17)
      size_bytes <- as.double(nchar(data, type = "bytes"))

      invisible(private$with_db(
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
          private$prune(con)
          invisible(NULL)
        },
        retry_busy = TRUE
      ))
    },

    delete = function(partition, id) {
      invisible(private$with_db(
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
        },
        retry_busy = TRUE
      ))
    }
    )
  )
}

btw_app_history_project_dir <- function(path_btw = NULL) {
  if (!is.null(path_btw) && !identical(path_btw, FALSE)) {
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
  retention_days <- btw_history_retention_days()
  if (
    retention_days == 0L ||
      !rlang::is_installed("RSQLite")
  ) {
    if (retention_days == 0L) {
      return(TRUE)
    }

    cli::cli_inform(
      c(
        "Chat history: conversations aren't saved between {.fn btw_app} sessions.",
        "i" = "Install the {.pkg RSQLite} R package to keep your conversation history in a local database: {.code install.packages(\"RSQLite\")}."
      ),
      .frequency = "once",
      .frequency_id = "btw_app_history_sqlite"
    )
    return(TRUE)
  }

  shinychat_history_options(
    store = btw_conversation_store_sqlite()$new(),
    scope = btw_app_history_project_dir(path_btw),
    restore_mode = "none"
  )
}

btw_project_active_conversation_id <- function(
  project_path,
  db_path = btw_db_path()
) {
  con <- btw_db_open(db_path, .envir = environment())
  project <- DBI::dbGetQuery(
    con,
    "SELECT active_conversation_id FROM projects WHERE path = ?",
    params = list(project_path)
  )

  if (nrow(project) == 0) {
    return(NULL)
  }

  project$active_conversation_id[[1]]
}

btw_project_set_active_conversation_id <- function(
  project_path,
  conversation_id,
  db_path = btw_db_path()
) {
  con <- btw_db_open(db_path, .envir = environment())
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO projects (path, active_conversation_id, last_opened_at) ",
      "VALUES (?, ?, ?) ",
      "ON CONFLICT(path) DO UPDATE SET ",
      "active_conversation_id = excluded.active_conversation_id, ",
      "last_opened_at = excluded.last_opened_at"
    ),
    params = list(
      project_path,
      conversation_id,
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  )
  invisible(NULL)
}

btw_app_history_restore_project_conversation <- function(
  controller,
  project_path
) {
  conversation_id <- tryCatch(
    btw_project_active_conversation_id(project_path),
    error = function(err) {
      cli::cli_warn(c(
        "Failed to read the active chat history conversation.",
        conditionMessage(err)
      ))
      NULL
    }
  )
  if (is.null(conversation_id)) {
    return(FALSE)
  }

  tryCatch(
    {
      record <- controller$get_record(controller$partition, conversation_id)
      if (is.null(record)) {
        return(FALSE)
      }
      controller$switch_to(conversation_id)
      TRUE
    },
    error = function(err) {
      cli::cli_warn(c(
        "Failed to restore the active chat history conversation.",
        conditionMessage(err)
      ))
      FALSE
    }
  )
}

btw_app_history_use_project_pointer <- function(
  chat,
  controller,
  project_path
) {
  settled <- FALSE
  previous_on_settled <- controller$on_settled

  controller$on_settled <- function(restored) {
    if (!settled) {
      settled <<- TRUE
      btw_app_history_restore_project_conversation(controller, project_path)
    }

    if (!is.null(previous_on_settled)) {
      previous_on_settled(restored)
    }
  }

  shiny::observeEvent(
    chat$history$conversation_id(),
    ignoreNULL = TRUE,
    {
      tryCatch(
        btw_project_set_active_conversation_id(
          project_path,
          chat$history$conversation_id()
        ),
        error = function(err) {
          cli::cli_warn(c(
            "Failed to save the active chat history conversation.",
            conditionMessage(err)
          ))
        }
      )
    }
  )

  invisible(NULL)
}

# nocov end
