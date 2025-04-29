#' Perform a SQL query on the data, and return the results as JSON.
#' 
#' @param query A DuckDB SQL query; must be a SELECT statement.
# TODO: should any `get`table data frame work here?
#' @param data_frame The name of the data frame.
#' @return The results of the query as a JSON string.
btw_tool_env_query_data_frame <- function(query, data_frame) {
  d <- get(data_frame)
  conn <- btw_connection()

  if (!DBI::dbExistsTable(conn, data_frame)) {
    duckdb::duckdb_register(conn, data_frame, d, experimental = FALSE)
  }
  
  res <- DBI::dbGetQuery(conn, query)

  btw_tool_env_describe_data_frame(res, format = "json", dims = c(Inf, Inf))
}

.btw_add_to_tools(
  name = "btw_tool_env_query_data_frame",
  group = "env",
  tool = function() {
    ellmer::tool(
      btw_tool_env_query_data_frame,
      .name = "btw_tool_env_query_data_frame",
      .description = 
        "Run a DuckDB SQL query against a data frame.
         Use this tool instead of btw_tool_env_describe_data_frame to run more
         targeted queries, e.g. calculating statistics on specific columns.",
      query = ellmer::type_string("A DuckDB SQL query, as a string."),
      data_frame = ellmer::type_string("The name of the data frame, as a string.")
    )
  }
)

btw_connect <- function() {
  # TODO: also check if the connection is active
  if (is.null(btw_env$conn)) {
    btw_env$conn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  }
}

btw_connection <- function() {
  btw_connect()

  btw_env$conn
}
