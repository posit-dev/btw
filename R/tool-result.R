BtwToolResult <- S7::new_class(
  "BtwToolResult",
  parent = ellmer::ContentToolResult
)

# OpenAI and other providers require the tool result value to be a single
# JSON-serializable value; multi-element character vectors are serialized as
# arrays of bare strings, which the APIs reject.
as_tool_result_value <- function(value) {
  if (is.data.frame(value)) {
    return(as_json_rowwise(value))
  }

  if (is.character(value) && length(value) > 1) {
    return(paste(value, collapse = "\n"))
  }

  value
}

btw_tool_result <- function(value, data = NULL, ..., cls = BtwToolResult) {
  cls(
    value = as_tool_result_value(value),
    extra = list(data = data, ...)
  )
}
