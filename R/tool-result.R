BtwToolResult <- S7::new_class(
  "BtwToolResult",
  parent = ellmer::ContentToolResult
)

btw_tool_result <- function(value, data = NULL, ..., cls = BtwToolResult) {
  if (is.data.frame(value)) {
    value <- as_json_rowwise(value)
  }

  cls(
    value = value,
    extra = list(data = data, ...)
  )
}
