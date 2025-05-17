BtwToolResult <- S7::new_class(
  "BtwToolResult",
  parent = ellmer::ContentToolResult
)

btw_tool_result <- function(value, data = NULL, ..., cls = BtwToolResult) {
  cls(
    value = value,
    extra = list(data = data, ...)
  )
}
