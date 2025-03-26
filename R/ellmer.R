contents_text <- S7::new_external_generic(
  package = "ellmer",
  name = "contents_text",
  dispatch_args = "content"
)

S7::method(contents_text, ellmer::ContentToolRequest) <- function(
  content,
  ...
) {
  if (length(content@arguments) == 0) {
    call <- call2(content@name)
  } else {
    call <- call2(content@name, !!!content@arguments)
  }
  sprintf("`%s`", format(call))
}

S7::method(contents_text, ellmer::ContentToolResult) <- function(
  content,
  ...
) {
  x <- sprintf(
    "<details><summary>Result</summary>\n\n%s\n\n</details>",
    paste(content@value, collapse = "\n")
  )
  class(x) <- c("html", "character")
  x
}
