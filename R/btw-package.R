#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
## usethis namespace: end
NULL

utils::globalVariables(c(
  "aliases",
  "capture.output",
  "Entry",
  "Field",
  "help",
  "help.search",
  "installed.packages",
  "Name",
  "title",
  "topic_id"
))

.yes_we_use_ellmer <- function() {
  ellmer::tool()
}
