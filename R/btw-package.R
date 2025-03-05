#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
## usethis namespace: end
NULL

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
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
