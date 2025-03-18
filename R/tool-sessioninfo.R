#' Tool: Describe user's platform
#'
#' Describes the R version, operating system, and language and locale settings
#' for the user's system. When using [btw_client()] or [btw_app()], this
#' information is automatically included in the system prompt.
#'
#' @seealso [btw_register_tools()]
#'
#' @returns Returns a single string describing the user's platform.
#'
#' @examples
#' cat(btw_tool_describe_platform())
#'
#' @family Tools
#' @export
btw_tool_describe_platform <- function() {
  platform <- platform_info()

  # fmt: skip
  description <- paste0(
    "The user is running ", platform$version, " on a ", platform$os, " system (",
    platform$system, "). ",
    "The user interface being used is ", ifelse(platform$ui == "", "terminal/console", platform$ui), ". ",
    "The system's language setting is ", platform$language, ", ",
    "with locale collation rules set to '", platform$collate, "' ",
    "and character encoding set to '", platform$ctype, "'. ",
    "The system is configured to use the '", platform$tz, "' timezone. ",
    "The current date is ", platform_date(), ". ",
    "When processing date-related queries or timezone-specific information, please account for these system settings."
  )

  return(description)
}

.btw_add_to_tools(
  name = "btw_tool_describe_platform",
  group = "session",
  tool = function() {
    ellmer::tool(
      btw_tool_describe_platform,
      .description = "Describes the R version, operating system, language and locale settings for the user's system."
    )
  }
)

platform_date <- function(when = Sys.time()) {
  format(when, "%A, %B %d, %Y (%F)")
}

platform_info <- function() {
  platform <- sessioninfo::platform_info()

  if (identical(Sys.getenv("POSITRON"), "1")) {
    platform$ui <- "Positron (a VS Code equivalent IDE for data science)"
  } else if (identical(Sys.getenv("RSTUDIO"), "1")) {
    platform$ui <- "RStudio"
  } else if (identical(Sys.getenv("TERM_PROGRAM"), "vscode")) {
    platform$ui <- "VS Code"
  }

  if (identical(platform$language, "(EN)")) {
    platform$language <- "EN (implied default)"
  }

  platform
}
