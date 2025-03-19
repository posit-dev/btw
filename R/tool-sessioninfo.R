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
    "The user is running ", platform$version, " on ", platform$os, " (",
    platform$system, ") ",
    "using ", ifelse(platform$ui == "", "a terminal/console", platform$ui), ". ",
    "System language: ", platform$language, "; ",
    "locale: ", platform$collate, "; ",
    "character encoding: ", platform$ctype, "; ",
    "timezone: ", platform$tz, ". ",
    "The current date is ", platform_date(), ". ",
    "Please account for these system settings in all responses."
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
  format(when, "%A, %B %e, %Y (%F)")
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
    platform$language <- "EN (implied default)" # nocov
  }

  platform
}
