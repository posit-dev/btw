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
  platform <- trimws(capture.output(platform)[-1])
  platform <- sub(" +", " ", platform)
  platform <- paste(platform, collapse = "\n")

  sprintf("<system_info>\n%s\n</system_info>", platform)
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

  platform$date <- platform_date()
  platform$pandoc <- NULL
  platform$quarto <- NULL

  if (identical(Sys.getenv("POSITRON"), "1")) {
    platform$ui <- "Positron (a VS Code equivalent)"
  } else if (identical(Sys.getenv("RSTUDIO"), "1")) {
    platform$ui <- "RStudio"
  } else if (identical(Sys.getenv("TERM_PROGRAM"), "vscode")) {
    platform$ui <- "VS Code"
  }

  recode <- c(
    "version" = "r_version",
    "collate" = "locale",
    "ctype" = "encoding",
    "tz" = "timezone"
  )

  needs_recode <- names(platform) %in% names(recode)

  names(platform)[needs_recode] <- recode[names(platform)[needs_recode]]
  names(platform) <- sprintf("%s:", toupper(names(platform)))

  platform
}
