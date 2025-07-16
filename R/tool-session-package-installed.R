#' Tool: Check if a package is installed
#'
#' Checks if a package is installed in the current session. If the package is
#' installed, it returns the version number. If not, it suggests packages with
#' similar names to help the LLM resolve typos.
#'
#' @examples
#' btw_tool_session_check_package_installed("dplyr")@value
#'
#' tryCatch(
#'   btw_tool_session_check_package_installed("dplry"),
#'   error = function(err) {
#'     cat(conditionMessage(err))
#'   }
#' )
#'
#' @param package_name The name of the package.
#' @returns A message indicating whether the package is installed and
#'   its version, or an error indicating that the package is not installed.
#'
#' @seealso [btw_tools()]
#' @family tools
#' @export
btw_tool_session_check_package_installed <- function(package_name) {
  check_installed(package_name)

  version <- package_version(package_name)

  BtwToolResult(
    value = glue_(
      "Package `{{package_name}}` version {{version}} is installed."
    ),
    extra = list(
      package = package_name,
      version = version
    )
  )
}

.btw_add_to_tools(
  "btw_tool_session_check_package_installed",
  group = "session",
  tool = function() {
    ellmer::tool(
      btw_tool_session_check_package_installed,
      name = "btw_tool_session_check_package_installed",
      description = "Check if a package is installed in the current session.",
      annotations = ellmer::tool_annotations(
        title = "Package Check",
        read_only_hint = TRUE,
        open_world_hint = FALSE,
        idempotent_hint = FALSE
      ),
      arguments = list(
        package_name = ellmer::type_string(
          "The exact name of the package.",
          required = TRUE
        )
      )
    )
  }
)

# rlang's version prompts when interactive
check_installed <- function(package_name, call = caller_env()) {
  if (is_installed(package_name)) {
    return(invisible())
  }

  candidates <- find_package_candidates(package_name)

  cli::cli_abort(
    c(
      "Package {.pkg {package_name}} is not installed.",
      "i" = "Did you mean {.or {.val {candidates}}}?"
    ),
    call = call
  )
}

package_version <- function(package_name) {
  if (identical(package_name, "R")) {
    return(paste(R.version[c("major", "minor")], collapse = "."))
  }
  if (is_installed(package_name)) {
    as.character(utils::packageVersion(package_name))
  }
}

find_package_candidates <- function(package_name, installed_only = TRUE) {
  all_packages <-
    if (installed_only) {
      rownames(installed.packages())
    } else {
      rownames(utils::available.packages())
    }

  dists <- utils::adist(tolower(package_name), tolower(all_packages))

  if (any(dists == 0)) {
    candidate_indices <- which(dists == 0)
  } else {
    candidate_indices <- order(dists)[1:5]
  }

  all_packages[candidate_indices]
}
