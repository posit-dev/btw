.onLoad <- function(libname, pkgname) {
  # run_on_load()
  S7::methods_register()

  pkg_env <- rlang::fn_env(btw_tools)
  for (tool_def in as_ellmer_tools(.btw_tools, force = TRUE)) {
    assign(tool_def@name, tool_def, envir = pkg_env)
  }

  # Patch ellmer:::Chat to add set_model() if it doesn't exist
  ellmer_chat <- utils::getFromNamespace("Chat", "ellmer")
  if (!is.null(ellmer_chat)) {
    if (!"set_model" %in% names(ellmer_chat$public_methods)) {
      ellmer_chat$set("public", "set_model", function(model) {
        old <- private$provider@model
        private$provider@model <- model
        invisible(old)
      })
    }
  }

  rlang::run_on_load()
}

# enable usage of <S7_object>@name in package code
#' @rawNamespace if (getRversion() < "4.3.0") importFrom("S7", "@")
NULL

if (getRversion() < "4.3.0") {
  utils::globalVariables(
    c(
      "annotations",
      "arguments",
      "description",
      "extra",
      "name",
      "properties",
      "value"
    )
  )
}
