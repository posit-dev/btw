.onLoad <- function(libname, pkgname) {
  # run_on_load()
  S7::methods_register()

  pkg_env <- rlang::fn_env(btw_tools)
  for (tool_def in as_ellmer_tools(.btw_tools)) {
    assign(tool_def@name, tool_def, envir = pkg_env)
  }
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
