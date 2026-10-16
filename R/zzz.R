.onLoad <- function(libname, pkgname) {
  # run_on_load()
  if ("S7_on_load" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_load")()
  } else {
    S7::methods_register()
  }

  pkg_env <- rlang::fn_env(btw_tools)
  for (tool_def in as_ellmer_tools(.btw_tools, force = TRUE)) {
    assign(tool_def@name, tool_def, envir = pkg_env)
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
      "private",
      "properties",
      "value"
    )
  )
}

# R6 binds `self` and `super` at runtime; declare them so codetools doesn't
# flag them as global variables in R CMD check.
utils::globalVariables(c("private", "self", "super"))

.onUnload <- function(...) {
  if ("S7_on_unload" %in% getNamespaceExports("S7")) {
    getExportedValue("S7", "S7_on_unload")()
  }
}

if ("S7_on_build" %in% getNamespaceExports("S7")) {
  getExportedValue("S7", "S7_on_build")()
}
