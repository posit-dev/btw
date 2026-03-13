#' Install the btw CLI
#'
#' Installs the `btw` CLI launcher using [Rapp::install_pkg_cli_apps()].
#' [Rapp](https://github.com/r-lib/Rapp) is required to build and install the
#' CLI.
#'
#' @param destdir Directory where the CLI launcher will be installed. If `NULL`,
#'   the default location used by [Rapp::install_pkg_cli_apps()] is used.
#'
#' @returns The result of [Rapp::install_pkg_cli_apps()], invisibly.
#'
#' @export
install_btw_cli <- function(destdir = NULL) {
  rlang::check_installed(c(
    "Rapp (>= 0.3.0)",
    "devtools",
    "pkgload",
    "callr",
    "covr",
    "testthat",
    "rmarkdown",
    "pkgsearch"
  ))

  invisible(Rapp::install_pkg_cli_apps(package = "btw", destdir = destdir))
}
