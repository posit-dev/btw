#' Install the btw CLI
#'
#' Installs the `btw` CLI launcher using [Rapp::install_pkg_cli_apps()].
#' [Rapp](https://github.com/r-lib/Rapp) is required to build and install the
#' CLI.
#'
#' After installing the CLI, you will be offered the option to install the
#' `r-btw-cli` skill, which helps AI coding assistants discover and use the
#' `btw` CLI. If you decline or are in a non-interactive session, the skill
#' instructions are copied to the clipboard (or printed) so you can add them
#' to your `CLAUDE.md`, `AGENTS.md`, or other context file manually.
#'
#' @param destdir Directory where the CLI launcher will be installed. If `NULL`,
#'   the default location used by [Rapp::install_pkg_cli_apps()] is used.
#' @inheritDotParams Rapp::install_pkg_cli_apps -package -destdir
#'
#' @returns The result of [Rapp::install_pkg_cli_apps()], invisibly.
#'
#' @export
install_btw_cli <- function(destdir = NULL, ...) {
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

  result <- Rapp::install_pkg_cli_apps(package = "btw", destdir = destdir, ...)

  for (path in result) {
    cli::cli_alert_success("Installed {.code btw} CLI to {.path {path}}")
  }

  install_btw_cli_skill()

  invisible(result)
}

install_btw_cli_skill <- function() {
  if (!is_interactive()) {
    install_btw_cli_skill_clipboard()
    return(invisible())
  }

  cli::cli_h2("Install btw CLI skill")
  cli::cli_text(
    "The {.field r-btw-cli} skill helps AI coding assistants discover and use
    the {.code btw} CLI."
  )

  scope_choices <- c(
    "~/.agents/skills (Recommended)",
    "~/.claude/skills",
    "Custom path",
    "Skip (copy to clipboard instead)"
  )

  choice <- utils::menu(
    choices = scope_choices,
    graphics = FALSE,
    title = "\u276F Where should the skill be installed?"
  )

  if (choice == 0 || choice == 4) {
    install_btw_cli_skill_clipboard()
    return(invisible())
  }

  scope <- switch(
    as.character(choice),
    "1" = "~/.agents/skills",
    "2" = "~/.claude/skills",
    "3" = readline("Enter path: ")
  )

  skill_dir <- system.file("cli-skill", "r-btw-cli", package = "btw")
  tryCatch(
    install_skill_from_dir(skill_dir, scope = scope),
    error = function(e) {
      cli::cli_warn(
        "Failed to install skill: {conditionMessage(e)}"
      )
      install_btw_cli_skill_clipboard()
    }
  )

  invisible()
}

install_btw_cli_skill_clipboard <- function() {
  skill_path <- system.file(
    "cli-skill",
    "r-btw-cli",
    "SKILL.md",
    package = "btw"
  )

  if (!nzchar(skill_path) || !file.exists(skill_path)) {
    cli::cli_warn("Could not find the {.field r-btw-cli} skill file.")
    return(invisible())
  }

  fm <- frontmatter::read_front_matter(skill_path)
  body <- fm$body %||% ""

  write_to_clipboard(body, what = "{.strong r-btw-cli} skill instructions")

  cli::cli_alert_info(
    "Add the copied instructions to your {.file CLAUDE.md}, {.file AGENTS.md}, or other coding agent's context file."
  )

  invisible()
}
