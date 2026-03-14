# Install the btw CLI

Installs the `btw` CLI launcher using
[`Rapp::install_pkg_cli_apps()`](https://rdrr.io/pkg/Rapp/man/install_pkg_cli_apps.html).
[Rapp](https://github.com/r-lib/Rapp) is required to build and install
the CLI. See
[`Rapp::install_pkg_cli_apps()`](https://rdrr.io/pkg/Rapp/man/install_pkg_cli_apps.html)
for details on where the launcher is installed and how to manage it.

## Usage

``` r
install_btw_cli(destdir = NULL, ...)
```

## Arguments

- destdir:

  Directory where the CLI launcher will be installed. If `NULL`, the
  default location used by
  [`Rapp::install_pkg_cli_apps()`](https://rdrr.io/pkg/Rapp/man/install_pkg_cli_apps.html)
  is used.

- ...:

  Arguments passed on to
  [`Rapp::install_pkg_cli_apps`](https://rdrr.io/pkg/Rapp/man/install_pkg_cli_apps.html)

  `lib.loc`

  :   Additional library paths forwarded to
      [`base::system.file()`](https://rdrr.io/r/base/system.file.html)
      while locating package scripts. Discovery happens at install time;
      written launchers embed absolute script paths.

  `overwrite`

  :   Whether to replace an existing executable. `TRUE` always
      overwrites, `FALSE` never overwrites non-Rapp executables, and
      `NA` (the default) prompts interactively and otherwise skips.

## Value

The result of
[`Rapp::install_pkg_cli_apps()`](https://rdrr.io/pkg/Rapp/man/install_pkg_cli_apps.html),
invisibly.

## Details

After installing the CLI, you will be offered the option to install the
`r-btw-cli` skill, which helps AI coding assistants discover and use the
`btw` CLI. If you decline or are in a non-interactive session, the skill
instructions are copied to the clipboard (or printed) so you can add
them to your `CLAUDE.md`, `AGENTS.md`, or other context file manually.
