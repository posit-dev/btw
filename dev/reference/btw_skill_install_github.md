# Install a skill from GitHub

Download and install a skill from a GitHub repository. The repository
should contain one or more skill directories, each with a `SKILL.md`
file.

## Usage

``` r
btw_skill_install_github(
  repo,
  skill = NULL,
  scope = "project",
  overwrite = NULL
)
```

## Arguments

- repo:

  GitHub repository in `"owner/repo"` format. Optionally include a Git
  reference (branch, tag, or SHA) as `"owner/repo@ref"`, following the
  convention used by
  [`pak::pak()`](https://pak.r-lib.org/reference/pak.html) and
  [`remotes::install_github()`](https://remotes.r-lib.org/reference/install_github.html).
  Defaults to `"HEAD"` when no ref is specified.

- skill:

  Optional skill name. If `NULL` and the repository contains multiple
  skills, an interactive picker is shown (or an error in non-interactive
  sessions).

- scope:

  Where to install the skill. One of:

  - `"project"` (default): Installs to a project-level skills directory,
    chosen from `.btw/skills/` or `.agents/skills/` in that order. If
    one already exists, it is used; otherwise `.btw/skills/` is created.

  - `"user"`: Installs to the user-level skills directory
    (`tools::R_user_dir("btw", "config")/skills`).

  - A directory path: Installs to a custom directory, e.g.
    `scope = ".openhands/skills"`. Use `I("project")` or `I("user")` if
    you need a literal directory with those names.

- overwrite:

  Whether to overwrite an existing skill with the same name. If `NULL`
  (default), prompts interactively when a conflict exists; in
  non-interactive sessions defaults to `FALSE`, which errors. Set to
  `TRUE` to always overwrite, or `FALSE` to always error on conflict.

## Value

The path to the installed skill directory, invisibly.

## See also

Other skills:
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_package.md),
[`btw_tool_skill()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_skill.md)
