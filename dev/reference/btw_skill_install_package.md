# Install a skill from an R package

Install a skill bundled in an R package. Packages can bundle skills in
their `inst/skills/` directory, where each subdirectory containing a
`SKILL.md` file is a skill.

Note that if a package is attached with
[`library()`](https://rdrr.io/r/base/library.html), its skills are
**automatically available** without installation — btw discovers skills
from all attached packages at runtime. Use this function when you want
to permanently copy a skill to your project or user directory so it
remains available regardless of which packages are loaded.

## Usage

``` r
btw_skill_install_package(
  package,
  skill = NULL,
  scope = "project",
  overwrite = NULL
)
```

## Arguments

- package:

  Name of an installed R package that bundles skills.

- skill:

  Optional skill name. If `NULL` and the package contains multiple
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
[`btw_skill_install_github()`](https://posit-dev.github.io/btw/dev/reference/btw_skill_install_github.md),
[`btw_tool_skill()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_skill.md)
