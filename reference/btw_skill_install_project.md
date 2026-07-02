# Install skills from all project dependencies

Discovers R packages that are dependencies of the current project and
installs skills from any that bundle them in `inst/skills/`. If a
`DESCRIPTION` file exists in the working directory, packages are read
from its `Imports` and `Suggests` fields. Otherwise,
[`renv::dependencies()`](https://rstudio.github.io/renv/reference/dependencies.html)
is used as a fallback (requires the renv package).

Packages without skills are silently skipped. If no dependencies bundle
skills, a message is printed and nothing is installed.

## Usage

``` r
btw_skill_install_project(path = ".", scope = "project", overwrite = NULL)
```

## Arguments

- path:

  Path to the project directory. Defaults to the current working
  directory.

- scope:

  Where to install the skills. See
  [`btw_skill_install_package()`](https://posit-dev.github.io/btw/reference/btw_skill_install_package.md)
  for details.

- overwrite:

  Whether to overwrite existing skills. See
  [`btw_skill_install_package()`](https://posit-dev.github.io/btw/reference/btw_skill_install_package.md)
  for details.

## Value

The paths to all installed skill directories, invisibly.

## See also

Other skills:
[`btw_skill_install_github()`](https://posit-dev.github.io/btw/reference/btw_skill_install_github.md),
[`btw_skill_install_package()`](https://posit-dev.github.io/btw/reference/btw_skill_install_package.md),
[`btw_tool_skill()`](https://posit-dev.github.io/btw/reference/btw_tool_skill.md)
