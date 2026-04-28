---
name: r-btw-cli
description: "Use the `btw` CLI to access R documentation, manage R package development, query CRAN, and inspect the R environment from the command line. Use when you need to: (1) read R help pages or vignettes, (2) run R CMD check, tests, or devtools::document(), (3) search CRAN for packages, (4) check installed package versions or R platform info."
---

# btw CLI

`btw` is a command-line interface for accessing R documentation, package development, CRAN search, and R environment info.

## Commands

Use `btw docs` to read R help pages, vignettes, and package NEWS without starting an interactive R session.

```
btw docs help <topic> [-p <pkg>]     Read an R help page (tries topic first, falls back to package listing)
btw docs help {<pkg>}                List help topics for a package
btw docs help <pkg>::<topic>         Read a specific help page (scoped)
btw docs vignette <pkg> [-n <name>]  Read a vignette (--list to list available)
btw docs news <pkg> [-s <term>]      Read package NEWS/changelog
```

Use `btw pkg` to run development tasks on an R package under active development.

```
btw pkg document [--path <dir>]            Generate roxygen2 docs
btw pkg check [--path <dir>]               Run R CMD check
btw pkg test [-f <filter>] [--path <dir>]  Run testthat tests
btw pkg load [--path <dir>]                Load package with pkgload
btw pkg coverage [--file <f>] [--json]     Compute test coverage
```

Use `btw info` to inspect the current R installation and installed packages.

```
btw info platform [--json]             R version, OS, locale
btw info packages [<pkg>...] [--json]  Installed package details (--check for quick lookup, --deps for dep types)
```

Use `btw cran` to search for packages or look up metadata for a known package on CRAN.

```
btw cran search <query> [-n <count>] [--json]  Search CRAN for packages
btw cran info <pkg> [--json]                   CRAN package details
```

Run `btw --help`, `btw <group> --help`, or `btw <group> <cmd> --help` for full usage details.
