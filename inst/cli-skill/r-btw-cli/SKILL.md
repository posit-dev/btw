---
name: r-btw-cli
description: "Use the `btw` CLI to access R documentation, manage R package development, query CRAN, and inspect the R environment from the command line. Use when you need to: (1) read R help pages or vignettes, (2) run R CMD check, tests, or devtools::document(), (3) search CRAN for packages, (4) check installed package versions or R platform info."
---

# btw CLI

`btw` is a command-line interface for accessing R documentation, package development, CRAN search, and R environment info.

## Commands

```
btw docs help <topic> [-p <pkg>]     Read an R help page (or list topics for a package)
btw docs vignette <pkg> [-n <name>]  Read a vignette (--list to list available)
btw docs news <pkg> [-s <term>]      Read package NEWS/changelog

btw pkg document [--path <dir>]      Generate roxygen2 docs
btw pkg check [--path <dir>]         Run R CMD check
btw pkg test [-f <filter>] [--path]  Run testthat tests
btw pkg load [--path <dir>]          Load package with pkgload
btw pkg coverage [--file <f>]        Compute test coverage

btw info platform                    R version, OS, locale
btw info packages [<pkg>...]         Installed package details (--check for quick lookup)

btw cran search <query> [-n <count>] Search CRAN for packages
btw cran info <pkg>                  CRAN package details
```

Run `btw --help`, `btw <group> --help`, or `btw <group> <cmd> --help` for full usage details.
