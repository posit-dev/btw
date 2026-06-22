---
name: r-btw-cli
description: "Use the `btw` CLI to access R documentation, manage R package development, query CRAN, inspect the R environment, and discover or fetch skills from the command line. Use when you need to: (1) read R help pages or vignettes, (2) run R CMD check, tests, or devtools::document(), (3) search CRAN for packages, (4) check installed package versions or R platform info, (5) list or fetch skills from R packages or GitHub repositories."
---

# btw CLI

`btw` is a command-line interface for accessing R documentation, package development, CRAN search, and R environment info.

## When to use which command

- Use `btw check-installed` to quickly verify whether a package is installed.
- Use `btw docs` to read documentation for a locally-installed package.
- Use `btw cran` only to search for packages or retrieve basic CRAN metadata (version, description, dependencies); do not use it as a substitute for `btw docs` when the package is available locally.

## Commands

Use `btw docs` to read R help pages, vignettes, and package NEWS for locally installed packages.

```
btw docs topics <pkg> [--only help|vignettes] [--json]  List help topics and vignettes for a package
btw docs help <topic> [-p <pkg>]                        Read an R help page
btw docs help <pkg>::<topic>                            Read a specific help page (scoped)
btw docs vignette <pkg> [-n <name>]                     Read a vignette (--list to list available)
btw docs news <pkg> [-s <term>]                         Read package NEWS/changelog
```

Use `btw pkg` to run development tasks on an R package under active development.

```
btw pkg document [--path <dir>]            Generate roxygen2 docs
btw pkg check [--path <dir>]               Run R CMD check
btw pkg test [-f <filter>] [--path <dir>]  Run testthat tests
btw pkg load [--path <dir>]                Load package with pkgload
btw pkg coverage [--file <f>] [--json]     Compute test coverage
```

Use these commands to inspect the current R installation and installed packages. Use `btw check-installed` to verify whether a specific package is installed before deciding whether to use `btw docs` or `btw cran`.

```
btw system-info [--json]                             R version, OS, locale
btw check-installed <pkg>... [--fail] [--json]       Check if packages are installed
btw installed-packages <pkg>... [--deps <types>] [--json]  Installed package details
```

Use `btw cran` to search CRAN for packages or retrieve basic metadata (version, description, dependencies) for a package. Prefer `btw docs` over `btw cran` for reading documentation when the package is installed locally.

```
btw cran search <query> [-n <count>] [--json]  Search CRAN for packages
btw cran info <pkg> [--json]                   CRAN package details
```

Use `btw skills` to discover and fetch skills from R packages or GitHub repositories.

```
btw skills list <source> [--json]                        List available skills
btw skills get <source> [names...] [--all]               Fetch skill content (list if no names given)
btw skills resource <source> <name> <paths...>           Fetch resource files bundled with a skill
btw skills install <source> [--skill <name>] [--scope]   Install a skill locally
```

`<source>` is a package name (e.g. `btw`) or a GitHub repo spec (e.g. `posit-dev/btw`).

Run `btw --help`, `btw <group> --help`, or `btw <group> <cmd> --help` for full usage details, including the field names and types returned by `--json` flags.