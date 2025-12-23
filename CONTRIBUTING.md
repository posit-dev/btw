# Contributing to btw

We welcome contributions to **btw**! This guide explains how to propose
a change.

## Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://posit-dev.github.io/btw/CODE_OF_CONDUCT.md). By
participating in this project you agree to abide by its terms.

## Before you start

**Always file an issue first.** Before making any pull request, open an
issue to discuss your proposed change. This helps ensure:

- Your work aligns with the project’s goals
- You’re not duplicating effort
- Maintainers can provide early feedback

You do not need to open an issue first for small and simple changes,
like fixing typos.

## Filing issues

When reporting a bug or suggesting a feature:

- Provide a clear description
- Include a minimal reproducible example for bugs – the
  [reprex](https://reprex.tidyverse.org/) package is a great tool for
  this!
- Describe your environment (R version, OS, etc.)
- Be sure to provide as much detail as possible about which AI provider
  and model you are using

See our guide on [how to create a great
issue](https://code-review.tidyverse.org/issues/) for more advice.

## Making changes

1.  [Fork](https://github.com/posit-dev/btw/fork) the repository
2.  Create a branch off of `main` for your work (don’t use `main`
    directly!)
3.  Make your changes following the [tidyverse style
    guide](https://style.tidyverse.org/)
4.  Style your code with [air](https://posit-dev.github.io/air/)

### Before submitting a pull request

- **Add tests** for new functionality using testthat
- **Update documentation** with roxygen2 for user-facing changes
- **Add a NEWS.md entry** describing your change
- **Run
  [`devtools::check()`](https://devtools.r-lib.org/reference/check.html)**
  to ensure no errors, warnings, or notes
- **Keep commits clean** with clear, descriptive messages

## Submitting a pull request

1.  Push your branch to your fork
2.  Open a pull request linking to the related issue
    - Use `Fixes #123` to auto-close the parent issue
3.  Provide a clear description of what you changed and why
4.  Be responsive to feedback from maintainers

## Questions?

Open an issue if you have questions—we’re happy to help!
