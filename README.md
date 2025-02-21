
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btw

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/btw)](https://CRAN.R-project.org/package=btw)
[![R-CMD-check](https://github.com/simonpcouch/btw/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/simonpcouch/btw/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/simonpcouch/btw/graph/badge.svg)](https://app.codecov.io/gh/simonpcouch/btw)
<!-- badges: end -->

btw helps you describe your computational environment to LLMs.

- When used **interactively**, `btw::btw()` assembles context on your
  global R environment as well as function or package documentation,
  copying the results to your clipboard for easy pasting into chat
  interfaces.
- The `btw()` function wraps several lower-level functions that can be
  easily incorporated into **ellmer tool calls** for describing various
  kinds of objects in R. To equip your ellmer chat with the ability to
  peruse documentation and check out the objects in your R environment,
  pass your chat to `register_btw_tools()`.

## Installation

You can install the development version of btw like so:

``` r
pak::pak("simonpcouch/btw")
```

## Example

``` r
library(btw)
```

The easiest way to interact with btw is to just call `btw()` with no
inputs, which will describe the objects in your global environment in a
string attached to your clipboard:

``` r
btw()
```

    ✔ btw copied to the clipboard!

The package allows you to describe all sorts of objects, though. For
example, if you’d like to describe the mtcars dataset:

``` r
btw(mtcars)
```

The following would be attached to your clipboard:

    mtcars
    #> $ mpg  <dbl> 21.0, 21.0, 22.8, 21.4, 18.7
    #> $ cyl  <dbl> 6, 6, 4, 6, 8
    #> $ disp <dbl> 160, 160, 108, 258, 360
    #> $ hp   <dbl> 110, 110, 93, 110, 175
    #> $ drat <dbl> 3.90, 3.90, 3.85, 3.08, 3.15
    #> $ wt   <dbl> 2.620, 2.875, 2.320, 3.215, 3.440
    #> $ qsec <dbl> 16.46, 17.02, 18.61, 19.44, 17.02
    #> $ vs   <dbl> 0, 0, 1, 1, 0
    #> $ am   <dbl> 1, 1, 1, 0, 0
    #> $ gear <dbl> 4, 4, 4, 3, 3
    #> $ carb <dbl> 4, 4, 1, 1, 2

Or, to describe the `btw()` function:

``` r
btw(btw::btw)
```

The following would be attached to your clipboard:

    btw::btw
    #> btw                    package:btw                     R Documentation

    Plain-text descriptions of R objects

    Description:

         This function allows you to quickly describe your computational
         environment to a model by concatenating plain-text descriptions of
         "R stuff", from data frames to packages to function documentation.

    Usage:

         btw(..., clipboard = interactive())
         
    Arguments:

         ...: Objects to describe from your R environment. You can pass
              objects themselves, like data frames or functions, or the
              function also accepts output from get_*() functions like
              'get_package_help()', 'get_help_page()', etc. If omitted,
              this function will just describe the elements in your global
              R environment.

    clipboard: Whether to write the results to the clipboard. A single
              logical value; will default to 'TRUE' when run interactively.

    Value:

         The combined elements as a string, invisibly. If 'clipboard' is
         'TRUE', the result is also written to the system clipboard.

    Examples:

         btw()
         
         btw(mtcars)
         
         btw(btw::btw)
         
