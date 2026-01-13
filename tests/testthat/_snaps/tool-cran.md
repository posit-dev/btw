# btw_tool_search_packages() snapshots

    Code
      cli::cat_line(btw_tool_search_packages("string interpolation", format = "long")@
        value)
    Condition
      Warning:
      `btw_tool_search_packages()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_cran_search()` instead.
    Output
      Found 337 packages matching `string interpolation`, showing 3 results.
      
      ### glue (v1.8.0) -- Interpreted String Literals
      
      * Maintainer: Jennifer Bryan
      * Homepage: https://glue.tidyverse.org/, https://github.com/tidyverse/glue
      * Date: 2024-09-30
      * Downloads Last Month: 1,488,317
      
      An implementation of interpreted string literals, inspired by
      Python's Literal String Interpolation
      <https://www.python.org/dev/peps/pep-0498/> and Docstrings
      <https://www.python.org/dev/peps/pep-0257/> and Julia's Triple-Quoted
      String Literals
      <https://docs.julialang.org/en/v1.3/manual/strings/#Triple-Quoted-String-Literals-1>.
      
      ### epoxy (v1.0.0) -- String Interpolation for Documents, Reports and Apps
      
      * Maintainer: Garrick Aden-Buie
      * Homepage: https://pkg.garrickadenbuie.com/epoxy/, https://github.com/gadenbuie/epoxy
      * Date: 2023-09-19
      * Downloads Last Month:       258
      
      Extra strength 'glue' for data-driven templates. String
      interpolation for 'Shiny' apps or 'R Markdown' and 'knitr'-powered
      'Quarto' documents, built on the 'glue' and 'whisker' packages.
      
      ### gsubfn (v0.7) -- Utilities for Strings and Function Arguments
      
      * Maintainer: G. Grothendieck
      * Homepage: https://github.com/ggrothendieck/gsubfn
      * Date: 2018-03-16
      * Downloads Last Month:    19,149
      
      The gsubfn function is like gsub but can take a replacement
      function or certain other objects instead of the replacement string.
      Matches and back references are input to the replacement function and
      replaced by the function output.   gsubfn can be used to split strings
      based on content rather than delimiters and for quasi-perl-style string
      interpolation. The package also has facilities for translating formulas
      to functions and allowing such formulas in function calls instead of
      functions.  This can be used with R functions such as apply, sapply,
      lapply, optim, integrate, xyplot, Filter and any other function that
      expects another function as an input argument or functions like cat
      or sql calls that may involve strings where substitution is desirable.
      There is also a facility for returning multiple objects from functions
      and a version of transform that allows the RHS to refer to LHS used in
      the same transform.

---

    Code
      cli::cat_line(btw_tool_search_packages("string interpolation", format = "short")@
        value)
    Condition
      Warning:
      `btw_tool_search_packages()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_cran_search()` instead.
    Output
      Found 337 packages matching `string interpolation`, showing 3 results.
      
      | package | title | version | date | url | downloads_last_month |
      |---------|-------|---------|------|-----|----------------------|
      | glue | Interpreted String Literals | 1.8.0 | 2024-09-30 | https://glue.tidyverse.org/, https://github.com/tidyverse/glue | 1,488,317 |
      | epoxy | String Interpolation for Documents, Reports and Apps | 1.0.0 | 2023-09-19 | https://pkg.garrickadenbuie.com/epoxy/, https://github.com/gadenbuie/epoxy |       258 |
      | gsubfn | Utilities for Strings and Function Arguments | 0.7 | 2018-03-16 | https://github.com/ggrothendieck/gsubfn |    19,149 |

# btw_tool_search_package_info() snapshots

    Code
      cli::cat_line(btw_tool_search_package_info("anyflights")@value)
    Condition
      Warning:
      `btw_tool_search_package_info()` was deprecated in btw 1.2.0.
      i Please use `btw_tool_cran_package()` instead.
    Output
      ### anyflights (v0.3.5) -- Query 'nycflights13'-Like Air Travel Data for Given Years and
      Airports
      
      #### Description
      
      Supplies a set of functions to query air travel data for user-
      specified years and airports. Datasets include on-time flights, airlines,
      airports, planes, and weather.
      
      #### Details
      
      * License: CC0
      * Home: https://github.com/simonpcouch/anyflights,https://simonpcouch.github.io/anyflights/
      * Issue Tracker: https://github.com/simonpcouch/anyflights/issues
      * Last Updated: 2025-01-10
      
      #### Dependencies
      
      * Depends
        * R >= 3.5.0
      * Imports
        * httr
        * dplyr
        * readr
        * utils
        * lubridate
        * vroom
        * glue
        * purrr
        * stringr
        * curl
        * usethis
        * roxygen2
        * progress
        * tidyr
      * Suggests
        * testthat
        * nycflights13
        * covr
      
      #### Author Information
      
      Simon P. Couch [aut, cre],
      Hadley Wickham [ctb],
      Jay Lee [ctb],
      Dennis Irorere [ctb]
      
      **Maintainer**: Simon P. Couch <simonpatrickcouch@gmail.com>

---

    Code
      cli::cat_line(btw_this(mock_cran_package("anyflights")))
    Output
      ### anyflights (v0.3.5) -- Query 'nycflights13'-Like Air Travel Data for Given Years and
      Airports
      
      #### Description
      
      Supplies a set of functions to query air travel data for user-
      specified years and airports. Datasets include on-time flights, airlines,
      airports, planes, and weather.
      
      #### Details
      
      * License: CC0
      * Home: https://github.com/simonpcouch/anyflights,https://simonpcouch.github.io/anyflights/
      * Issue Tracker: https://github.com/simonpcouch/anyflights/issues
      * Last Updated: 2025-01-10
      
      #### Dependencies
      
      * Depends
        * R >= 3.5.0
      * Imports
        * httr
        * dplyr
        * readr
        * utils
        * lubridate
        * vroom
        * glue
        * purrr
        * stringr
        * curl
        * usethis
        * roxygen2
        * progress
        * tidyr
      * Suggests
        * testthat
        * nycflights13
        * covr
      
      #### Author Information
      
      Simon P. Couch [aut, cre],
      Hadley Wickham [ctb],
      Jay Lee [ctb],
      Dennis Irorere [ctb]
      
      **Maintainer**: Simon P. Couch <simonpatrickcouch@gmail.com>

