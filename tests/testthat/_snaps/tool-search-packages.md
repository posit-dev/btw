# btw_tool_search_packages() snapshots

    Code
      cli::cat_line(btw_tool_search_packages("string interpolation", format = "long")@
        value)
    Output
      Found 337 packages matching `string interpolation`, showing 5 results.
      
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
      
      ### stringmagic (v1.2.0) -- Character String Operations and Interpolation, Magic Edition
      
      * Maintainer: Laurent R Berge
      * Homepage: https://lrberge.github.io/stringmagic/, https://github.com/lrberge/stringmagic
      * Date: 2025-04-18
      * Downloads Last Month:         1
      
      Performs complex string operations compactly and efficiently. Supports string interpolation jointly with over 50 string operations. Also enhances regular string functions (like grep() and co). See an introduction at <https://lrberge.github.io/stringmagic/>.
      
      ### headliner (v0.0.3) -- Compose Sentences to Describe Comparisons
      
      * Maintainer: Jake Riley
      * Homepage: https://rjake.github.io/headliner/, https://github.com/rjake/headliner/
      * Date: 2022-12-20
      * Downloads Last Month:       181
      
      Create dynamic, data-driven text. Given two values, a list of
      talking points is generated and can be combined using string
      interpolation. Based on the 'glue' package.

---

    Code
      cli::cat_line(btw_tool_search_packages("string interpolation", format = "short")@
        value)
    Output
      Found 337 packages matching `string interpolation`, showing 10 results.
      
      | package | title | version | date | url | downloads_last_month |
      |---------|-------|---------|------|-----|----------------------|
      | glue | Interpreted String Literals | 1.8.0 | 2024-09-30 | https://glue.tidyverse.org/, https://github.com/tidyverse/glue | 1,488,317 |
      | epoxy | String Interpolation for Documents, Reports and Apps | 1.0.0 | 2023-09-19 | https://pkg.garrickadenbuie.com/epoxy/, https://github.com/gadenbuie/epoxy |       258 |
      | gsubfn | Utilities for Strings and Function Arguments | 0.7 | 2018-03-16 | https://github.com/ggrothendieck/gsubfn |    19,149 |
      | stringmagic | Character String Operations and Interpolation, Magic Edition | 1.2.0 | 2025-04-18 | https://lrberge.github.io/stringmagic/, https://github.com/lrberge/stringmagic |         1 |
      | headliner | Compose Sentences to Describe Comparisons | 0.0.3 | 2022-12-20 | https://rjake.github.io/headliner/, https://github.com/rjake/headliner/ |       181 |
      | stringi | Fast and Portable Character String Processing Facilities | 1.8.7 | 2025-03-27 | https://stringi.gagolewski.com/, https://github.com/gagolews/stringi, https://icu.unicode.org/ | 1,170,101 |
      | stringr | Simple, Consistent Wrappers for Common String Operations | 1.5.1 | 2023-11-14 | https://stringr.tidyverse.org, https://github.com/tidyverse/stringr | 1,139,411 |
      | tidyr | Tidy Messy Data | 1.3.1 | 2024-01-24 | https://tidyr.tidyverse.org, https://github.com/tidyverse/tidyr | 1,098,165 |
      | tidyselect | Select from a Set of Strings | 1.2.1 | 2024-03-11 | https://tidyselect.r-lib.org, https://github.com/r-lib/tidyselect | 1,110,863 |
      | sqlhelper | Easier 'SQL' Integration | 0.2.1 | 2024-01-21 | https://majerr.github.io/sqlhelper/dev/, https://github.com/majerr/sqlhelper/ |       130 |

