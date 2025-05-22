mock_pkgsearch <- function(..., format = "long") {
  # pkgsearch::pkg_search("string interpolation", format = "long", size = 3)
  structure(
    list(
      score = c(11496.1045, 1918.5801, 1007.0536),
      package = c("glue", "epoxy", "gsubfn"),
      version = structure(
        list(c(1L, 8L, 0L), c(1L, 0L, 0L), c(0L, 7L)),
        class = c("package_version", "numeric_version")
      ),
      title = c(
        "Interpreted String Literals",
        "String Interpolation for Documents, Reports and Apps",
        "Utilities for Strings and Function Arguments"
      ),
      description = c(
        "An implementation of interpreted string literals, inspired by\nPython's Literal String Interpolation\n<https://www.python.org/dev/peps/pep-0498/> and Docstrings\n<https://www.python.org/dev/peps/pep-0257/> and Julia's Triple-Quoted\nString Literals\n<https://docs.julialang.org/en/v1.3/manual/strings/#Triple-Quoted-String-Literals-1>.",
        "Extra strength 'glue' for data-driven templates. String\ninterpolation for 'Shiny' apps or 'R Markdown' and 'knitr'-powered\n'Quarto' documents, built on the 'glue' and 'whisker' packages.",
        "The gsubfn function is like gsub but can take a replacement\nfunction or certain other objects instead of the replacement string.\nMatches and back references are input to the replacement function and\nreplaced by the function output.   gsubfn can be used to split strings\nbased on content rather than delimiters and for quasi-perl-style string\ninterpolation. The package also has facilities for translating formulas\nto functions and allowing such formulas in function calls instead of\nfunctions.  This can be used with R functions such as apply, sapply,\nlapply, optim, integrate, xyplot, Filter and any other function that\nexpects another function as an input argument or functions like cat\nor sql calls that may involve strings where substitution is desirable.\nThere is also a facility for returning multiple objects from functions\nand a version of transform that allows the RHS to refer to LHS used in\nthe same transform."
      ),
      date = structure(
        c(1727731801, 1695164402, 1521187083),
        class = c("POSIXct", "POSIXt"),
        tzone = "UTC"
      ),
      maintainer_name = c(
        "Jennifer Bryan",
        "Garrick Aden-Buie",
        "G. Grothendieck"
      ),
      maintainer_email = c(
        "jenny@posit.co",
        "garrick@adenbuie.com",
        "ggrothendieck@gmail.com"
      ),
      revdeps = c(909L, 1L, 15L),
      downloads_last_month = c(1488317L, 258L, 19149L),
      license = c("MIT + file LICENSE", "MIT + file LICENSE", "GPL (>= 2)"),
      url = c(
        "https://glue.tidyverse.org/, https://github.com/tidyverse/glue",
        "https://pkg.garrickadenbuie.com/epoxy/,\nhttps://github.com/gadenbuie/epoxy",
        "https://github.com/ggrothendieck/gsubfn"
      ),
      bugreports = c(
        "https://github.com/tidyverse/glue/issues",
        "https://github.com/gadenbuie/epoxy/issues",
        "https://github.com/ggrothendieck/gsubfn/issues"
      ),
      package_data = structure(
        list(
          list(
            `Config/testthat/edition` = "3",
            ByteCompile = "true",
            `Authors@R` = "c(\nperson(\"Jim\", \"Hester\", role = \"aut\",\ncomment = c(ORCID = \"0000-0002-2739-7082\")),\nperson(\"Jennifer\", \"Bryan\", , \"jenny@posit.co\", role = c(\"aut\", \"cre\"),\ncomment = c(ORCID = \"0000-0002-6983-2759\")),\nperson(\"Posit Software, PBC\", role = c(\"cph\", \"fnd\"))\n)",
            Enhances = "",
            Version = "1.8.0",
            RoxygenNote = "7.3.2",
            Encoding = "UTF-8",
            `Config/Needs/website` = "bench, forcats, ggbeeswarm, ggplot2, R.utils,\nrprintf, tidyr, tidyverse/tidytemplate",
            Package = "glue",
            LinkingTo = "",
            Imports = "methods (*), ",
            revdeps = 909L,
            Repository = "CRAN",
            Packaged = "2024-09-27 16:00:45 UTC; jenny",
            VignetteBuilder = "knitr",
            date = "2024-09-30T21:30:01+00:00",
            downloads = 1488317L,
            Title = "Interpreted String Literals",
            Depends = "R (>= 3.6), ",
            crandb_file_date = "2024-09-30 22:47:05",
            Suggests = "DBI (>= 1.2.0), RSQLite (*), vctrs (>= 0.3.0), magrittr (*), rmarkdown (*), knitr (*), dplyr (*), rlang (*), waldo (>= 0.5.3), withr (*), testthat (>= 3.2.0), crayon (*), ",
            Author = "Jim Hester [aut] (<https://orcid.org/0000-0002-2739-7082>),\nJennifer Bryan [aut, cre] (<https://orcid.org/0000-0002-6983-2759>),\nPosit Software, PBC [cph, fnd]",
            MD5sum = "283bba07f61d89e32f9895021bf5099c",
            BugReports = "https://github.com/tidyverse/glue/issues",
            License = "MIT + file LICENSE",
            URL = "https://glue.tidyverse.org/, https://github.com/tidyverse/glue",
            `Date/Publication` = "2024-09-30 22:30:01 UTC",
            Maintainer = "Jennifer Bryan <jenny@posit.co>",
            NeedsCompilation = "yes",
            Description = "An implementation of interpreted string literals, inspired by\nPython's Literal String Interpolation\n<https://www.python.org/dev/peps/pep-0498/> and Docstrings\n<https://www.python.org/dev/peps/pep-0257/> and Julia's Triple-Quoted\nString Literals\n<https://docs.julialang.org/en/v1.3/manual/strings/#Triple-Quoted-String-Literals-1>."
          ),
          list(
            `Config/testthat/edition` = "3",
            `Authors@R` = "c(\nperson(\"Garrick\", \"Aden-Buie\", , \"garrick@adenbuie.com\", role = c(\"aut\", \"cre\"),\ncomment = c(ORCID = \"0000-0002-7111-0077\")),\nperson(\"Kushagra\", \"Gour\", role = \"ctb\",\ncomment = \"hint.css\"),\nperson(\"The mustache.js community\", role = \"ctb\",\ncomment = \"mustache.js\")\n)",
            Enhances = "",
            Version = "1.0.0",
            RoxygenNote = "7.2.3",
            Encoding = "UTF-8",
            `Config/Needs/website` = "rstudio/rmarkdown, gadenbuie/grkgdown",
            Package = "epoxy",
            LinkingTo = "",
            Imports = "whisker (*), lifecycle (*), scales (>= 1.1.0), tools (*), rmarkdown (*), knitr (>= 1.37), purrr (*), rlang (*), and (*), glue (>= 1.5.0), htmltools (*), ",
            revdeps = 1L,
            Repository = "CRAN",
            Packaged = "2023-09-19 18:33:04 UTC; garrick",
            VignetteBuilder = "cleanrmd, knitr, rmarkdown",
            LazyData = "true",
            date = "2023-09-19T23:00:02+00:00",
            downloads = 258L,
            Title = "String Interpolation for Documents, Reports and Apps",
            Depends = "R (>= 3.6.0), ",
            crandb_file_date = "2023-09-20 00:39:39",
            Suggests = "testthat (*), shinytest2 (*), dplyr (*), shiny (*), cleanrmd (*), commonmark (*), dbplyr (*), pandoc (*), debugme (*), ",
            Author = "Garrick Aden-Buie [aut, cre] (<https://orcid.org/0000-0002-7111-0077>),\nKushagra Gour [ctb] (hint.css),\nThe mustache.js community [ctb] (mustache.js)",
            MD5sum = "2f0f8a6877c72c7b1b9118a8abe4b238",
            BugReports = "https://github.com/gadenbuie/epoxy/issues",
            License = "MIT + file LICENSE",
            URL = "https://pkg.garrickadenbuie.com/epoxy/,\nhttps://github.com/gadenbuie/epoxy",
            `Date/Publication` = "2023-09-20 00:00:02 UTC",
            Maintainer = "Garrick Aden-Buie <garrick@adenbuie.com>",
            NeedsCompilation = "no",
            `Config/Needs/rcmdcheck` = "RSQLite, rstudio/chromote",
            Description = "Extra strength 'glue' for data-driven templates. String\ninterpolation for 'Shiny' apps or 'R Markdown' and 'knitr'-powered\n'Quarto' documents, built on the 'glue' and 'whisker' packages."
          ),
          list(
            Enhances = "",
            Version = "0.7",
            Package = "gsubfn",
            LinkingTo = "",
            revdeps = 15L,
            Imports = "",
            Date = "2018-03-15",
            Repository = "CRAN",
            Packaged = "2018-03-15 23:17:54 UTC; Louis",
            date = "2018-03-16T07:58:03+00:00",
            downloads = 19149L,
            Title = "Utilities for Strings and Function Arguments",
            Depends = "proto (*), ",
            crandb_file_date = "2018-03-16 09:02:27",
            Author = "G. Grothendieck",
            Suggests = "svUnit (*), boot (*), tcltk (*), zoo (*), grid (*), chron (*), quantreg (*), lattice (*), ",
            MD5sum = "7c3ac3d87b367aaf06a8add353fb162a",
            BugReports = "https://github.com/ggrothendieck/gsubfn/issues",
            License = "GPL (>= 2)",
            URL = "https://github.com/ggrothendieck/gsubfn",
            `Date/Publication` = "2018-03-16 08:58:03 UTC",
            Maintainer = "G. Grothendieck <ggrothendieck@gmail.com>",
            NeedsCompilation = "no",
            Description = "The gsubfn function is like gsub but can take a replacement\nfunction or certain other objects instead of the replacement string.\nMatches and back references are input to the replacement function and\nreplaced by the function output.   gsubfn can be used to split strings\nbased on content rather than delimiters and for quasi-perl-style string\ninterpolation. The package also has facilities for translating formulas\nto functions and allowing such formulas in function calls instead of\nfunctions.  This can be used with R functions such as apply, sapply,\nlapply, optim, integrate, xyplot, Filter and any other function that\nexpects another function as an input argument or functions like cat\nor sql calls that may involve strings where substitution is desirable.\nThere is also a facility for returning multiple objects from functions\nand a version of transform that allows the RHS to refer to LHS used in\nthe same transform."
          )
        ),
        class = "AsIs"
      )
    ),
    class = c("pkg_search_result", "tbl", "data.frame"),
    row.names = c(NA, -3L),
    metadata = list(
      query = "string interpolation",
      format = format,
      from = 1,
      size = 3,
      server = "https://search.r-pkg.org",
      total = 337L,
      max_score = 11496.1045,
      took = 141L,
      timed_out = FALSE
    )
  )
}

mock_cran_package <- function(...) {
  structure(
    list(
      Package = "anyflights",
      Title = "Query 'nycflights13'-Like Air Travel Data for Given Years and\nAirports",
      Version = "0.3.5",
      `Authors@R` = "c(\nperson(\"Simon P.\", \"Couch\", , \"simonpatrickcouch@gmail.com\", c(\"aut\", \"cre\")),\nperson(\"Hadley\", \"Wickham\", , \"hadley@rstudio.com\", \"ctb\"),\nperson(\"Jay\", \"Lee\", , \"jaylee@reed.edu\", \"ctb\"),\nperson(\"Dennis\", \"Irorere\", , \"denironyx@gmail.com\", \"ctb\")\n)",
      Description = "Supplies a set of functions to query air travel data for user-\nspecified years and airports. Datasets include on-time flights, airlines,\nairports, planes, and weather.",
      License = "CC0",
      Depends = list(R = ">= 3.5.0"),
      Imports = list(
        httr = "*",
        dplyr = "*",
        readr = "*",
        utils = "*",
        lubridate = "*",
        vroom = "*",
        glue = "*",
        purrr = "*",
        stringr = "*",
        curl = "*",
        usethis = "*",
        roxygen2 = "*",
        progress = "*",
        tidyr = "*"
      ),
      URL = "https://github.com/simonpcouch/anyflights,\nhttps://simonpcouch.github.io/anyflights/",
      BugReports = "https://github.com/simonpcouch/anyflights/issues",
      RoxygenNote = "7.3.2",
      Encoding = "UTF-8",
      Suggests = list(
        testthat = "*",
        nycflights13 = "*",
        covr = "*"
      ),
      NeedsCompilation = "no",
      Packaged = "2025-01-10 19:42:22 UTC; simoncouch",
      Author = "Simon P. Couch [aut, cre],\nHadley Wickham [ctb],\nJay Lee [ctb],\nDennis Irorere [ctb]",
      Maintainer = "Simon P. Couch <simonpatrickcouch@gmail.com>",
      Repository = "CRAN",
      `Date/Publication` = "2025-01-10 20:30:02 UTC",
      crandb_file_date = "2025-01-10 20:46:55",
      MD5sum = "ba62eeb4c481153e634313042c4274cf",
      date = "2025-01-10T19:30:02+00:00",
      releases = list()
    ),
    class = "cran_package"
  )
}
