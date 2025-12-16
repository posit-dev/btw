# btw_tool_session_platform_info() works

    Code
      cat(platform_description@value)
    Output
      <system_info>
      R_VERSION: R VERSION
      OS: OPERATING SYSTEM
      SYSTEM: SYSTEM VERSION
      UI: Positron (a VS Code equivalent)
      LANGUAGE: es
      LOCALE: C
      ENCODING: LC_CTYPE
      TIMEZONE: Europe/Madrid
      DATE: DAY OF WEEK, MONTH DAY, YEAR (YYYY-MM-DD)
      </system_info>

# btw_tool_session_package_info()

    Code
      cat(btw_tool_session_package_info("dplyr")@value)
    Output
      ```
       package    * version date (UTC) lib source
       cli          3.6.4   2025-02-13 [1] CRAN (R 4.4.1)
       dplyr        1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
       fansi        1.0.6   2023-12-08 [1] CRAN (R 4.4.0)
       generics     0.1.3   2022-07-05 [1] CRAN (R 4.4.0)
       glue         1.8.0   2024-09-30 [1] CRAN (R 4.4.1)
       lifecycle    1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
       magrittr     2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
       pillar       1.10.1  2025-01-07 [1] CRAN (R 4.4.1)
       pkgconfig    2.0.3   2019-09-22 [1] CRAN (R 4.4.0)
       R6           2.6.1   2025-02-15 [1] CRAN (R 4.4.1)
       rlang        1.1.5   2025-01-17 [1] CRAN (R 4.4.1)
       tibble       3.2.1   2023-03-20 [1] CRAN (R 4.4.0)
       tidyselect   1.2.1   2024-03-11 [1] CRAN (R 4.4.0)
       utf8         1.2.4   2023-10-22 [1] CRAN (R 4.4.0)
       vctrs        0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
       withr        3.0.2   2024-10-28 [1] CRAN (R 4.4.1)
      
       [1] /Users/garrick/Library/R/arm64/4.4/library
       [2] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
      ```

---

    Code
      cat(btw_tool_session_package_info("digest", c("Imports", "Suggests"))@value)
    Output
      ```
       ! package         * version date (UTC) lib source
         digest            0.6.37  2024-08-19 [1] CRAN (R 4.4.1)
       X simplermarkdown   <NA>    <NA>       [?] <NA>
       X tinytest          <NA>    <NA>       [?] <NA>
      
       [1] /Users/garrick/Library/R/arm64/4.4/library
       [2] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
      
       X -- Package is not installed
      ```

---

    Code
      cat(btw_tool_session_package_info("attached")@value)
    Output
      ### Attached Packages
      
      ```
       ! package  * version    date (UTC) lib source
       P btw      * 0.0.0.9001 2025-03-21 [?] Github (posit-dev/btw@cfb84d4)
         devtools * 2.4.5      2022-10-11 [1] CRAN (R 4.4.0)
         testthat * 3.2.3      2025-01-30 [1] Github (r-lib/testthat@50a99ec)
         usethis  * 2.2.3      2024-02-19 [1] CRAN (R 4.4.0)
      
       [1] /Users/garrick/Library/R/arm64/4.4/library
       [2] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
      
       * -- Packages attached to the search path.
       P -- Loaded and on-disk path mismatch.
      ```

---

    Code
      cat(btw_tool_session_package_info("loaded")@value)
    Output
      ### Loaded Packages
      
      ```
       ! package     * version     date (UTC) lib source
         base64enc     0.1-3       2015-07-28 [1] CRAN (R 4.4.0)
         brio          1.1.5       2024-04-24 [1] CRAN (R 4.4.0)
       P btw         * 0.0.0.9001  2025-03-21 [?] Github (posit-dev/btw@cfb84d4)
         cachem        1.1.0       2024-05-16 [1] CRAN (R 4.4.0)
         cli           3.6.4       2025-02-13 [1] CRAN (R 4.4.1)
         clipr         0.8.0       2022-02-22 [1] CRAN (R 4.4.0)
         coro          1.1.0       2024-11-05 [1] CRAN (R 4.4.1)
         desc          1.4.3       2023-12-10 [1] CRAN (R 4.4.0)
         devtools    * 2.4.5       2022-10-11 [1] CRAN (R 4.4.0)
         digest        0.6.37      2024-08-19 [1] CRAN (R 4.4.1)
         dplyr         1.1.4       2023-11-17 [1] CRAN (R 4.4.0)
         ellipsis      0.3.2       2021-04-29 [1] CRAN (R 4.4.0)
         ellmer        0.1.1       2025-02-07 [1] CRAN (R 4.4.1)
         evaluate      1.0.3       2025-01-10 [1] CRAN (R 4.4.2)
         fastmap       1.2.0       2024-05-15 [1] CRAN (R 4.4.0)
         fs            1.6.5       2024-10-30 [1] CRAN (R 4.4.1)
         generics      0.1.3       2022-07-05 [1] CRAN (R 4.4.0)
         glue          1.8.0       2024-09-30 [1] CRAN (R 4.4.1)
         grkstyle      0.2.1       2024-03-21 [1] https://g~
         htmltools     0.5.8.1     2024-04-04 [1] CRAN (R 4.4.0)
         htmlwidgets   1.6.4       2023-12-06 [1] CRAN (R 4.4.0)
         httpuv        1.6.15      2024-03-26 [1] CRAN (R 4.4.0)
         httr2         1.1.1       2025-03-08 [1] CRAN (R 4.4.1)
         jsonlite      1.9.1       2025-03-03 [1] CRAN (R 4.4.1)
         knitr         1.50        2025-03-16 [1] CRAN (R 4.4.1)
         later         1.4.1       2024-11-27 [1] CRAN (R 4.4.1)
         lifecycle     1.0.4       2023-11-07 [1] CRAN (R 4.4.0)
         magrittr      2.0.3       2022-03-30 [1] CRAN (R 4.4.0)
         memoise       2.0.1       2021-11-26 [1] CRAN (R 4.4.0)
         mime          0.13        2025-03-17 [1] CRAN (R 4.4.1)
         miniUI        0.1.1.1     2018-05-18 [1] CRAN (R 4.4.0)
         pillar        1.10.1      2025-01-07 [1] CRAN (R 4.4.1)
         pkgbuild      1.4.6       2025-01-16 [1] CRAN (R 4.4.2)
         pkgconfig     2.0.3       2019-09-22 [1] CRAN (R 4.4.0)
         pkgload       1.4.0       2024-06-28 [1] CRAN (R 4.4.0)
         praise        1.0.0       2015-08-11 [1] CRAN (R 4.4.0)
         profvis       0.3.8       2023-05-02 [1] CRAN (R 4.4.0)
         promises      1.3.2       2024-11-28 [1] CRAN (R 4.4.1)
         purrr         1.0.4       2025-02-05 [1] CRAN (R 4.4.1)
         R6            2.6.1       2025-02-15 [1] CRAN (R 4.4.1)
         rappdirs      0.3.3       2021-01-31 [1] CRAN (R 4.4.0)
         Rcpp          1.0.14      2025-01-12 [1] CRAN (R 4.4.2)
         remotes       2.5.0.9000  2025-02-03 [1] Github (MangoTheCat/remotes@39a6a81)
         repr          1.1.7       2024-03-22 [1] CRAN (R 4.4.0)
         rlang         1.1.5       2025-01-17 [1] CRAN (R 4.4.1)
         rmarkdown     2.29        2024-11-04 [1] any (@2.29)
         roxygen2      7.3.2       2024-06-28 [1] CRAN (R 4.4.0)
         rprojroot     2.0.4       2023-11-05 [1] CRAN (R 4.4.0)
         rsthemes      0.4.0       2024-03-13 [1] https://g~
         rstudioapi    0.17.1      2024-10-22 [1] CRAN (R 4.4.1)
         S7            0.2.0       2024-11-07 [1] CRAN (R 4.4.1)
         sessioninfo   1.2.3       2025-02-05 [1] CRAN (R 4.4.1)
         shiny         1.10.0.9000 2025-02-26 [1] Github (rstudio/shiny@531f31b)
         shrtcts       0.1.2.9000  2025-01-22 [1] https://g~
         skimr         2.1.5       2022-12-23 [1] CRAN (R 4.4.0)
         stringi       1.8.4       2024-05-06 [1] CRAN (R 4.4.0)
         stringr       1.5.1       2023-11-14 [1] CRAN (R 4.4.0)
         testthat    * 3.2.3       2025-01-30 [1] Github (r-lib/testthat@50a99ec)
         tibble        3.2.1       2023-03-20 [1] CRAN (R 4.4.0)
         tidyselect    1.2.1       2024-03-11 [1] CRAN (R 4.4.0)
         urlchecker    1.0.1       2021-11-30 [1] CRAN (R 4.4.0)
         usethis     * 2.2.3       2024-02-19 [1] CRAN (R 4.4.0)
         vctrs         0.6.5       2023-12-01 [1] CRAN (R 4.4.0)
         withr         3.0.2       2024-10-28 [1] CRAN (R 4.4.1)
         xfun          0.51        2025-02-19 [1] CRAN (R 4.4.2)
         xml2          1.3.8       2025-03-14 [1] CRAN (R 4.4.1)
         xtable        1.8-4       2019-04-21 [1] CRAN (R 4.4.0)
      
       [1] /Users/garrick/Library/R/arm64/4.4/library
       [2] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
      
       * -- Packages attached to the search path.
       P -- Loaded and on-disk path mismatch.
      ```

---

    Code
      cat(btw_tool_session_package_info("dplyr,tidyr", "false")@value)
    Output
      ```
       package * version date (UTC) lib source
       dplyr     1.1.4   2023-11-17 [1] CRAN (R 4.4.0)
       tidyr     1.3.1   2024-01-24 [1] CRAN (R 4.4.0)
      
       [1] /Users/garrick/Library/R/arm64/4.4/library
       [2] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library
      ```

