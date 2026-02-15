# btw_tool_docs_package_news() snapshots

    Code
      cat(btw("@news dplyr")@text)
    Output
      ## Context
      
      ### dplyr v1.1.4
      
      - `join_by()` now allows its helper functions to be namespaced with
        `dplyr::`, like `join_by(dplyr::between(x, lower, upper))` (#6838).
      
      - `left_join()` and friends now return a specialized error message if
        they detect that your join would return more rows than dplyr can
        handle (#6912).
      
      - `slice_*()` now throw the correct error if you forget to name `n`
        while also prefixing the call with `dplyr::` (#6946).
      
      - `dplyr_reconstruct()`\'s default method has been rewritten to avoid
        materializing duckplyr queries too early (#6947).
      
      - Updated the `storms` data to include 2022 data (#6937, \@steveharoz).
      
      - Updated the `starwars` data to use a new API, because the old one is
        defunct. There are very minor changes to the data itself (#6938,
        \@steveharoz).

---

    Code
      cat(btw("@news dplyr join")@text)
    Output
      ## Context
      
      ### dplyr v1.1.4
      
      `join_by()` now allows its helper functions to be namespaced with
      `dplyr::`, like `join_by(dplyr::between(x, lower, upper))` (#6838).
      
      `left_join()` and friends now return a specialized error message if they
      detect that your join would return more rows than dplyr can handle
      (#6912).

