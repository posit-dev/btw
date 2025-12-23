# Describe objects

Character strings in
[`btw_this()`](https://posit-dev.github.io/btw/reference/btw_this.md)
are used as shortcuts to many underlying methods.
[`btw_this()`](https://posit-dev.github.io/btw/reference/btw_this.md)
detects specific formats in the input string to determine which method
to call, or by default it will try to evaluate the character string as R
code and return the appropriate object description.

[`btw_this()`](https://posit-dev.github.io/btw/reference/btw_this.md)
knows about the following special character string formats:

- `"./path"`  
  Any string starting with `./` is treated as a relative path. If the
  path is a file, we call
  [`btw_tool_files_read_text_file()`](https://posit-dev.github.io/btw/reference/btw_tool_files_read_text_file.md)
  and if the path is a directory we call
  [`btw_tool_files_list_files()`](https://posit-dev.github.io/btw/reference/btw_tool_files_list_files.md)
  on the path.

  - `btw_this("./data")` lists the files in `data/`.

  - `btw_this("./R/load_data.R")` reads the source of the
    `R/load_data.R` file.

- `"{pkgName}"` or `"@pkg pkgName"`  
  A package name wrapped in braces, or using the `@pkg` command. Returns
  the list of help topics
  ([`btw_tool_docs_package_help_topics()`](https://posit-dev.github.io/btw/reference/btw_tool_package_docs.md))
  and, if it exists, the introductory vignette for the package
  ([`btw_tool_docs_vignette()`](https://posit-dev.github.io/btw/reference/btw_tool_package_docs.md)).

  - `btw_this("{dplyr}")` or `btw_this("@pkg dplyr")` includes dplyr's
    introductory vignette.

  - `btw_this("{btw}")` returns only the package help index (because
    `btw` doesn't have an intro vignette, yet).

- `"?help_topic"` or `"@help topic"`  
  When the string starts with `?` or `@help`, btw searches R's help
  topics using
  [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/reference/btw_tool_package_docs.md).
  Supports multiple formats:

  - `btw_this("?dplyr::across")` or `btw_this("@help dplyr::across")`

  - `btw_this("@help dplyr across")` - space-separated format

  - `btw_this("@help across")` - searches all packages

- `"@news {{package_name}} {{search_term}}"`  
  Include the release notes (NEWS) from the latest package release, e.g.
  `"@news dplyr"`, or that match a search term, e.g.
  `"@news dplyr join_by"`.

- `"@url {{url}}"`  
  Include the contents of a web page at the specified URL as markdown,
  e.g. `"@url https://cran.r-project.org/doc/FAQ/R-FAQ.html"`. Requires
  the chromote package to be installed.

- `"@git status"`, `"@git diff"`, `"@git log"`  
  Git commands for viewing repository status, diffs, and commit history.
  Requires gert package and a git repository.

  - `btw_this("@git status")` - show working directory status

  - `btw_this("@git status staged")` - show only staged files

  - `btw_this("@git diff")` - show unstaged changes

  - `btw_this("@git diff HEAD")` - show staged changes

  - `btw_this("@git log")` - show recent commits (default 10)

  - `btw_this("@git log main 20")` - show 20 commits from main branch

- `"@issue #number"` or `"@pr #number"`  
  Fetch a GitHub issue or pull request. Automatically detects the
  current repository, or you can specify `owner/repo#number` or
  `owner/repo number`. Requires gh package and GitHub authentication.

  - `btw_this("@issue #65")` - issue from current repo

  - `btw_this("@pr posit-dev/btw#64")` - PR from specific repo

  - `btw_this("@issue tidyverse/dplyr 1234")` - space-separated format

- `"@current_file"` or `"@current_selection"`  
  When used in RStudio or Positron, or anywhere else that the rstudioapi
  is supported, `btw("@current_file")` includes the contents of the file
  currently open in the editor using
  [`rstudioapi::getSourceEditorContext()`](https://rstudio.github.io/rstudioapi/reference/rstudio-editors.html).

- `"@clipboard"`  
  Includes the contents currently stored in your clipboard.

- `"@platform_info"`  
  Includes information about the current platform, such as the R
  version, operating system, IDE or UI being used, as well as language,
  locale, timezone and current date.

- `"@attached_packages"`, `"@loaded_packages"`,
  `"@installed_packages"`  
  Includes information about the attached, loaded, or installed packages
  in your R session, using
  [`sessioninfo::package_info()`](https://sessioninfo.r-lib.org/reference/package_info.html).

- `"@last_error"`  
  Includes the message from the last error that occurred in your
  session. To reliably capture the last error, you need to enable
  [`rlang::global_entrace()`](https://rlang.r-lib.org/reference/global_entrace.html)
  in your session.

- `"@last_value"`  
  Includes the `.Last.value`, i.e. the result of the last expression
  evaluated in your R console.

## Usage

``` r
# S3 method for class 'character'
btw_this(x, ..., caller_env = parent.frame())
```

## Arguments

- x:

  A character string

- ...:

  Ignored.

- caller_env:

  The caller environment.

## Value

A character vector of lines describing the object.

## See also

Other btw formatting methods:
[`btw_this()`](https://posit-dev.github.io/btw/reference/btw_this.md),
[`btw_this.data.frame()`](https://posit-dev.github.io/btw/reference/btw_this.data.frame.md),
[`btw_this.environment()`](https://posit-dev.github.io/btw/reference/btw_this.environment.md)

## Examples

``` r
mtcars[1:3, 1:4]
#>                mpg cyl disp  hp
#> Mazda RX4     21.0   6  160 110
#> Mazda RX4 Wag 21.0   6  160 110
#> Datsun 710    22.8   4  108  93
cat(btw_this("@last_value"))
#> $repos NULL  $Ncpus NULL  $HTTPUserAgent [1] "R/4.5.2 (ubuntu-24.04) R (4.5.2 x86_64-pc-linux-gnu x86_64 linux-gnu)" 
```
