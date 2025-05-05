
<!-- README.md is generated from README.Rmd. Please edit that file -->

# btw

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/btw)](https://CRAN.R-project.org/package=btw)
[![R-CMD-check](https://github.com/posit-dev/btw/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/posit-dev/btw/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/posit-dev/btw/graph/badge.svg)](https://app.codecov.io/gh/posit-dev/btw)
<!-- badges: end -->

btw helps you describe your computational environment to LLMs.

- When used **interactively**, `btw()` assembles context on your R
  environment, package documentation, and working directory, copying the
  results to your clipboard for easy pasting into chat interfaces.
- The `btw()` function wraps methods that can be easily incorporated
  into **ellmer tool calls** for describing various kinds of objects
  in R. To equip your ellmer chat with the ability to peruse
  documentation, check out the objects in your R environment, and
  explore your working directory, pass `your chat to
  `btw_set_tools()`.`

## Installation

You can install the development version of btw like so:

``` r
pak::pak("posit-dev/btw")
```

## Example

``` r
library(btw)
```

### Interactive use

The `btw()` function allows you to compile information about all sorts
of R stuff and copy it to your clipboard.

- Pass any R object to `btw()`, like `btw(mtcars)`, `btw(dplyr::across)`
  or `btw(globalenv())`.
- `btw("{dplyr}")` will describe the dplyr package via the function
  reference and (if available) introductory vignette.
- `btw("?dplyr::across")` or `btw(?dplyr::across)` includes the help
  pages for `dplyr::across()`.
- `btw("./path")` will read the lines of the file at that path (or list
  the files at that path if it’s a directory).
- `btw("@current_file")` and `btw("@current_selection")` reads the
  contents of the current editor or selection in RStudio, Positron, or
  anywhere the [rstudioapi](https://rstudio.github.io/rstudioapi) is
  supported.
- `btw("@platform_info")` describes your R version, operating system,
  and locale information.
- `btw()` with `"@attached_packages"`, `"@loaded_packages"`, or
  `"@installed_packages"` includes a listing of attached (i.e. with
  `library()`), loaded (in use in the session, often indirectly), or
  installed packages.
- And more! See `?btw_this.character` for the full list of additional
  context strings.

When passed multiple arguments, `btw()` will concatenate each
description. For example, you could run:

``` r
btw(mtcars, "{btw}", ?btw::btw)
```

    ✔ btw copied to the clipboard!

The following would be attached to your clipboard:

    ## Context

    mtcars
    ```json
    {"n_cols":11,"n_rows":32,"groups":[],"class":"data.frame","columns":{"mpg":{"variable":"mpg","type":"numeric","mean":20.0906,"sd":6.0269,"p0":10.4,"p25":15.425,"p50":19.2,"p75":22.8,"p100":33.9},"cyl":{"variable":"cyl","type":"numeric","mean":6.1875,"sd":1.7859,"p0":4,"p25":4,"p50":6,"p75":8,"p100":8},"disp":{"variable":"disp","type":"numeric","mean":230.7219,"sd":123.9387,"p0":71.1,"p25":120.825,"p50":196.3,"p75":326,"p100":472},"hp":{"variable":"hp","type":"numeric","mean":146.6875,"sd":68.5629,"p0":52,"p25":96.5,"p50":123,"p75":180,"p100":335},"drat":{"variable":"drat","type":"numeric","mean":3.5966,"sd":0.5347,"p0":2.76,"p25":3.08,"p50":3.695,"p75":3.92,"p100":4.93},"wt":{"variable":"wt","type":"numeric","mean":3.2172,"sd":0.9785,"p0":1.513,"p25":2.5812,"p50":3.325,"p75":3.61,"p100":5.424},"qsec":{"variable":"qsec","type":"numeric","mean":17.8487,"sd":1.7869,"p0":14.5,"p25":16.8925,"p50":17.71,"p75":18.9,"p100":22.9},"vs":{"variable":"vs","type":"numeric","mean":0.4375,"sd":0.504,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"am":{"variable":"am","type":"numeric","mean":0.4062,"sd":0.499,"p0":0,"p25":0,"p50":0,"p75":1,"p100":1},"gear":{"variable":"gear","type":"numeric","mean":3.6875,"sd":0.7378,"p0":3,"p25":3,"p50":4,"p75":4,"p100":5},"carb":{"variable":"carb","type":"numeric","mean":2.8125,"sd":1.6152,"p0":1,"p25":2,"p50":2,"p75":4,"p100":8}}}
    ```

    "{btw}"
    ```json
    [
      {"topic_id":"btw","title":"Plain-text descriptions of R objects","aliases":["btw"]},
      {"topic_id":"btw-package","title":"btw: Describe R Stuff to Large Language Models","aliases":["btw-package"]},
      {"topic_id":"btw_client","title":"Create a btw-enhanced ellmer chat client","aliases":["btw_client","btw_app"]},
      {"topic_id":"btw_set_tools","title":"Tools: Register tools from btw","aliases":["btw_set_tools"]},
      {"topic_id":"btw_this","title":"Describe something for use by an LLM","aliases":["btw_this"]},
      {"topic_id":"btw_this.character","title":"Describe objects","aliases":["btw_this.character"]},
      {"topic_id":"btw_this.data.frame","title":"Describe a data frame in plain text","aliases":["btw_this.data.frame","btw_this.tbl"]},
      {"topic_id":"btw_this.environment","title":"Describe the contents of an environment","aliases":["btw_this.environment"]},
      {"topic_id":"btw_tool_describe_data_frame","title":"Tool: Describe data frame","aliases":["btw_tool_describe_data_frame"]},
      {"topic_id":"btw_tool_describe_environment","title":"Tool: Describe an environment","aliases":["btw_tool_describe_environment"]},
      {"topic_id":"btw_tool_list_files","title":"Tool: List files","aliases":["btw_tool_list_files"]},
      {"topic_id":"btw_tool_package_docs","title":"Tool: Describe R package documentation","aliases":["btw_tool_package_docs","btw_tool_get_package_help_topics","btw_tool_get_help_page","btw_tool_get_available_vignettes_in_package","btw_tool_get_vignette_from_package"]},
      {"topic_id":"btw_tool_read_current_editor","title":"Tool: Read current file","aliases":["btw_tool_read_current_editor"]},
      {"topic_id":"btw_tool_read_text_file","title":"Tool: Read a file","aliases":["btw_tool_read_text_file"]},
      {"topic_id":"btw_tool_session_package_info","title":"Tool: Gather information about a package or currently loaded packages","aliases":["btw_tool_session_package_info"]},
      {"topic_id":"btw_tool_session_platform_info","title":"Tool: Describe user's platform","aliases":["btw_tool_session_platform_info"]}
    ]
    ```

    `?`(btw::btw)
    btw                    package:btw                     R Documentation

    Plain-text descriptions of R objects

    Description:

         This function allows you to quickly describe your computational
         environment to a model by concatenating plain-text descriptions of
         "R stuff", from data frames to packages to function documentation.

         There are two key ways to use 'btw()':

           1. Use it interactively at the console to gather information
              about your environment into prompt text that you can paste
              into the chat interface of an LLM, like ChatGPT or Claude. By
              default, 'btw()' copies the prompt to the clipboard for you.

              btw(vignette("colwise", "dplyr"), dplyr::across, dplyr::starwars)
              #> ✔ btw copied to the clipboard!
              
           2. Pair 'btw()' with ellmer::Chat during a chat session to
              create a prompt that includes additional context drawn from
              your environment and help pages.

              library(ellmer)
              
              chat <- chat_claude() # requires an Anthropic API key
              chat <- chat_ollama(model = "llama3.1:8b") # requires ollama and a local model
              
              chat$chat(btw(
                vignette("colwise", "dplyr"),
                dplyr::across,
                dplyr::starwars,
                "Create a few interesting examples that use `dplyr::across()`",
                "with the `starwars` data set."
              ))
              
    Usage:

         btw(..., clipboard = TRUE)
         
    Arguments:

         ...: Objects to describe from your R environment. You can pass
              objects themselves, like data frames or functions, or the
              function also accepts output from btw_tool_*() functions like
              'btw_tool_get_package_help_topics()',
              'btw_tool_get_help_page()', etc. If omitted, this function
              will just describe the elements in your global R environment.

    clipboard: Whether to write the results to the clipboard. A single
              logical value; will default to 'TRUE' when run interactively.

    Value:

         Returns an ellmer::ContentText object with the collected prompt.
         If 'clipboard = TRUE', the prompt text is copied to the clipboard
         when the returned object is printed for the first time (e.g.
         calling 'btw()' without assignment).

    Examples:

         btw()
         
         btw(mtcars)
         
         btw(btw::btw)
         
         if (FALSE) {
           # btw() can also be used directly in {ellmer} chats
           library(ellmer)
         
           chat <- chat_ollama(model = "llama3.1:8b")
           chat$chat(
             btw(mtcars, "Are there cars with 8 cylinders in this dataset?")
           )
         }
         

You can also just call `btw()` with no inputs, which will describe the
objects in your global environment.

### Supercharging assistants

`btw_client()` equips LLM chats with all of the same capabilities:
peruse documentation, check out objects in your R environment, and
explore your working directory.

``` r
ch <- btw_client()

ch$chat("Hey!")
```

    Hello! I'm here to help you work with R-related information and data. I can help
    you:

    1. Look up information about installed R packages
    2. Get help documentation for specific R packages and functions
    3. Examine and manipulate data frames
    4. Check what's available in the current environment

    What would you like to know about? Feel free to ask a specific question, and I'll
    use the available tools to help you find the information you need.

You can also provide `btw_client()` with objects and documentation to
include as context in the chat:

``` r
ch <- btw_client(mtcars)
ch$chat("Write base R code to summarize average mpg of cars with 4-6 cylinders.")
```

    I'll help you write base R code to calculate the average MPG for cars with 4-6
    cylinders using the mtcars dataset.

    Here's the code:

    ```R
    mean(mtcars$mpg[mtcars$cyl >= 4 & mtcars$cyl <= 6])
    ```

    Alternatively, you could also write it with `subset()` from base R:

    ```R
    mean(subset(mtcars, cyl >= 4 & cyl <= 6)$mpg)
    ```

    Both approaches will give you the average MPG for cars with 4-6 cylinders.
    Given the data shown in the context, this will include cars with 4 and 6
    cylinders (as the `cyl` column only contains values 4, 6, and 8).

`btw_client()` uses `ellmer::chat_claude()` by default, or you can
customize the chat client used in one of two ways:

- Set the `btw.chat_client` option (possibly in your `.Rprofile` with
  `usethis::edit_r_profile()`), or
- Provide your own chat object via the `client` argument.

``` r
options(btw.chat_client = ellmer::chat_ollama(model = "llama3.1:8b"))
ch <- btw_client()

# same as above
ch <- btw_client(client = ellmer::chat_ollama(model = "llama3.1:8b"))
```

Alternatively, you can call `btw_app()` to jump straight into a Shiny
chat app.

For fully customized chat clients, you can use `btw_set_tools()` to
add btw tools to an existing chat interface. Each of the individual
tools registered by `btw_set_tools()` are themselves exported, and
can be selected via the `tools` argument of `btw_set_tools()` (also
available in `btw_client()` or `btw_app()`).
