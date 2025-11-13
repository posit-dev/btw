# Plain-text descriptions of R objects

This function allows you to quickly describe your computational
environment to a model by concatenating plain-text descriptions of "R
stuff", from data frames to packages to function documentation.

There are two key ways to use `btw()`:

1.  Use it interactively at the console to gather information about your
    environment into prompt text that you can paste into the chat
    interface of an LLM, like ChatGPT or Claude. By default, `btw()`
    copies the prompt to the clipboard for you.

        btw(vignette("colwise", "dplyr"), dplyr::across, dplyr::starwars)
        #> btw copied to the clipboard!

2.  Pair `btw()` with
    [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
    during a chat session to create a prompt that includes additional
    context drawn from your environment and help pages.

        library(ellmer)

        chat <- chat_anthropic() # requires an Anthropic API key
        chat <- chat_ollama(model = "llama3.1:8b") # requires ollama and a local model

        chat$chat(btw(
          vignette("colwise", "dplyr"),
          dplyr::across,
          dplyr::starwars,
          "Create a few interesting examples that use `dplyr::across()`",
          "with the `starwars` data set."
        ))

### Additional examples

1.  Use `btw()` without arguments to describe all objects in your
    workspace:

        btw()
        #> btw copied to the clipboard!

2.  Describe a function (it's documentation) and a data frame:

        btw(dplyr::mutate, mtcars)
        #> btw copied to the clipboard!

3.  Use `btw()` to give additional context to an
    [ellmer::Chat](https://ellmer.tidyverse.org/reference/Chat.html)
    session:

        library(ellmer)

        chat <- chat_ollama(model = "llama3.1:8b")
        chat$chat(
          btw(mtcars, "Are there cars with 8 cylinders in this dataset?")
        )

## Usage

``` r
btw(..., clipboard = TRUE)
```

## Arguments

- ...:

  Objects to describe from your R environment. You can pass objects
  themselves, like data frames or functions, or the function also
  accepts output from `btw_tool_*()` functions like
  [`btw_tool_docs_package_help_topics()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
  [`btw_tool_docs_help_page()`](https://posit-dev.github.io/btw/dev/reference/btw_tool_package_docs.md),
  etc. If omitted, this function will just describe the elements in your
  global R environment.

- clipboard:

  Whether to write the results to the clipboard. A single logical value;
  will default to `TRUE` when run interactively.

## Value

Returns an
[ellmer::ContentText](https://ellmer.tidyverse.org/reference/Content.html)
object with the collected prompt. If `clipboard = TRUE`, the prompt text
is copied to the clipboard when the returned object is printed for the
first time (e.g. calling `btw()` without assignment).

## Examples

``` r
# See documentation for detailed examples
btw(mtcars)
```
