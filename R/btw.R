#' Plain-text descriptions of R objects
#'
#' @description
#' This function allows you to quickly describe your computational environment
#' to a model by concatenating plain-text descriptions of "R stuff", from
#' data frames to packages to function documentation.
#'
#' There are two key ways to use `btw()`:
#'
#' 1. Use it interactively at the console to gather information about your
#'    environment into prompt text that you can paste into the chat interface of
#'    an LLM, like ChatGPT or Claude. By default, `btw()` copies the prompt to
#'    the clipboard for you.
#'
#'    ```r
#'    btw(vignette("colwise", "dplyr"), dplyr::across, dplyr::starwars)
#'    #> ✔ btw copied to the clipboard!
#'    ```
#'
#' 2. Pair `btw()` with [ellmer::Chat] during a chat session to create a prompt
#'    that includes additional context drawn from your environment and help
#'    pages.
#'
#'    ```r
#'    library(ellmer)
#'
#'    chat <- chat_anthropic() # requires an Anthropic API key
#'    chat <- chat_ollama(model = "llama3.1:8b") # requires ollama and a local model
#'
#'    chat$chat(btw(
#'      vignette("colwise", "dplyr"),
#'      dplyr::across,
#'      dplyr::starwars,
#'      "Create a few interesting examples that use `dplyr::across()`",
#'      "with the `starwars` data set."
#'    ))
#'    ```
#'
#' @param ... Objects to describe from your R environment. You can pass objects
#'   themselves, like data frames or functions, or the function also accepts
#'   output from `btw_tool_*()` functions like
#'   [btw_tool_docs_package_help_topics()], [btw_tool_docs_help_page()], etc. If
#'   omitted, this function will just describe the elements in your global R
#'   environment.
#' @param clipboard Whether to write the results to the clipboard.
#'   A single logical value; will default to `TRUE` when run interactively.
#'
#' @examples
#' btw()
#'
#' btw(mtcars)
#'
#' btw(btw::btw)
#'
#' if (FALSE) {
#'   # btw() can also be used directly in {ellmer} chats
#'   library(ellmer)
#'
#'   chat <- chat_ollama(model = "llama3.1:8b")
#'   chat$chat(
#'     btw(mtcars, "Are there cars with 8 cylinders in this dataset?")
#'   )
#' }
#'
#' @returns
#' Returns an [ellmer::ContentText] object with the collected prompt. If
#' `clipboard = TRUE`, the prompt text is copied to the clipboard when the
#' returned object is printed for the first time (e.g. calling `btw()` without
#' assignment).
#'
#' @export
btw <- function(..., clipboard = TRUE) {
  check_bool(clipboard)

  elts <- dots_list(!!!enquos(...), .named = TRUE)

  if (length(elts) == 0) {
    res <- btw_this(globalenv())
  } else {
    elts <- lapply(elts, eval_tidy)
    res <- btw_this(
      new_environment(elts, parent = parent.frame()),
      items = names(elts)
    )
  }

  res <- paste(res, collapse = "\n")

  if (identical(res, "")) {
    cli::cli_alert_warning("Nothing to include in the btw() context.")
    return(invisible(""))
  }

  BTW(text = res, settings = env(clipboard = clipboard))
}

BTW <- S7::new_class(
  "btw",
  parent = ellmer::ContentText,
  package = "btw",
  properties = list(
    text = S7::class_character,
    settings = S7::class_environment
  )
)

S7::method(print, BTW) <- function(x, ...) {
  if (!x@settings$clipboard) {
    writeLines(x@text)
    return(invisible(x))
  }

  x@settings$clipboard <- FALSE

  write_to_clipboard(x@text)

  invisible(x)
}

S7::method(as.character, BTW) <- function(x, ...) {
  x@text
}
