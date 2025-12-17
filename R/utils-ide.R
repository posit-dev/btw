which_ide <- function() {
  if (identical(Sys.getenv("POSITRON"), "1")) {
    return("positron")
  }

  if (identical(Sys.getenv("RSTUDIO"), "1")) {
    return("rstudio")
  }

  if (identical(Sys.getenv("TERM_PROGRAM"), "vscode")) {
    return("vs_code")
  }
}

which_ide_title <- function() {
  switch(
    which_ide() %||% "",
    positron = "Positron",
    rstudio = "RStudio",
    vs_code = "VS Code"
  )
}

#' Open a new document and add code
#'
#' Creates a new untitled R document in RStudio and populates it with the
#' provided code.
#'
#' @param code A string containing lines of text to add to the new document
#'
#' @return Invisibly returns the document ID of the newly created document
#'
#' @noRd
ide_insert_new_file <- function(code, language = "r") {
  if (!rstudioapi::hasFun("documentNew")) {
    cli::cli_abort(
      "Your IDE does not support creating new documents via {.pkg rstudioapi}."
    )
  }

  check_string(code)
  check_string(language, allow_null = TRUE)
  # documentNew() only supports these 3 types rstudio/rstudioapi#316
  language <- intersect(language, c("r", "rmarkdown", "sql"))

  doc_id <- rstudioapi::documentNew(
    text = code,
    type = language %||% "r",
    position = rstudioapi::document_position(0, 0),
    execute = FALSE
  )

  invisible(doc_id)
}


#' Insert code into the currently open editor
#'
#' Inserts the provided code at the current cursor position in the active
# RStudio editor.
#'
#' @param code A scalar string containing lines of text to insert into the
#'   current document
#'
#' @return Invisible returns active document context
#'
#' @noRd
ide_insert_cursor <- function(code) {
  if (
    !rstudioapi::hasFun("getActiveDocumentContext") ||
      !rstudioapi::hasFun("insertText")
  ) {
    cli::cli_abort(
      "Your IDE does not support inserting text into documents via {.pkg rstudioapi}."
    )
  }

  check_string(code)

  # Get the active document context to ensure an editor is open
  context <- rstudioapi::getActiveDocumentContext()

  # Insert text at the current cursor position
  rstudioapi::insertText(
    text = code,
    id = context$id,
    location = context$selection[[1]]$range
  )

  invisible(context)
}

#' Run code in the console
#'
#' Sends the provided code to the R console in RStudio, executes it, and
#' activates the console view.
#'
#' @param code A scalar string containing lines of text to send to the console
#'
#' @return Invisibly returns NULL (called for side effects)
#'
#' @noRd
ide_run_in_console <- function(code) {
  if (!rstudioapi::hasFun("sendToConsole")) {
    cli::cli_abort(
      "Your IDE does not support sending code to the console via {.pkg rstudioapi}."
    )
  }

  check_string(code)

  rstudioapi::sendToConsole(code = code)

  invisible(NULL)
}
