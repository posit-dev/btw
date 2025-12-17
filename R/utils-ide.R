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
