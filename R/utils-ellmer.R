btw_prompt <- function(path, ..., .envir = parent.frame()) {
  path <- system.file("prompts", path, package = "btw")
  ellmer::interpolate_file(path, ..., .envir = .envir)
}
