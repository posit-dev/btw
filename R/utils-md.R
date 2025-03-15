pipe_wrap <- function(x) paste("|", paste(x, collapse = " | "), "|")

# Simple knitr::kable() replacement
md_table <- function(df) {
  check_data_frame(df)
  withr::local_options(list(digits = 5))

  header_row <- pipe_wrap(colnames(df))
  separator_row <- gsub("[^|]", "-", header_row)

  data_rows <- apply(df, 1, function(row) {
    pipe_wrap(as.character(row))
  })

  paste(c(header_row, separator_row, data_rows), collapse = "\n")
}

md_code_block <- function(type = "", ...) {
  for (n_ticks in 3:20) {
    ticks <- paste(rep("`", n_ticks), collapse = "")
    if (any(grepl(ticks, c(...), fixed = TRUE))) {
      next
    }
    break
  }
  c(paste0(ticks, type), ..., ticks)
}
