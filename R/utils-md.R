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

md_kv_table <- function(df, drop_na = FALSE) {
  check_data_frame(df)

  res <- vector("character", nrow(df))

  for (i in seq_len(nrow(df))) {
    item <- vector("character", ncol(df))
    for (j in seq_len(ncol(df))) {
      value <- df[[j]][[i]]
      if (drop_na && is.na(value)) {
        next
      }
      item[j] <- paste0(colnames(df)[j], ": ", format(value))
    }
    item <- item[nzchar(item)]
    res[i] <- paste(item, collapse = "\n")
  }

  paste(res, collapse = "\n\n")
}
