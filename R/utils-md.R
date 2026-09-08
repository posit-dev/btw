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
  paste(c(paste0(ticks, type), ..., ticks), collapse = "\n")
}

# Escape raw HTML in markdown before rendering it as trusted `display$html`
# (shinychat renders html cards without sanitizing them). Raw HTML in
# markdown passes straight through commonmark into the page, so it must be
# escaped first. Inline code spans and fenced code blocks are left verbatim:
# commonmark escapes code content itself, and pre-escaping would render
# `&lt;` entities literally inside code.
escape_markdown_html <- function(md) {
  if (!is_string(md)) {
    return(md)
  }

  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  fence <- NULL

  for (i in seq_along(lines)) {
    line <- lines[[i]]

    if (!is.null(fence)) {
      if (grepl(md_fence_close(fence), line)) {
        fence <- NULL
      }
      next
    }

    open <- regmatches(line, regexec("^ {0,3}(`{3,}|~{3,})", line))[[1]]
    if (length(open) > 0) {
      fence <- open[[1]]
      next
    }

    lines[[i]] <- escape_md_html_line(line)
  }

  paste(lines, collapse = "\n")
}

md_fence_close <- function(fence) {
  sprintf("^ {0,3}%s{%d,}[ ]*$", substr(fence, 1, 1), nchar(fence))
}

escape_md_html_line <- function(line) {
  runs <- regmatches(line, gregexpr("`+", line))[[1]]
  if (length(runs) == 0) {
    return(htmltools::htmlEscape(line))
  }

  chunks <- regmatches(line, gregexpr("`+", line), invert = TRUE)[[1]]

  # Keep the content of matched code spans verbatim: commonmark renders it
  # as code and escapes it itself. A code span opens with a run of backticks
  # and closes at the next run of the same length; content between
  # unmatched runs is rendered as prose, so it must stay escaped.
  keep <- rep(FALSE, length(chunks))
  j <- 1
  while (j < length(runs)) {
    closers <- which(nchar(runs[(j + 1):length(runs)]) == nchar(runs[[j]]))
    if (length(closers) == 0) {
      break
    }
    k <- j + closers[[1]]
    keep[(j + 1):k] <- TRUE
    j <- k + 1
  }

  chunks[!keep] <- htmltools::htmlEscape(chunks[!keep])

  out <- character(length(chunks) + length(runs))
  out[seq_along(chunks) * 2 - 1] <- chunks
  out[seq_along(runs) * 2] <- runs
  paste(out, collapse = "")
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
