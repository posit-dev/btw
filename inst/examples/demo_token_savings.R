# Compare token usage between original and optimized argument sections
# This compares the ACTUAL markdown output from btw_tool_docs_help_page
# before and after our optimization

library(fs)

# Create comparison output directory
output_dir <- "test_token_comparison"
dir_create(output_dir)

# Helper function to extract Arguments section
extract_arguments_section <- function(lines) {
  args_start <- which(grepl("^#### Arguments", lines))
  if (length(args_start) == 0) return(NULL)

  args_start <- args_start[1]

  # Find next section
  next_sections <- which(grepl("^#### ", lines[(args_start + 1):length(lines)]))
  if (length(next_sections) > 0) {
    args_end <- args_start + next_sections[1] - 1
  } else {
    args_end <- length(lines)
  }

  lines[args_start:args_end]
}

# Helper function to get help page WITHOUT our optimization
get_help_page_original <- function(topic, package_name) {
  help_page <- utils::help(topic, package = (package_name))
  help_path <- as.character(help_page)
  rd_name <- basename(help_path)
  rd_package <- basename(dirname(dirname(help_path)))
  rd_obj <- tools::Rd_db(rd_package)[[paste0(rd_name, ".Rd")]]

  # Generate HTML
  tmp_html <- tempfile(fileext = ".html")
  tools::Rd2HTML(rd_obj, out = tmp_html)

  # Convert to markdown WITHOUT simplification
  tmp_md <- tempfile(fileext = ".md")
  rmarkdown::pandoc_convert(
    tmp_html,
    from = "html",
    to = "markdown_strict+pipe_tables+backtick_code_blocks",
    output = tmp_md,
    options = c("--shift-heading-level-by=1")
  )

  readLines(tmp_md)
}

# Load optimized version
devtools::load_all()

# Test cases - focus on functions with complex argument tables
test_cases <- list(
  list(pkg = "ggplot2", topic = "ggplot"),
  list(pkg = "ggplot2", topic = "geom_point"),
  list(pkg = "dplyr", topic = "mutate"),
  list(pkg = "dplyr", topic = "select"),
  list(pkg = "dplyr", topic = "join"),
  list(pkg = "tidyr", topic = "pivot_longer"),
  list(pkg = "tidyr", topic = "pivot_wider"),
  list(pkg = "purrr", topic = "map"),
  list(pkg = "readr", topic = "read_csv"),
  list(pkg = "base", topic = "data.frame"),
  list(pkg = "base", topic = "merge"),
  list(pkg = "stats", topic = "lm"),
  list(pkg = "stats", topic = "glm"),
  list(pkg = "utils", topic = "read.table"),
  list(pkg = "utils", topic = "install.packages")
)

# Run comparisons
results <- list()
cat("Comparing original vs optimized argument sections...\n\n")

for (i in seq_along(test_cases)) {
  tc <- test_cases[[i]]
  test_name <- sprintf("%s::%s", tc$pkg, tc$topic)

  cat(sprintf("[%2d/%2d] %s ... ", i, length(test_cases), test_name))

  result <- tryCatch({
    # Get ORIGINAL version (before our optimization)
    original_lines <- get_help_page_original(tc$topic, tc$pkg)
    original_args <- extract_arguments_section(original_lines)

    # Get OPTIMIZED version (with our optimization)
    optimized_result <- btw_tool_docs_help_page_impl(tc$topic, tc$pkg)
    optimized_lines <- optimized_result@value
    optimized_args <- extract_arguments_section(optimized_lines)

    if (is.null(original_args) || is.null(optimized_args)) {
      cat("No Arguments section\n")
      return(NULL)
    }

    # Calculate character counts
    orig_chars <- sum(nchar(original_args))
    opt_chars <- sum(nchar(optimized_args))
    saved_chars <- orig_chars - opt_chars
    pct_saved <- round(100 * saved_chars / orig_chars, 1)

    # Estimate tokens (rough approximation: 1 token ≈ 4 characters)
    orig_tokens <- round(orig_chars / 4)
    opt_tokens <- round(opt_chars / 4)
    saved_tokens <- orig_tokens - opt_tokens

    cat(sprintf("✓ Saved %d chars (%.1f%%) ≈ %d tokens\n",
                saved_chars, pct_saved, saved_tokens))

    # Save comparison files
    orig_file <- path(output_dir, sprintf("%02d_%s_%s_ORIGINAL.txt", i, tc$pkg, tc$topic))
    opt_file <- path(output_dir, sprintf("%02d_%s_%s_OPTIMIZED.txt", i, tc$pkg, tc$topic))
    writeLines(original_args, orig_file)
    writeLines(optimized_args, opt_file)

    list(
      test_name = test_name,
      orig_chars = orig_chars,
      opt_chars = opt_chars,
      saved_chars = saved_chars,
      pct_saved = pct_saved,
      orig_tokens = orig_tokens,
      opt_tokens = opt_tokens,
      saved_tokens = saved_tokens,
      orig_file = basename(orig_file),
      opt_file = basename(opt_file)
    )
  }, error = function(e) {
    cat(sprintf("✗ Error: %s\n", conditionMessage(e)))
    NULL
  })

  results[[i]] <- result
}

# Filter out NULL results
results <- Filter(Negate(is.null), results)

# Generate summary report
summary_file <- path(output_dir, "00_COMPARISON_SUMMARY.txt")

summary_lines <- c(
  "========================================",
  "Token Comparison: Arguments Section Only",
  "========================================",
  sprintf("Test date: %s", Sys.time()),
  sprintf("Total comparisons: %d", length(results)),
  "",
  "Methodology:",
  "  - ORIGINAL: btw_tool_docs_help_page WITHOUT simplify_help_tables()",
  "  - OPTIMIZED: btw_tool_docs_help_page WITH simplify_help_tables()",
  "  - Only comparing Arguments section content",
  "  - Token estimate: 1 token ≈ 4 characters",
  "",
  "========================================",
  ""
)

writeLines(summary_lines, summary_file)

# Detailed results
cat("\n\nDetailed Results:\n", file = summary_file, append = TRUE)
cat("=================\n\n", file = summary_file, append = TRUE)

for (i in seq_along(results)) {
  r <- results[[i]]
  lines <- c(
    sprintf("%2d. %s", i, r$test_name),
    sprintf("    Original:  %5d chars ≈ %4d tokens", r$orig_chars, r$orig_tokens),
    sprintf("    Optimized: %5d chars ≈ %4d tokens", r$opt_chars, r$opt_tokens),
    sprintf("    Saved:     %5d chars ≈ %4d tokens (%.1f%%)",
            r$saved_chars, r$saved_tokens, r$pct_saved),
    sprintf("    Files: %s", r$orig_file),
    sprintf("           %s", r$opt_file),
    ""
  )
  cat(paste(lines, collapse = "\n"), file = summary_file, append = TRUE)
}

# Statistics
cat("\nStatistics:\n", file = summary_file, append = TRUE)
cat("===========\n", file = summary_file, append = TRUE)

total_orig_chars <- sum(vapply(results, function(r) r$orig_chars, numeric(1)))
total_opt_chars <- sum(vapply(results, function(r) r$opt_chars, numeric(1)))
total_saved_chars <- total_orig_chars - total_opt_chars
total_pct <- round(100 * total_saved_chars / total_orig_chars, 1)

total_orig_tokens <- sum(vapply(results, function(r) r$orig_tokens, numeric(1)))
total_opt_tokens <- sum(vapply(results, function(r) r$opt_tokens, numeric(1)))
total_saved_tokens <- total_orig_tokens - total_opt_tokens

avg_pct_saved <- mean(vapply(results, function(r) r$pct_saved, numeric(1)))

stats_lines <- c(
  sprintf("Total original characters:  %6d ≈ %5d tokens", total_orig_chars, total_orig_tokens),
  sprintf("Total optimized characters: %6d ≈ %5d tokens", total_opt_chars, total_opt_tokens),
  sprintf("Total saved:                %6d ≈ %5d tokens (%.1f%%)",
          total_saved_chars, total_saved_tokens, total_pct),
  "",
  sprintf("Average reduction per function: %.1f%%", avg_pct_saved),
  sprintf("Functions tested: %d", length(results)),
  ""
)

cat(paste(stats_lines, collapse = "\n"), file = summary_file, append = TRUE)

# Token savings breakdown
cat("\nToken Savings Breakdown:\n", file = summary_file, append = TRUE)
cat("========================\n", file = summary_file, append = TRUE)

# Sort by tokens saved
results_sorted <- results[order(vapply(results, function(r) r$saved_tokens, numeric(1)),
                                decreasing = TRUE)]

for (i in 1:min(10, length(results_sorted))) {
  r <- results_sorted[[i]]
  cat(sprintf("%2d. %-30s  Saved: %4d tokens (%.1f%%)\n",
              i, r$test_name, r$saved_tokens, r$pct_saved),
      file = summary_file, append = TRUE)
}

cat("\n========================================\n", file = summary_file, append = TRUE)
cat(sprintf("Output directory: %s\n", path_abs(output_dir)), file = summary_file, append = TRUE)
cat("========================================\n", file = summary_file, append = TRUE)

# Print summary to console
cat("\n\n")
cat("========================================\n")
cat("Comparison Summary\n")
cat("========================================\n")
cat(sprintf("✓ Compared %d functions\n", length(results)))
cat(sprintf("✓ Total tokens saved: %d (%.1f%%)\n", total_saved_tokens, total_pct))
cat(sprintf("✓ Average reduction: %.1f%%\n", avg_pct_saved))
cat(sprintf("✓ Output directory: %s/\n", path_abs(output_dir)))
cat("========================================\n\n")

# Display full summary
cat(readLines(summary_file), sep = "\n")
