# A consolidated summary of .gitignore syntax (root-level .gitignore)
#
# -  Comments and blank lines
#   - Lines starting with # are comments.
#   - Blank lines are ignored.

# -  Whitespace and escaping
#   - Leading/trailing spaces are significant unless escaped with a backslash.
#   - Escape spaces or special characters with \ (e.g., My\ File.txt, \!, \#).
#
# -  Negation
#   - A leading ! negates a pattern (re-includes files previously ignored).
#   - Negation only applies if a prior rule ignored the path.
#   - If a parent directory is ignored, you must unignore it (and possibly ancestors) to reach a child:
#     - node_modules/
#     - !node_modules/keep/
#     - !node_modules/keep/important.txt
#
# -  Path separators and anchors
#   - Use / as the path separator on all platforms.
#   - A leading / anchors the pattern to the .gitignore directory (repo root here).
#     - /build matches only a root-level path segment named build (file or dir), not build_directory.
#     - build (no leading slash) matches any path segment named build at any depth.
#   - A trailing / matches directories only.
#     - logs/ matches any directory named logs, not a file logs.
#
# -  Exact segment matching vs partials
#   - Patterns match whole path segments unless you use globs.
#   - /build does not match /build_directory.
#   - To match names starting with build at root, use /build*.
#   - To match only the build directory at root, use /build/.
#
# -  Wildcards (globs)
#   - * matches any sequence except /.
#     - *.log ignores all .log files at any depth (unless anchored).
#   - ? matches any single character except /.
#     - file?.txt matches file1.txt but not file10.txt.
#   - [abc] and ranges like [0-9] match one character from the set/range.
#   - Escape special characters with \ to treat them literally.
#
# -  Double-asterisk patterns
#   - ** can match across directory boundaries.
#     - docs/** matches everything under docs/ at any depth.
#     - **/temp matches any temp directory at any depth.
#     - temp/**/cache matches cache directories nested under any temp at any depth.
#
# -  Order and precedence
#   - Rules are evaluated top-to-bottom; the last matching rule wins.
#   - More specific later rules can override earlier ones (often via ! negations).
#
# -  Matching scope
#   - Only affects untracked files. Tracked files remain unless you untrack them (git rm --cached).
#   - Git combines rules from root and nested .gitignore files, .git/info/exclude, and global excludesfile.
#
# -  Practical examples
#   - Ignore a root build dir only: /build/
#   - Ignore any build dir at any depth: build/
#   - Ignore names starting with build at root: /build*
#   - Ignore all node_modules dirs: node_modules/
#   - Ignore all .env files: *.env
#   - Keep one file in an ignored dir:
#     - logs/
#     - !logs/.keep
#   - Ignore all .log except app.log in root:
#     - *.log
#     - !/app.log

#' @param paths character vector of file paths (relative to repo root, using /)
#' @param exclusions character vector of .gitignore-like patterns (order matters)
#' @returns paths with excluded ones removed
#' @noRd
filter_paths_with_gitignore <- function(paths, exclusions) {
  paths <- fs::path_norm(paths)
  rules <- parse_exclusion_rules(exclusions)

  if (length(rules) == 0L) {
    return(paths)
  }

  keep <- map_lgl(paths, gitignore_path_allowed_by_rules, rules = rules)
  paths[keep]
}

# Decision helper for a single path ---------------------------------------

gitignore_path_allowed_by_rules <- function(path, rules) {
  ignored_by_prior <- FALSE
  decision <- TRUE

  for (rule in rules) {
    if (!grepl(rule$pattern, path, perl = TRUE)) {
      # doesn't match this rule, check next
      next
    }

    if (rule$is_neg) {
      if (ignored_by_prior) {
        decision <- TRUE
      } else {
        next
      }
    } else {
      decision <- FALSE
      ignored_by_prior <- TRUE
    }
  }

  decision
}

# Helpers ------------------------------------------------------------------

parse_exclusion_rules <- function(exclusions) {
  if (length(exclusions) == 0L) {
    return()
  }

  raw <- exclusions <- exclusions[nzchar(exclusions)]

  if (length(exclusions) == 0L) {
    return(NULL)
  }

  is_neg <- grepl("^!", exclusions, perl = TRUE)
  exclusions <- substring(exclusions, ifelse(is_neg, 2L, 1L))

  unesc <- map_chr(exclusions, gitignore_unescape)

  is_anchored <- grepl("^/", unesc, perl = TRUE)
  is_dir_only <- grepl("/$", unesc, perl = TRUE)

  pattern <- unesc
  pattern[is_anchored] <- substring(pattern[is_anchored], 2L)
  pattern[is_dir_only] <- substr(
    pattern[is_dir_only],
    1L,
    nchar(pattern[is_dir_only]) - 1L
  )

  # Transpose into list of rules
  rules <- map(seq_along(pattern), function(i) {
    pattern <- gitignore_as_regex(pattern[i], is_anchored[i], is_dir_only[i])
    if (pattern == "") {
      return(NULL)
    }
    list(
      rule = raw[i],
      pattern = pattern,
      is_neg = is_neg[i]
    )
  })

  compact(rules)
}

gitignore_unescape <- function(s) {
  if (is.na(s) || identical(s, "")) {
    return(s)
  }
  PL <- "\uE000"
  s <- gsub("\\\\\\\\", PL, s, perl = TRUE) # \\ -> PL
  s <- gsub("\\\\ ", " ", s, perl = TRUE) # \  -> space
  s <- gsub("\\\\!", "!", s, perl = TRUE)
  s <- gsub("\\\\#", "#", s, perl = TRUE)
  s <- gsub("\\\\\\[", "[", s, perl = TRUE)
  s <- gsub("\\\\\\]", "]", s, perl = TRUE)
  s <- gsub("\\\\-", "-", s, perl = TRUE)
  s <- gsub("\\\\\\*", "*", s, perl = TRUE)
  s <- gsub("\\\\\\?", "?", s, perl = TRUE)
  s <- gsub("\\\\", "", s, perl = TRUE) # drop remaining backslashes
  s <- gsub(PL, "\\\\", s, perl = TRUE) # restore backslash
  s
}

gitignore_as_regex <- function(pattern, is_anchored, is_dir_only = FALSE) {
  pat <- pattern
  if (is.na(pat) || identical(pat, "")) {
    return("")
  }

  # Escape only literal text characters in the original pattern (NOT parentheses).
  # We still need to protect regex metacharacters except [], *, ?, and '/' which
  # we will handle per gitignore semantics below.
  # Keep [] so character classes can be used by the user pattern.
  esc_literal <- function(x) {
    # Escape regex specials except: [ ] * ? /
    # Specials to escape: . ( ) + ^ $ { } | \
    gsub("([.()+^${}|\\\\])", "\\\\\\1", x, perl = TRUE)
  }

  p <- esc_literal(pat)
  # restore character class brackets
  p <- gsub("\\\\\\[", "[", p, perl = TRUE)
  p <- gsub("\\\\\\]", "]", p, perl = TRUE)

  # Sentinels to protect injected regex fragments
  S1 <- "\uE100" # "/**/"     -> "(?:/.+?/)*"
  S2 <- "\uE101" # "/**" end  -> "/.*"
  S3 <- "\uE102" # "**/" start-> "(?:.*/)?"
  S4 <- "\uE103" # remaining "/**" -> "/.*"
  S5 <- "\uE104" # bare "**"  -> ".*"

  # Handle ** in a safe order, replacing with sentinels
  p <- gsub("/[*]{2}/", S1, p, perl = TRUE)

  p <- gsub("/[*]{2}$", S2, p, perl = TRUE)

  p <- sub("^[*]{2}/", S3, p, perl = TRUE)

  p <- gsub("/[*]{2}", S4, p, perl = TRUE)

  # Protect bare ** (anywhere) so single-segment expansions don't touch them
  p <- gsub("[*]{2}", S5, p, perl = TRUE)

  # Single-segment expansions on remaining unprotected '*' and '?'
  p <- gsub("\\*", "[^/]*", p, perl = TRUE)
  p <- gsub("\\?", "[^/]", p, perl = TRUE)

  # Restore sentinels to actual regex fragments
  p <- gsub(S1, "(?:/(.*/)?)*", p, perl = TRUE)
  p <- gsub(S2, "/.*", p, perl = TRUE)
  p <- gsub(S3, "(?:.*/)?", p, perl = TRUE)
  p <- gsub(S4, "/.*", p, perl = TRUE)
  p <- gsub(S5, ".*", p, perl = TRUE)

  # Base boundary
  base <- if (is_anchored) {
    paste0("^", p, "(?:$|/)")
  } else {
    paste0("(^|/)", p, "($|/)")
  }

  if (!isTRUE(is_dir_only)) {
    return(base)
  }

  # Enforce trailing slash for directory-only rules
  dir_rx <- sub("\\(\\?:\\$\\|/\\)\\s*$", "/", base, perl = TRUE)
  dir_rx <- sub("\\(\\$\\|/\\)\\s*$", "/", dir_rx, perl = TRUE)

  if (identical(dir_rx, base)) {
    dir_rx <- paste0("(", base, ")/")
  }

  dir_rx
}
