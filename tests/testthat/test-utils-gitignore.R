# Utility to help compare ignoring order
expect_setequal_char <- function(object, expected) {
  object <- as.character(object)
  expected <- as.character(expected)
  expect_setequal(object, expected)
}

test_that("returns input when no exclusions", {
  paths <- fs::path(c("a.txt", "b/c.txt", "d/e/f.txt"))
  expect_equal(filter_paths_with_gitignore(paths, character()), paths)
})

test_that("basic glob: * and ? work within segments", {
  paths <- fs::path(c("file1.txt", "file10.txt", "other.txt", "dir/file2.txt"))
  exclusions <- c("file?.txt") # matches file1.txt, file2.txt, ..., but not file10.txt
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, c("file10.txt", "other.txt"))

  exclusions2 <- c("*.txt")
  kept2 <- filter_paths_with_gitignore(paths, exclusions2)
  expect_length(kept2, 0L) # all are ignored
})

test_that("character classes and ranges are supported", {
  paths <- c("file0.txt", "file5.txt", "filea.txt", "filex.txt")
  exclusions <- c("file[0-9].txt") # single digit only
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, c("file10.txt"[FALSE], "filea.txt", "filex.txt")) # i.e., filea, filex

  exclusions2 <- c("file[ax].txt")
  kept2 <- filter_paths_with_gitignore(paths, exclusions2)
  expect_setequal_char(kept2, c("file0.txt", "file5.txt"))
})

test_that("anchors: leading / restricts to repo root", {
  paths <- c("build/app", "src/build/app", "build_directory/app")
  exclusions <- c("/build") # matches root-level segment 'build' (file or dir)
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, c("src/build/app", "build_directory/app"))

  exclusions2 <- c("/build/")
  kept2 <- filter_paths_with_gitignore(paths, exclusions2)
  expect_setequal_char(kept2, c("src/build/app", "build_directory/app"))

  exclusions3 <- c("/build*")
  kept3 <- filter_paths_with_gitignore(paths, exclusions3)
  expect_setequal_char(kept3, "src/build/app") # root build* catches build and build_directory
})

test_that("unanchored matches any path segment", {
  paths <- c(
    "build/app",
    "src/build/app",
    "lib/build/test",
    "build_directory/app"
  )
  exclusions <- c("build") # matches any segment named 'build', not 'build_directory'
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, "build_directory/app")
})

test_that("trailing slash: directory-only semantics", {
  paths <- c(
    "logs", # file named logs
    "logs/app.log", # under logs dir
    "other/logs/x", # nested logs dir
    "notlogs/app.log" # not a logs segment
  )
  exclusions <- c("logs/") # ignore directories named logs
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_true("logs" %in% kept) # file named logs is kept
  expect_false("logs/app.log" %in% kept)
  expect_false("other/logs/x" %in% kept)
  expect_true("notlogs/app.log" %in% kept)
})

test_that("order and negation: last match wins and ! requires prior ignore", {
  paths <- c("logs/app.log", "logs/.keep", "app.log")
  exclusions <- c(
    "logs/",
    "!logs/.keep",
    "*.log",
    "!/app.log"
  )
  kept <- filter_paths_with_gitignore(paths, exclusions)
  # logs/app.log ignored (by logs/ then *.log), logs/.keep re-included, app.log kept by final negation
  expect_setequal_char(kept, c("logs/.keep", "app.log"))

  # Negation with no prior ignore has no effect
  kept2 <- filter_paths_with_gitignore(paths, c("!logs/app.log"))
  expect_setequal_char(kept2, paths)
})

test_that("double star: ** crosses directory boundaries", {
  paths <- c(
    "docs/a.txt",
    "docs/nested/b.txt",
    "src/docs/c.txt",
    "temp/a/cache/file.txt",
    "temp/x/y/cache/z.txt",
    "x/temp/cache/file.txt",
    "a/temp",
    "nested/temp/b",
    "tempfile/cache/q.txt"
  )

  # docs/** ignores everything under docs at any depth
  exclusions1 <- c("docs/**")
  kept1 <- filter_paths_with_gitignore(paths, exclusions1)
  expect_setequal_char(
    kept1,
    c(
      "temp/a/cache/file.txt",
      "temp/x/y/cache/z.txt",
      "x/temp/cache/file.txt",
      "a/temp",
      "nested/temp/b",
      "tempfile/cache/q.txt"
    )
  )

  # **/temp ignores any directory named temp at any depth (not tempfile)
  exclusions2 <- c("**/temp")
  kept2 <- filter_paths_with_gitignore(paths, exclusions2)
  expect_setequal_char(
    kept2,
    c(
      "docs/a.txt",
      "docs/nested/b.txt",
      "src/docs/c.txt",
      "tempfile/cache/q.txt"
    )
  )

  # temp/**/cache ignores cache dirs under any temp at any depth
  exclusions3 <- c("temp/**/cache")
  kept3 <- filter_paths_with_gitignore(paths, exclusions3)
  expected3 <- c(
    "docs/a.txt",
    "docs/nested/b.txt",
    "src/docs/c.txt",
    "a/temp",
    "nested/temp/b",
    "tempfile/cache/q.txt"
  )
  expect_setequal_char(kept3, expected3)
})

test_that("use negation to unignoring a file", {
  paths <- c(
    "node_modules/a.js",
    "node_modules/keep/important.txt",
    "node_modules/keep/skip.txt"
  )
  exclusions <- c(
    "node_modules/",
    "!node_modules/keep/important.txt"
  )
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, "node_modules/keep/important.txt")
})

test_that("escaped spaces and specials are handled", {
  paths <- c(
    "My File.txt",
    "My  File.txt",
    "!important.txt",
    "#literal.txt",
    "hash#note.txt"
  )
  exclusions <- c("My\\ File.txt", "\\!important.txt", "\\#literal.txt")
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, c("My  File.txt", "hash#note.txt"))
})

test_that("anchored negation works for root-only exceptions", {
  paths <- c("app.log", "src/app.log", "nested/app.log")
  exclusions <- c(
    "*.log",
    "!/app.log"
  )
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, "app.log")
})

test_that("empty and NA lines are ignored in exclusions", {
  paths <- c("a.txt", "b.txt")
  exclusions <- c(NA_character_, "", "   ", "#comment", "*.txt")
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_length(kept, 0L)
})

test_that("never-matching rule placeholder is harmless", {
  # Internally, empty or NA patterns map to a never-match regex; ensure no effect
  paths <- c("a", "b/c")
  # create an empty pattern line by constructing a negation of empty which becomes empty after parse
  exclusions <- c("!", "x") # '!' alone ignored as non-effective; 'x' ignores 'x'
  kept <- filter_paths_with_gitignore(paths, exclusions)
  expect_setequal_char(kept, c("a", "b/c"))
})

test_that("multiple rules with last-match-wins semantics", {
  paths <- c("x.tmp", "dir/x.tmp", "x.txt")
  exclusions <- c("*.tmp", "!dir/x.tmp", "x.*", "!x.tmp")
  kept <- filter_paths_with_gitignore(paths, exclusions)
  # Evaluate:
  # - *.tmp ignores x.tmp and dir/x.tmp
  # - !dir/x.tmp re-include dir/x.tmp
  # - x.* ignores x.tmp and x.txt
  # - !x.tmp re-include root x.tmp
  expect_setequal_char(kept, c("x.tmp", "dir/x.tmp"))
})

test_that("directory-only patterns do not match files with the same name", {
  paths <- c("build", "build/file", "builds/file")
  exclusions <- c("build/")
  kept <- filter_paths_with_gitignore(paths, exclusions)
  # Our semantics: 'build' alone (file) should not be matched by dir-only rule
  expect_true("build" %in% kept)
  expect_false("build/file" %in% kept)
  expect_true("builds/file" %in% kept)
})
