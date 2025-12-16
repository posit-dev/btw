#' Describe something for use by an LLM
#'
#' A generic function used to describe an object for use by LLM.
#'
#' @examples
#' btw_this(mtcars) # describe the mtcars dataset
#' btw_this(dplyr::mutate) # include function source
#'
#' @param x The thing to describe.
#' @param ... Additional arguments passed down to underlying methods. Unused
#'   arguments are silently ignored.
#'
#' @return A character vector of lines describing the object.
#' @family btw formatting methods
#' @export
btw_this <- function(x, ...) {
  UseMethod("btw_this")
}

#' @export
btw_this.default <- function(x, ...) {
  capture_print(x)
}

#' @export
btw_this.matrix <- function(x, ...) {
  capture_print(x)
}

capture_print <- function(x) {
  # TODO: Replace with {evaluate}
  local_reproducible_output(max.print = 100)

  out <- capture.output(print(x))
  if (length(out) == 0 || !any(nzchar(out))) {
    out <- capture.output(print(x), type = "message")
  }

  as_btw_capture(out)
}

as_btw_capture <- function(x) {
  x <- cli::ansi_strip(x)
  structure(x, class = c("btw_captured", "character"))
}

#' Describe objects
#'
#' @description
#' Character strings in `btw_this()` are used as shortcuts to many underlying
#' methods. `btw_this()` detects specific formats in the input string to
#' determine which method to call, or by default it will try to evaluate the
#' character string as R code and return the appropriate object description.
#'
#' `btw_this()` knows about the following special character string formats:
#'
#' * `"./path"` \cr
#'   Any string starting with `./` is treated as a relative path.
#'   If the path is a file, we call [btw_tool_files_read_text_file()] and if the path
#'   is a directory we call [btw_tool_files_list_files()] on the path.
#'
#'   * `btw_this("./data")` lists the files in `data/`.
#'   * `btw_this("./R/load_data.R")` reads the source of the `R/load_data.R`
#'     file.
#'
#' * `"{pkgName}"` or `"@pkg pkgName"` \cr
#'   A package name wrapped in braces, or using the `@pkg` command. Returns the
#'   list of help topics ([btw_tool_docs_package_help_topics()]) and, if it
#'   exists, the introductory vignette for the package ([btw_tool_docs_vignette()]).
#'
#'   * `btw_this("{dplyr}")` or `btw_this("@pkg dplyr")` includes dplyr's introductory vignette.
#'   * `btw_this("{btw}")` returns only the package help index (because `btw`
#'     doesn't have an intro vignette, yet).
#'
#' * `"?help_topic"` or `"@help topic"` \cr
#'   When the string starts with `?` or `@help`, btw searches R's help
#'   topics using [btw_tool_docs_help_page()]. Supports multiple formats:
#'
#'   * `btw_this("?dplyr::across")` or `btw_this("@help dplyr::across")`
#'   * `btw_this("@help dplyr across")` - space-separated format
#'   * `btw_this("@help across")` - searches all packages
#'
#' * `"@news {{package_name}} {{search_term}}"` \cr
#'   Include the release notes (NEWS) from the latest package release, e.g.
#'   `"@news dplyr"`, or that match a search term, e.g. `"@news dplyr join_by"`.
#'
#' * `"@url {{url}}"` \cr
#'   Include the contents of a web page at the specified URL as markdown, e.g.
#'   `"@url https://cran.r-project.org/doc/FAQ/R-FAQ.html"`. Requires the
#'   \pkg{chromote} package to be installed.
#'
#' * `"@git status"`, `"@git diff"`, `"@git log"` \cr
#'   Git commands for viewing repository status, diffs, and commit history.
#'   Requires \pkg{gert} package and a git repository.
#'
#'   * `btw_this("@git status")` - show working directory status
#'   * `btw_this("@git status staged")` - show only staged files
#'   * `btw_this("@git diff")` - show unstaged changes
#'   * `btw_this("@git diff HEAD")` - show staged changes
#'   * `btw_this("@git log")` - show recent commits (default 10)
#'   * `btw_this("@git log main 20")` - show 20 commits from main branch
#'
#' * `"@issue #number"` or `"@pr #number"` \cr
#'   Fetch a GitHub issue or pull request. Automatically detects the current
#'   repository, or you can specify `owner/repo#number` or `owner/repo number`.
#'   Requires \pkg{gh} package and GitHub authentication.
#'
#'   * `btw_this("@issue #65")` - issue from current repo
#'   * `btw_this("@pr posit-dev/btw#64")` - PR from specific repo
#'   * `btw_this("@issue tidyverse/dplyr 1234")` - space-separated format
#'
#' * `"@current_file"` or `"@current_selection"` \cr
#'   When used in RStudio or Positron, or anywhere else that the
#'   \pkg{rstudioapi} is supported, `btw("@current_file")` includes the contents
#'   of the file currently open in the editor using
#'   [rstudioapi::getSourceEditorContext()].
#'
#' * `"@clipboard"` \cr
#'   Includes the contents currently stored in your clipboard.
#'
#' * `"@platform_info"` \cr
#'   Includes information about the current platform, such as the R version,
#'   operating system, IDE or UI being used, as well as language, locale,
#'   timezone and current date.
#'
#' * `"@attached_packages"`, `"@loaded_packages"`, `"@installed_packages"` \cr
#'   Includes information about the attached, loaded, or installed packages in
#'   your R session, using [sessioninfo::package_info()].
#'
#' * `"@last_error"` \cr
#'   Includes the message from the last error that occurred in your session.
#'   To reliably capture the last error, you need to enable
#'   [rlang::global_entrace()] in your session.
#'
#' * `"@last_value"` \cr
#'   Includes the `.Last.value`, i.e. the result of the last expression
#'   evaluated in your R console.
#'
#' @examples
#' mtcars[1:3, 1:4]
#' cat(btw_this("@last_value"))
#'
#' @param x A character string
#' @param ... Ignored.
#' @param caller_env The caller environment.
#'
#' @inherit btw_this return
#'
#' @family btw formatting methods
#' @export
btw_this.character <- function(x, ..., caller_env = parent.frame()) {
  check_string(x)
  x <- trimws(x)

  # Try @ command parsing first
  if (substring(x, 1, 1) == "@") {
    cmd <- parse_at_command(x)
    result <- dispatch_at_command(cmd, caller_env)
    if (!is.null(result)) {
      return(result)
    }
  }

  # ./path
  if (grepl("^\\./", x)) {
    return(btw_this_file_path(x))
  }

  # {pkgName}
  if (grepl("^\\{[a-zA-Z][a-zA-Z0-9.]+\\}$", x)) {
    return(btw_this_package_braces(x))
  }

  # ?help_topic
  if (substring(x, 1, 1) == "?") {
    return(btw_this_help_question(x))
  }

  # Default: user prompt
  btw_user_prompt(x)
}

# @ Command Parsing and Dispatch ----------------------------------------------

parse_at_command <- function(x) {
  if (substring(x, 1, 1) != "@") {
    return(NULL)
  }

  x <- substring(x, 2)

  # Split command from args
  parts <- strsplit(x, " ", fixed = TRUE)[[1]]

  command <- parts[1]

  args <- if (length(parts) > 1) {
    # Put args back together as a single string; each tool might have its own
    # way to parse these arguments.
    paste(parts[-1], collapse = " ")
  } else {
    ""
  }

  list(command = command, args = args)
}

dispatch_at_command <- function(cmd, caller_env) {
  # Simple commands (no arguments needed)
  if (cmd$command == "current_file") {
    return(I(
      btw_tool_ide_read_current_editor_impl(
        selection = FALSE,
        consent = TRUE
      )@value
    ))
  }

  if (cmd$command == "current_selection") {
    return(I(
      btw_tool_ide_read_current_editor_impl(
        selection = TRUE,
        consent = TRUE
      )@value
    ))
  }

  if (cmd$command == "clipboard") {
    return(I(clipr::read_clip()))
  }

  if (cmd$command == "platform_info") {
    return(btw_tool_session_platform_info_impl()@value)
  }

  if (cmd$command == "attached_packages") {
    return(I(btw_tool_session_package_info_impl("attached")@value))
  }

  if (cmd$command == "loaded_packages") {
    return(I(btw_tool_session_package_info_impl("loaded")@value))
  }

  if (cmd$command == "installed_packages") {
    return(I(btw_tool_session_package_info_impl("installed")@value))
  }

  if (cmd$command == "last_error") {
    err <- get_last_error()
    return(if (is.null(err)) btw_ignore() else capture_print(err))
  }

  if (cmd$command == "last_value") {
    return(btw_this(get_last_value()))
  }

  btw_this_cmd <- switch(
    cmd$command,
    news = btw_this_news,
    url = btw_this_url,
    pkg = btw_this_pkg,
    help = btw_this_help,
    git = btw_this_git,
    issue = btw_this_github_issue,
    pr = btw_this_github_pr,
    # Unknown command - return NULL to fall through
    function(args) NULL
  )

  btw_this_cmd(cmd$args)
}

# Command Handlers ------------------------------------------------------------

btw_this_news <- function(args) {
  if (!nzchar(args)) {
    cli::cli_abort(
      c(
        "{.code @news} must be followed by a package name and an optional search term.",
        "i" = 'e.g. {.code "@news dplyr"} or {.code "@news dplyr join_by"}'
      ),
      call = caller_env(n = 2)
    )
  }

  parts <- strsplit(args, " ", fixed = TRUE)[[1]]
  package_name <- parts[1]
  search_term <- if (length(parts) > 1) {
    paste(parts[-1], collapse = " ")
  } else {
    ""
  }

  I(btw_tool_docs_package_news_impl(package_name, search_term)@value)
}

btw_this_url <- function(args) {
  if (!has_chromote()) {
    cli::cli_abort(c(
      "{.strong @url} requires the {.pkg chromote} package to be installed.",
      "i" = "Please install it with {.run install.packages('chromote')}."
    ))
  }

  url <- trimws(args)
  if (!nzchar(url)) {
    cli::cli_abort(
      c(
        "{.strong @url} must be followed by a valid URL.",
        "i" = 'e.g. {.code "@url https://example.com"}'
      ),
      call = caller_env(n = 2)
    )
  }

  I(btw_tool_web_read_url_impl(url)@value)
}

btw_this_pkg <- function(args) {
  pkg <- trimws(args)
  if (!nzchar(pkg)) {
    cli::cli_abort(c(
      "{.code @pkg} must be followed by a package name.",
      "i" = 'e.g. {.code "@pkg dplyr"}'
    ))
  }

  # Reuse the same logic as {pkg} syntax
  res <- c(
    btw_tool_docs_package_help_topics_impl(pkg)@value,
    tryCatch(
      c("", btw_tool_docs_vignette_impl(pkg)@value),
      error = function(e) NULL
    )
  )
  res
}

btw_this_help <- function(args) {
  args <- trimws(args)
  if (!nzchar(args)) {
    cli::cli_abort(
      c(
        "{.code @help} must be followed by a help topic.",
        "i" = 'e.g. {.code "@help mutate"} or {.code "@help dplyr::mutate"} or {.code "@help dplyr mutate"}'
      ),
      call = caller_env(n = 2)
    )
  }

  # Parse the arguments: can be "pkg::topic", "topic", or "pkg topic"
  if (grepl("::", args, fixed = TRUE)) {
    # Format: @help pkg::topic
    parts <- strsplit(args, "::", fixed = TRUE)[[1]]
    return(btw_this(as_btw_docs_topic(parts[1], parts[2])))
  } else if (grepl(" ", args)) {
    # Format: @help pkg topic
    parts <- strsplit(args, " ", fixed = TRUE)[[1]]
    pkg <- parts[1]
    topic <- paste(parts[-1], collapse = " ")
    return(btw_this(as_btw_docs_topic(pkg, topic)))
  } else {
    # Format: @help topic (search all packages)
    return(btw_this(as_btw_docs_topic(NULL, args)))
  }
}

btw_this_git <- function(args) {
  check_installed("gert")

  # Try to get git info early to provide better error messages
  tryCatch(
    gert::git_info(),
    error = function(e) {
      cli::cli_abort(
        c(
          "Not in a git repository or {.pkg gert} cannot access git.",
          "i" = "Run {.code gert::git_info()} to check your git repository status."
        ),
        call = caller_env(n = 2)
      )
    }
  )

  args <- trimws(args)
  if (!nzchar(args)) {
    cli::cli_abort(
      c(
        "{.code @git} must be followed by a subcommand.",
        "i" = 'Use {.code "@git status"}, {.code "@git diff"}, or {.code "@git log"}'
      ),
      call = caller_env(n = 2)
    )
  }

  # Parse subcommand and remaining args
  git_parts <- strsplit(args, " ", fixed = TRUE)[[1]]
  subcommand <- git_parts[1]
  remaining_args <- if (length(git_parts) > 1) git_parts[-1] else character()

  switch(
    subcommand,
    status = btw_this_git_status(remaining_args),
    diff = btw_this_git_diff(remaining_args),
    log = btw_this_git_log(remaining_args),
    cli::cli_abort(
      c(
        "Unknown git subcommand: {.val {subcommand}}",
        "i" = 'Supported subcommands: {.code status}, {.code diff}, {.code log}'
      ),
      call = caller_env(n = 2)
    )
  )
}

btw_this_git_status <- function(args) {
  # args can be: [] or ["staged"] or ["unstaged"] or ["both"] or [pathspec...]
  include <- "both"
  pathspec <- NULL

  if (length(args) > 0) {
    # Check if first arg is a valid include option
    if (args[1] %in% c("staged", "unstaged", "both")) {
      include <- args[1]
      if (length(args) > 1) {
        pathspec <- args[-1]
      }
    } else {
      # All args are pathspec
      pathspec <- args
    }
  }

  I(md_code_block(
    paste("git status", paste(args, collapse = " ")),
    btw_tool_git_status_impl(include = include, pathspec = pathspec)@value
  ))
}

btw_this_git_diff <- function(args) {
  ref <- if (length(args) > 0) args[1] else NULL
  I(md_code_block(
    paste("git diff", paste(args, collapse = " ")),
    btw_tool_git_diff_impl(ref = ref)@value
  ))
}

btw_this_git_log <- function(args) {
  ref <- "HEAD"
  max <- 10

  if (length(args) > 0) {
    ref <- args[1]
  }
  if (length(args) > 1) {
    max <- suppressWarnings(as.integer(args[2]))
    if (is.na(max) || max < 1) {
      cli::cli_abort(
        c(
          "Invalid max value for {.code @git log}: {.val {args[2]}}",
          "i" = "Must be a positive integer."
        ),
        call = caller_env(n = 3)
      )
    }
  }

  I(md_code_block(
    paste("git log", paste(args, collapse = " ")),
    btw_tool_git_log_impl(ref = ref, max = max)@value
  ))
}

btw_this_github_issue <- function(args) {
  btw_this_github_item(args, type = "issue")
}

btw_this_github_pr <- function(args) {
  btw_this_github_item(args, type = "pr")
}

# Handle @issue or @pr command
# Supports formats:
#   - "#123" (uses current repo)
#   - "owner/repo#123"
#   - "owner/repo 123"
btw_this_github_item <- function(args, type = c("issue", "pr")) {
  check_installed("gh")
  type <- match.arg(type)

  args <- trimws(args)
  if (!nzchar(args)) {
    cli::cli_abort(
      c(
        "{.code @{type}} must be followed by an issue/PR number.",
        "i" = 'e.g. {.code "@{type} #65"} or {.code "@{type} owner/repo#123"}'
      ),
      call = caller_env(n = 3)
    )
  }

  # Parse the arguments to extract owner, repo, and number
  parsed <- parse_github_reference(args)

  # Get owner and repo (will auto-detect if not provided)
  if (is.null(parsed$owner) || is.null(parsed$repo)) {
    repo_info <- get_github_repo(NULL, NULL)
    owner <- parsed$owner %||% repo_info$owner
    repo <- parsed$repo %||% repo_info$repo
  } else {
    owner <- parsed$owner
    repo <- parsed$repo
  }

  number <- parsed$number

  # Fetch from GitHub API
  tryCatch(
    {
      result <- gh::gh(
        "/repos/{owner}/{repo}/issues/{number}",
        owner = owner,
        repo = repo,
        number = number
      )

      # Format the output
      format_github_item(result, owner, repo, type)
    },
    error = function(e) {
      cli::cli_abort(
        "Failed to fetch {type} from GitHub: {owner}/{repo}#{number}",
        parent = e,
        call = caller_env(n = 3)
      )
    }
  )
}

# Parse GitHub reference string
# Supports: "#123", "owner/repo#123", "owner/repo 123"
# @return List with owner, repo, number (owner/repo may be NULL)
parse_github_reference <- function(ref) {
  ref <- trimws(ref)

  # Pattern 1: "#123" (just number)
  if (grepl("^#[0-9]+$", ref)) {
    return(list(
      owner = NULL,
      repo = NULL,
      number = as.integer(substring(ref, 2))
    ))
  }

  # Pattern 2: "owner/repo#123"
  if (grepl("^[^/]+/[^#]+#[0-9]+$", ref)) {
    parts <- strsplit(ref, "[/#]")[[1]]
    return(list(
      owner = parts[1],
      repo = parts[2],
      number = as.integer(parts[3])
    ))
  }

  # Pattern 3: "owner/repo 123" (space-separated)
  if (grepl("^[^/]+/[^ ]+ +[0-9]+$", ref)) {
    parts <- strsplit(ref, "[ /]")[[1]]
    parts <- parts[parts != ""] # Remove empty strings from multiple spaces
    if (length(parts) >= 3) {
      return(list(
        owner = parts[1],
        repo = parts[2],
        number = as.integer(parts[3])
      ))
    }
  }

  # Pattern 4: Just a number "123"
  if (grepl("^[0-9]+$", ref)) {
    return(list(
      owner = NULL,
      repo = NULL,
      number = as.integer(ref)
    ))
  }

  cli::cli_abort(c(
    "Invalid GitHub reference format: {.val {ref}}",
    "i" = 'Expected formats: {.code "#123"}, {.code "owner/repo#123"}, or {.code "owner/repo 123"}'
  ))
}

format_github_item <- function(item, owner, repo, type) {
  outer_tag <- if (type == "issue") "github-issue" else "github-pull-request"
  lines <- c(
    sprintf(
      '<%s owner="%s" repo="%s" number="%d">',
      outer_tag,
      owner,
      repo,
      item$number
    ),
    "<metadata>",
    sprintf("title: %s", item$title),
    sprintf("url: %s", item$html_url),
    sprintf(
      "type: %s",
      if (is.null(item$pull_request)) "Issue" else "Pull Request"
    ),
    sprintf("state: %s", item$state),
    sprintf("author: %s", item$user$login),
    sprintf("created: %s", item$created_at),
    sprintf("updated: %s", item$updated_at)
  )

  if (!is.null(item$closed_at)) {
    lines <- c(lines, sprintf("closed: %s", item$closed_at))
  }

  if (!is.null(item$merged_at)) {
    lines <- c(lines, sprintf("merged: %s", item$merged_at))
  }

  if (length(item$labels) > 0) {
    label_names <- vapply(item$labels, function(l) l$name, character(1))
    lines <- c(
      lines,
      sprintf("labels: %s", paste(label_names, collapse = ", "))
    )
  }

  if (!is.null(item$milestone)) {
    lines <- c(lines, sprintf("milestone: %s", item$milestone$title))
  }

  lines <- c(
    lines,
    "</metadata>",
    "<body>",
    if (is.null(item$body) || !nzchar(item$body)) {
      "_No description provided._"
    } else {
      trimws(item$body)
    },
    "</body>",
    sprintf("</%s>", outer_tag)
  )

  I(paste(lines, collapse = "\n"))
}

btw_this_file_path <- function(x) {
  path <- substring(x, 3, nchar(x))
  if (!nzchar(path)) {
    path <- "."
  }
  if (fs::is_file(path)) {
    return(
      btw_tool_files_read_text_file_impl(path, check_within_wd = FALSE)@value
    )
  } else {
    return(btw_tool_files_list_files_impl(path, check_within_wd = FALSE)@value)
  }
}

btw_this_package_braces <- function(x) {
  # Catch R packages in the form: {dplyr} or {btw}
  # R packages must:
  # * start with a letter
  # * use only letters, numbers or .
  # * be two or more characters long
  pkg <- substring(x, 2, nchar(x) - 1)
  res <- c(
    btw_tool_docs_package_help_topics_impl(pkg)@value,
    tryCatch(
      c("", btw_tool_docs_vignette_impl(pkg)@value),
      error = function(e) NULL
    )
  )
  res
}

btw_this_help_question <- function(x) {
  topic_str <- substring(x, 2, nchar(x))
  parts <- strsplit(topic_str, "::", fixed = TRUE)[[1]]
  if (length(parts) == 2) {
    return(btw_this(as_btw_docs_topic(parts[1], parts[2])))
  } else {
    return(btw_this(as_btw_docs_topic(NULL, parts[1])))
  }
}

# Other btw_this methods ------------------------------------------------------

#' @export
btw_this.Chat <- function(x, ...) {
  btw_ignore()
}

#' @export
btw_this.function <- function(x, ...) {
  fn_def <- capture.output(print(x))

  fn_def <- fn_def[!grepl("^<(bytecode|environment): ", fn_def)]

  md_code_block("r", fn_def)
}

#' @export
btw_this.btw_docs_topic <- function(x, ...) {
  btw_tool_docs_help_page_impl(package_name = x$package, topic = x$topic)@value
}

#' @export
btw_this.help_files_with_topic <- function(x, ...) {
  args <- call_args(attr(x, "call")) # help(topic = {topic}, package = {package})
  btw_tool_docs_help_page_impl(
    package_name = args$package,
    topic = args$topic
  )@value
}

as_btw_docs_topic <- function(package, topic) {
  structure(
    list(package = package, topic = topic),
    class = "btw_docs_topic"
  )
}

#' @export
btw_this.btw_docs_package <- function(x, ...) {
  c(
    sprintf("Documented functions and help topics in package %s:", x$package),
    btw_tool_docs_package_help_topics_impl(x$package)@value
  )
}

as_btw_docs_package <- function(package) {
  structure(list(package = package), class = "btw_docs_package")
}

#' @export
btw_this.packageIQR <- function(x, ...) {
  # vignette(package = "btw")
  package_name <- unique(x$results[, "Package"])
  btw_tool_docs_available_vignettes_impl(package_name)@value
}

#' @export
btw_this.btw_docs_vignettes <- function(x, ...) {
  btw_tool_docs_available_vignettes_impl(package_name = x$package)@value
}

#' @export
btw_this.btw_docs_vignette <- function(x, ...) {
  btw_tool_docs_vignette_impl(
    package_name = x$package,
    vignette = x$vignette %||% x$package
  )@value
}

#' @export
btw_this.vignette <- function(x, ...) {
  btw_tool_docs_vignette_impl(
    package_name = x$Package,
    vignette = x$Topic
  )@value
}

btw_ignore <- function() {
  structure(list(), class = "btw_ignore")
}

btw_user_prompt <- function(...) {
  structure(c(...), class = c("btw_user_prompt"))
}

# https://github.com/RConsortium/S7/issues/501#issuecomment-2494609728
#' @rawNamespace S3method(base::print, btw_ignore)
print.btw_ignore <- function(x, ...) {
  invisible(x)
}

btw_returns_character <- function(...) {
  structure(c(...), class = c("btw_returns_character", "character"))
}

#' @export
btw_this.btw_returns_character <- function(x, ...) {
  capture_print(unclass(x))
}

# Helpers ---------------------------------------------------------------------

get_last_error <- function() {
  # last_error() throws its own error if there aren't any errors yet
  err <- tryCatch(last_error(), error = function(e) NULL)
  if (is.null(err)) {
    cli::cli_warn(c(
      "No last error was found.",
      "i" = "Use {.run ?rlang::global_entrace} to enable {.code @last_error}."
    ))
  }
  err
}

get_last_value <- function() {
  base::.Last.value
}
