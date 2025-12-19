#' Tools: Register tools from btw
#'
#' @description
#' The `btw_tools()` function provides a list of tools that can be registered
#' with an ellmer chat via `chat$register_tools()` that allow the chat to
#' interface with your computational environment. Chats returned by
#' this function have access to the tools:
#'
#' `r .docs_list_tools()`
#'
#' @examplesIf identical(Sys.getenv("IN_PKGDOWN"), "true")
#' # requires an ANTHROPIC_API_KEY
#' ch <- ellmer::chat_anthropic()
#'
#' # register all of the available tools
#' ch$register_tools(btw_tools())
#'
#' # or register only the tools related to fetching documentation
#' ch$register_tools(btw_tools("docs"))
#'
#' # ensure that the current tools persist
#' ch$register_tools(c(ch$get_tools(), btw_tools()))
#'
#' @param ... Optional names of tools or tool groups to include when registering
#'   tools. By default all btw tools are included. For example, use `"docs"` to
#'   include only the documentation related tools, or `"env", "docs",
#'   "session"` for the collection of environment, documentation and session
#'   tools, and so on.
#'
#'   The names provided can be:
#'
#'   1. The name of a tool, such as `"btw_tool_env_describe_data_frame"`.
#'   2. The name of a tool group, such as `"env"`, which will include all tools
#'      in that group.
#'   3. The tool name without the `btw_tool_` prefix, such as
#'      `"env_describe_data_frame"`.
#'
#' @returns Registers the tools with `chat`, updating the `chat` object in
#'   place. The `chat` input is returned invisibly.
#'
#' @export
btw_tools <- function(...) {
  tools <- c(...)
  check_character(tools, allow_null = TRUE)

  if (length(tools) == 0) {
    withr::local_options(.btw_tools.match_mode = "all")
    tools <- names(.btw_tools)
  } else {
    withr::local_options(.btw_tools.match_mode = "explicit")
    tool_names <- map_chr(.btw_tools, function(x) x$name)
    tool_groups <- map_chr(.btw_tools, function(x) x$group)

    allowed <- c(
      tool_groups,
      tool_names,
      sub("btw_tool_", "", tool_names, fixed = TRUE)
    )
    allowed <- unique(allowed)

    tools <- tryCatch(
      arg_match(tools, allowed[!grepl("^btw_", allowed)], multiple = TRUE),
      error = function(err_short) {
        tryCatch(
          arg_match(tools, allowed, multiple = TRUE),
          error = function(err_long) {
            class(err_short) <- c("btw_unmatched_tool_error", class(err_short))
            cnd_signal(err_short)
          }
        )
      }
    )
  }

  tools_to_keep <- map_lgl(.btw_tools, is_tool_match, tools)
  res <- .btw_tools[tools_to_keep]
  res <- as_ellmer_tools(res)

  tools_can_register <- map_lgl(res, function(tool) {
    is.null(tool@annotations$btw_can_register) ||
      tool@annotations$btw_can_register()
  })

  res[tools_can_register]
}

is_tool_match <- function(tool, labels = NULL) {
  if (is.null(labels)) {
    return(TRUE)
  }
  if (tool$name %in% labels) {
    return(TRUE)
  }
  if (tool$group %in% labels) {
    return(TRUE)
  }
  if (sub("btw_tool_", "", tool$name) %in% labels) {
    return(TRUE)
  }
  FALSE
}

# Convert from .btw_tools (or a filtered version of it)
# to a format compatible with `client$set_tools()`
as_ellmer_tools <- function(x) {
  groups <- map_chr(x, function(.x) .x$group)
  tools <- compact(map(x, function(.x) .x$tool()))
  tools <- map2(tools, groups, set_tool_icon)
  map(tools, wrap_with_intent)
}

wrap_with_intent <- function(tool) {
  if ("_intent" %in% names(tool@arguments@properties)) {
    return(tool)
  }

  tool_fun <- S7::S7_data(tool)
  wrapped_tool <- new_function(
    c(fn_fmls(tool_fun), list(`_intent` = "")),
    fn_body(tool_fun),
    env = fn_env(tool_fun)
  )
  S7::S7_data(tool) <- wrapped_tool
  tool@arguments@properties[["_intent"]] <- ellmer::type_string(
    paste(
      "The intent of the tool call that describes why you called this tool.",
      "This should be a single, short phrase that explains this tool call to the user."
    )
  )

  tool
}

tool_group_icon <- function(group, default = NULL) {
  switch(
    group,
    "agent" = tool_icon("robot"),
    "docs" = tool_icon("dictionary"),
    "env" = tool_icon("source-environment"),
    "eval" = tool_icon("play-circle"),
    "files" = tool_icon("folder-open"),
    "git" = tool_icon("git"),
    "github" = tool_icon("github"),
    "ide" = tool_icon("code-blocks"),
    "pkg" = tool_icon("package"),
    "search" = tool_icon("search"),
    "session" = tool_icon("screen-search-desktop"),
    "web" = tool_icon("globe-book"),
    if (!is.null(default)) tool_icon(default)
  )
}

set_tool_icon <- function(tool, group) {
  if (!is.list(tool@annotations)) {
    tool@annotations <- list()
  }

  tool@annotations$icon <- tool_group_icon(group)
  tool
}

tool_icon <- local({
  icons <- list()
  function(name) {
    if (!is.null(icons[[name]])) {
      return(icons[[name]])
    }

    icon <- HTML(read_file(
      fs::path_package("btw", "icons", paste0(name, ".svg"))
    ))
    icons[[name]] <<- icon
    return(icon)
  }
})

# nocov start
.docs_list_tools <- function() {
  x <- map(.btw_tools, function(tool) {
    desc <- strsplit(S7::prop(tool$tool(), "description"), "[.]( |\n)")
    desc <- desc[[1]][1]

    if (!grepl("[.]$", desc)) {
      desc <- paste0(desc, ".")
    }
    data.frame(
      Name = tool$name,
      Group = tool$group,
      Description = desc,
      stringsAsFactors = FALSE,
      row.names = FALSE
    )
  })
  x <- do.call(rbind, x)
  x <- x[order(x$Group, x$Name), ]
  x$Name <- sprintf("[%s()]", x$Name)

  res <- c()
  for (group in unique(x$Group)) {
    res <- c(res, paste0("### Group: ", group, "\n\n"))
    res <- c(res, md_table(x[x$Group == group, c("Name", "Description")]))
    res <- c(res, "\n\n")
  }
  paste(res, collapse = "\n")
}
# nocov end
