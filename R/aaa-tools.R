.btw_tools <- list()

.btw_add_to_tools <- function(name, group = name, tool) {
  check_string(name)
  check_string(group)
  if (!is_function(tool)) {
    abort(
      "`tool` must be a function to ensure `ellmer::tool()` is called at run time."
    )
  }

  if (name %in% names(.btw_tools)) {
    cli::cli_abort("Tool names must be unique: {.val {name}}")
  }

  .btw_tools[[name]] <<- list(
    name = name,
    group = group,
    tool = tool
  )

  invisible(tool)
}

contents_shinychat <- S7::new_external_generic(
  package = "shinychat",
  name = "contents_shinychat",
  dispatch_args = "content"
)
