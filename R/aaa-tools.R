.btw_tools <- list()

.btw_add_to_tools <- function(
    name,
    group = name,
    tool,
    can_register = NULL,
    alias_group = NULL,
    alias_name = NULL
) {
  check_string(name)
  check_string(group)
  check_function(can_register, allow_null = TRUE)
  check_character(alias_group, allow_null = TRUE)
  check_character(alias_name, allow_null = TRUE)

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
    tool = tool,
    can_register = can_register,
    alias_group = alias_group,
    alias_name = alias_name
  )

  invisible(tool)
}

contents_shinychat <- S7::new_external_generic(
  package = "shinychat",
  name = "contents_shinychat",
  dispatch_args = "content"
)
