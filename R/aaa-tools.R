.btw_tools <- list()

.btw_add_to_tools <- function(name, group = name, fn) {
  check_string(name)
  check_string(group)
  check_function(fn)

  if (name %in% names(.btw_tools)) {
    cli::cli_abort("Tool names must be unique: {.val {name}}")
  }

  .btw_tools[[name]] <<- list(
    name = name,
    group = group,
    tool = fn
  )

  invisible(fn)
}
