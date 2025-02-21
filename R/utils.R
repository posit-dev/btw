# ad-hoc check functions ------------------------------------------------------
check_inherits <- function(x, class, x_arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, class)) {
    cli::cli_abort(
      "{.arg {x_arg}} must be a {.cls {class}}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }
}
