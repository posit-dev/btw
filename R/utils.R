# ad-hoc check functions ------------------------------------------------------
check_inherits <- function(x, class, x_arg = caller_arg(x), call = caller_env()) {
  if (!inherits(x, class)) {
    cli::cli_abort(
      "{.arg {x_arg}} must be a {.cls {class}}, not {.obj_type_friendly {x}}.",
      call = call
    )
  }

  invisible(NULL)
}

# rlang's version prompts when interactive
check_installed <- function(x, call = caller_env()) {
  if (!is_installed(x)) {
    cli::cli_abort("Package {.pkg {x}} is not installed.", call = call)
  }

  invisible(NULL)
}

interactive <- NULL
