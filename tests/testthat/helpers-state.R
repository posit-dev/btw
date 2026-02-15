stopifnot(is.null(getOption("btw.client")))
stopifnot(is.null(getOption("btw.subagent.client")))

set_state_inspector(function() {
  list(
    btw_client = getOption("btw.client"),
    btw_subagent_client = getOption("btw.subagent.client")
  )
})
