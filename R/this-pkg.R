# scope: just the readme, the intro vignette, or all of the functions
this_pkg <- function(x, topics = c("readme", "intro"), ..., clipboard = interactive()) {
  # pkg_prompt sort of thing, (though that code is quite buggy)
  # https://github.com/simonpcouch/pkgprompt

  if (clipboard && !in_btw()) {
    write_to_clipboard(res)
  }

  invisible(res)
}
