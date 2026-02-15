base_url <- "https://raw.githubusercontent.com/inorganik/countUp.js/refs/heads/master/"

pkg_dir <- fs::dir_create("inst/js/app/countUp")

for (file in c("dist/countUp.min.js", "LICENSE.md")) {
  download.file(
    file.path(base_url, file),
    fs::path(pkg_dir, fs::path_file(file)),
    quiet = TRUE
  )
}

# Get version from package.json
pkg_json <- jsonlite::fromJSON(file.path(base_url, "package.json"))
write_lines(
  c(
    sprintf("VERSION: %s", pkg_json$version),
    sprintf("SOURCE: %s", base_url),
    "UPDATE_WITH: scripts/update-countup-js.R"
  ),
  fs::path(pkg_dir, "VERSION")
)
