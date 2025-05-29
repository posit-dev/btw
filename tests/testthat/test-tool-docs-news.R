test_that("btw_tool_docs_package_news()", {
  expect_equal(
    ellmer::contents_text(btw(news(package = "dplyr"))),
    ellmer::contents_text(btw("@news dplyr"))
  )

  expect_equal(
    I(btw_tool_docs_package_news("dplyr")@value),
    btw_this("@news dplyr")
  )

  expect_btw_tool_result(btw_tool_docs_package_news("dplyr"), has_data = FALSE)
})

test_that("btw_tool_docs_package_news() with search term", {
  expect_equal(
    I(btw_tool_docs_package_news("dplyr", "filter")@value),
    btw_this("@news dplyr filter")
  )

  expect_btw_tool_result(
    btw_tool_docs_package_news("dplyr", "filter"),
    has_data = FALSE
  )
})

test_that("btw_tool_docs_package_news() with non-existent package", {
  expect_error(
    btw_tool_docs_package_news("nonexistentpackage"),
    "not installed"
  )
})

test_that("btw_tool_docs_package_news() with R package", {
  r_major <- sprintf("R-%s", R.version$major)
  news_r_major <- sprintf("@news %s", r_major)

  local_mocked_bindings(
    package_news = local({
      cache <- list()
      function(package_name) {
        # It takes a long time for utils::news() to compile the R news
        if (package_name %in% names(cache)) {
          return(cache[[package_name]])
        }

        (cache[[package_name]] <<- head(utils::news(package = package_name), 2))
      }
    }),
  )

  expect_equal(
    ellmer::contents_text(btw(package_news(package_name = "R"))),
    ellmer::contents_text(btw(!!news_r_major))
  )

  expect_equal(
    I(btw_tool_docs_package_news("R")@value),
    btw_this(!!news_r_major)
  )

  expect_btw_tool_result(btw_tool_docs_package_news("R"), has_data = FALSE)
  expect_btw_tool_result(
    btw_tool_docs_package_news(r_major),
    has_data = FALSE
  )
  expect_equal(
    btw_tool_docs_package_news("R"),
    btw_tool_docs_package_news(r_major)
  )
})

test_that("btw_tool_docs_package_news() snapshots", {
  skip_if_not_macos()

  local_mocked_bindings(
    package_version = function(package_name) {
      if (package_name == "dplyr") {
        return("1.1.4")
      }
      stop("Unknown package")
    },
    package_news = function(package_name) {
      if (package_name == "dplyr") {
        news <- utils::news(package = "dplyr")
        news <- news[news$Version == "1.1.4", ]
        return(news)
      }
      stop("Unknown package")
    }
  )

  expect_snapshot(print(btw("@news dplyr")))
  expect_snapshot(print(btw("@news dplyr join")))
})
