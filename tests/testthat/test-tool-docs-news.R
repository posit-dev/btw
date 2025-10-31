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
  btw_package_news <- package_news

  local_mocked_bindings(
    package_news = local({
      cache <- list()
      function(package_name) {
        # It takes a long time for utils::news() to compile the R news
        if (package_name %in% names(cache)) {
          return(cache[[package_name]])
        }

        (cache[[package_name]] <<- head(btw_package_news(package_name), 2))
      }
    }),
  )

  expect_equal(
    ellmer::contents_text(btw(package_news(package_name = "R"))),
    ellmer::contents_text(btw("@news R"))
  )

  expect_equal(
    I(btw_tool_docs_package_news("R")@value),
    btw_this("@news R")
  )

  expect_btw_tool_result(btw_tool_docs_package_news("R"), has_data = FALSE)
  expect_btw_tool_result(
    btw_tool_docs_package_news("R-3"),
    has_data = FALSE
  )
})

test_that("btw_tool_docs_package_news() when no news is found", {
  local_mocked_bindings(
    package_news = function(...) {
      structure(
        list(
          Version = character(0),
          Date = character(0),
          Category = character(0),
          Text = character(0),
          HTML = character(0)
        ),
        package = "R",
        row.names = integer(0),
        class = c("news_db_from_Rd", "news_db", "data.frame")
      )
    }
  )

  expect_message(
    expect_s3_class(btw(package_news("R")), "btw::btw"),
    "Nothing to include"
  )

  expect_message(
    expect_equal(
      ellmer::contents_text(btw(package_news("R"))),
      ""
    ),
    "Nothing to include"
  )
})

test_that("btw_tool_docs_package_news() with unmatched search term or version", {
  if (package_version("dplyr") != "1.1.1") {
    with_mocked_bindings(
      package_news = function(package_name) {
        news <- utils::news(package = package_name)
        news <- news[news$Version == "1.1.1", ]
        return(news)
      },
      code = {
        expect_error(
          btw_tool_docs_package_news("dplyr"),
          "No NEWS entries"
        )
        expect_error(
          btw("@news dplyr"),
          "No NEWS entries"
        )
      }
    )
  }

  expect_error(
    btw_tool_docs_package_news("dplyr", "asdfsdfgdfghfghj"),
    "No NEWS entries"
  )
  expect_error(
    btw("@news dplyr asdfsdfgdfghfghj"),
    "No NEWS entries"
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
