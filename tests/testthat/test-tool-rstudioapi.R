test_that("btw_tool_ide_read_current_editor()", {
  expect_error(btw_tool_ide_read_current_editor(selection = "yes"))
  expect_error(btw_tool_ide_read_current_editor(consent = "yes"))

  expect_snapshot(
    btw_tool_ide_read_current_editor(),
    error = TRUE
  )

  with_mocked_bindings(
    rstudioapi_has_source_editor_context = function() FALSE,
    {
      expect_snapshot(
        btw_tool_ide_read_current_editor(consent = TRUE),
        error = TRUE
      )

      expect_null(
        .btw_tools[["btw_tool_ide_read_current_editor"]]$tool()
      )
    }
  )
})


mock_source_editor_full <- function() {
  # Open fixtures/test.R
  # mock1 <- rstudioapi_get_source_editor_context()
  # constructive::construct(mock1) |> capture.output() |> parse(text = _) |> format()

  structure(
    list(
      path = test_path("fixtures/test.R"),
      contents = c(
        "library(dplyr)",
        "",
        "mtcars %>%",
        "  group_by(am) %>%",
        "  summarize(average_mpg = mean(mpg))",
        ""
      ),
      selection = structure(
        list(list(
          range = structure(
            list(
              start = structure(
                c(row = 3, column = 11),
                class = "document_position"
              ),
              end = structure(
                c(row = 3, column = 11),
                class = "document_position"
              )
            ),
            class = "document_range"
          ),
          text = ""
        )),
        class = "document_selection"
      )
    ),
    class = "document_context"
  )
}

mock_source_editor_selection <- function() {
  # Open fixtures/test.scss
  # select regions
  # mock2 <- rstudioapi_get_source_editor_context()
  # constructive::construct(mock2) |> capture.output() |> parse(text = _) |> format()

  structure(
    list(
      path = test_path("fixtures/test.scss"),
      contents = c(
        ".one { color: red; }",
        "",
        ".two { color: blue; }",
        "",
        ".three { color: green; }",
        ""
      ),
      selection = structure(
        list(
          list(
            range = structure(
              list(
                start = structure(
                  c(row = 1, column = 1),
                  class = "document_position"
                ),
                end = structure(
                  c(row = 1, column = 21),
                  class = "document_position"
                )
              ),
              class = "document_range"
            ),
            text = ".one { color: red; }"
          ),
          list(
            range = structure(
              list(
                start = structure(
                  c(row = 3, column = 1),
                  class = "document_position"
                ),
                end = structure(
                  c(row = 3, column = 5),
                  class = "document_position"
                )
              ),
              class = "document_range"
            ),
            text = ".two"
          ),
          list(
            range = structure(
              list(
                start = structure(
                  c(row = 5, column = 10),
                  class = "document_position"
                ),
                end = structure(
                  c(row = 5, column = 23),
                  class = "document_position"
                )
              ),
              class = "document_range"
            ),
            text = "color: green;"
          ),
          list(
            range = structure(
              list(
                start = structure(
                  c(row = 4, column = 10),
                  class = "document_position"
                ),
                end = structure(
                  c(row = 4, column = 10),
                  class = "document_position"
                )
              ),
              class = "document_range"
            ),
            text = ""
          )
        ),
        class = "document_selection"
      )
    ),
    class = "document_context"
  )
}

test_that("@current_file", {
  local_mocked_bindings(
    rstudioapi_get_source_editor_context = mock_source_editor_full,
    rstudioapi_has_source_editor_context = function() TRUE
  )

  expect_snapshot(
    cli::cat_line(btw("@current_file"))
  )

  expect_equal(
    btw_this("@current_file"),
    btw_tool_ide_read_current_editor(consent = TRUE)
  )

  expect_equal(
    btw_tool_ide_read_current_editor(selection = TRUE, consent = TRUE),
    btw_tool_ide_read_current_editor(selection = FALSE, consent = TRUE)
  )
})

test_that("@current_selection", {
  local_mocked_bindings(
    rstudioapi_get_source_editor_context = mock_source_editor_selection,
    rstudioapi_has_source_editor_context = function() TRUE
  )

  expect_snapshot(
    cli::cat_line(btw("@current_selection"))
  )

  expect_equal(
    btw_this("@current_selection"),
    btw_tool_ide_read_current_editor(consent = TRUE)
  )
})
