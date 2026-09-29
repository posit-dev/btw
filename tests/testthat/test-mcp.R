test_that("btw_mcp_server errors informatively with bad `tools`", {
  local_mocked_bindings(
    mcp_server = function(...) {
      "The test failed, an error should have been raised."
    },
    .package = "mcptools"
  )

  expect_error(
    class = "btw_unmatched_tool_error",
    btw_mcp_server(tools = "bop")
  )
})

test_that("btw_mcp_server works with a character vector of multiple tool groups", {
  local_enable_tools()

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    mcp_server = function(tools, ...) {
      captured$tools <- tools
      "mocked"
    },
    .package = "mcptools"
  )

  expect_equal(btw_mcp_server(tools = c("docs", "env")), "mocked")

  groups <- unique(vapply(
    captured$tools,
    function(tool) tool@annotations$btw_group,
    character(1)
  ))
  expect_setequal(groups, c("docs", "env"))
})

test_that("btw_mcp_server passes an R script path through to mcptools", {
  local_enable_tools()

  # The script is sourced by mcptools::mcp_server(), not by btw_mcp_server(),
  # so here we only assert that the path is handed over unflattened.
  path_tools <- withr::local_tempfile(
    lines = "btw::btw_tools('docs')",
    fileext = ".R"
  )

  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    mcp_server = function(tools, ...) {
      captured$tools <- tools
      "mocked"
    },
    .package = "mcptools"
  )

  expect_equal(btw_mcp_server(tools = path_tools), "mocked")
  expect_equal(captured$tools, path_tools)
})

test_that("btw_mcp_tools() excludes skills group by default", {
  local_enable_tools()
  tools <- btw_mcp_tools()
  tool_names <- vapply(tools, function(t) t@name, character(1))
  expect_false("btw_tool_skill" %in% tool_names)
})
