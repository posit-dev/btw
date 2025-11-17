use_latest_pandoc()

test_that("btw_tool_docs_package_help_topics() works", {
  res <- btw_tool_docs_package_help_topics("stats")

  expect_btw_tool_result(res)
  expect_match(res@value, '"topic_id":"Normal"', fixed = TRUE, all = FALSE)
})

test_that("btw_tool_docs_help_page() works", {
  res <- btw_tool_docs_help_page(package_name = "stats", topic = "rnorm")

  expect_btw_tool_result(res, has_data = FALSE)
  expect_equal(res@extra$topic, "Normal")
  expect_equal(res@extra$package, "stats")
  expect_type(res@extra$help_text, "character")

  skip_if_not_snapshot_env()
  expect_snapshot(cli::cat_line(res@value))
})

test_that("btw_tool_docs_help_page() with unknown topic", {
  expect_snapshot(error = TRUE, {
    btw_tool_docs_help_page("unknown-topic", "dplyr")
    btw_tool_docs_help_page("unknown-topic")
  })
})

test_that("btw_tool_docs_available_vignettes() works", {
  skip_on_cran()

  res <- btw_tool_docs_available_vignettes("dplyr")

  expect_btw_tool_result(res)

  expect_match(res@value, '"vignette":"dplyr"', fixed = TRUE, all = FALSE)
  expect_match(
    res@value,
    '"title":"Programming with dplyr"',
    fixed = TRUE,
    all = FALSE
  )

  expect_equal(
    btw_tool_docs_available_vignettes("dplyr")@value,
    btw_this(vignette(package = "dplyr"))
  )

  expect_true(
    jsonlite::validate(
      btw_tool_docs_available_vignettes("dplyr")@value
    )
  )
})

test_that("btw_tool_docs_vignette() works", {
  skip_on_cran()

  res_dplyr <- btw_tool_docs_vignette("dplyr")
  expect_btw_tool_result(res_dplyr)
  expect_match(
    res_dplyr@value,
    "Introduction to dplyr",
    fixed = TRUE,
    all = FALSE
  )
  expect_equal(btw_this(vignette("dplyr", "dplyr")), res_dplyr@value)

  res_prog <- btw_tool_docs_vignette("dplyr", "programming")
  expect_btw_tool_result(res_prog)
  expect_match(res_prog@value, "Programming", fixed = TRUE, all = FALSE)
  expect_equal(btw_this(vignette("programming", "dplyr")), res_prog@value)
})

test_that("btw_tool_docs_help_page() with multiple help topics", {
  skip_if_not_installed("dplyr")

  expect_snapshot(
    error = TRUE,
    btw_tool_docs_help_page("filter")
  )
})

# simplify_help_page_arguments() ----------------------------------------

describe("simplify_help_page_arguments()", {
  it("converts argument tables to heading format", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td><td><p>A numeric vector.</p></td></tr>',
      '<tr><td><code>y</code></td><td><p>Another vector.</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    expect_type(result, "character")
    expect_no_match(result, '<table role="presentation">', fixed = TRUE)
    expect_match(result, "<h4><code>x</code></h4>", fixed = TRUE)
    expect_match(result, "<h4><code>y</code></h4>", fixed = TRUE)
  })

  it("preserves tables without Arguments section", {
    html <- paste0(
      '<html><body>',
      '<h3 id="value">Value</h3>',
      '<table role="presentation">',
      '<tr><td>Data</td><td>Value</td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    # Should not transform non-Arguments tables
    expect_match(result, '<table role="presentation">', fixed = TRUE)
  })

  it("preserves full HTML structure including lists", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>.f</code></td>',
      '<td><p>A function:</p><ul><li>Named function</li><li>Anonymous function</li></ul></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    expect_match(result, "<h4><code>.f</code></h4>", fixed = TRUE)
    expect_match(result, "<ul>", fixed = TRUE)
    expect_match(result, "<li>Named function</li>", fixed = TRUE)
  })

  it("handles rows without code tags", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td><td><p>Valid param</p></td></tr>',
      '<tr><td>Not a param</td><td>Should be skipped</td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    expect_match(result, "<h4><code>x</code></h4>", fixed = TRUE)
    expect_no_match(result, "<h4>Not a param</h4>", fixed = TRUE)
  })

  it("handles empty tables", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    expect_type(result, "character")
  })

  it("handles tables with single cell rows", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td></tr>',
      '<tr><td><code>y</code></td><td><p>Valid</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    expect_no_match(result, "<h4><code>x</code></h4>", fixed = TRUE)
    expect_match(result, "<h4><code>y</code></h4>", fixed = TRUE)
  })

  it("handles descriptions without paragraph tags", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>simple</code></td><td>Plain text description.</td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    expect_match(result, "<h4><code>simple</code></h4>", fixed = TRUE)
    expect_match(result, "<p>Plain text description.</p>", fixed = TRUE)
  })

  it("returns original input on invalid HTML", {
    html <- c("not", "valid", "html")
    result <- simplify_help_page_arguments(html)

    expect_identical(result, html)
  })

  it("handles HTML with no Arguments section", {
    html <- '<html><body><p>Just text, no Arguments section</p></body></html>'
    result <- simplify_help_page_arguments(html)

    expect_type(result, "character")
    expect_true(grepl("Just text, no Arguments section", result))
  })

  it("only transforms first table in Arguments section", {
    html <- paste0(
      '<html><body>',
      '<h3 id="arguments">Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td><td><p>First param</p></td></tr>',
      '</table>',
      '<h3 id="value">Value</h3>',
      '<table role="presentation">',
      '<tr><td><code>y</code></td><td><p>Should not transform</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_page_arguments(html)

    # Arguments table should be transformed
    expect_match(result, "<h4><code>x</code></h4>", fixed = TRUE)
    # Value table should not be transformed
    expect_match(result, '<td><code>y</code></td>', fixed = TRUE)
  })
})
