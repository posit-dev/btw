test_that("check_installed() works", {
  # returns NULL invisibly if installed
  local_mocked_bindings(is_installed = function(x) TRUE)
  expect_invisible(check_installed("somepackage"))
  expect_null(check_installed("somepackage"))

  skip_if_not_macos()

  # informative error if not installed
  local_mocked_bindings(
    is_installed = function(x) FALSE,
    find_package_candidates = function(...) {
      c("doudpackage", "datapackage", "findPackage", "FSTpackage", "somspace")
    }
  )
  expect_snapshot(check_installed("somepackage"), error = TRUE)
})

test_that("check_inherits() works", {
  expect_null(check_inherits("true", "character"))
  expect_null(check_inherits(as_btw_capture("foo"), "btw_captured"))
  expect_error(check_inherits(as_btw_docs_package("btw"), "not_this_class"))
})

# Tests for remove_base64_images() -----------------------------------------

describe("remove_base64_images()", {
  it("removes base64 images with alt text", {
    html <- '<html><body><img src="data:image/png;base64,iVBORw0KGgoAAAANS" alt="test image"/></body></html>'
    result <- remove_base64_images(html)

    expect_type(result, "character")
    expect_false(grepl("data:image", result))
    expect_true(grepl("\\[Image: test image\\]", result))
    expect_true(grepl("<span>", result))
  })

  it("removes base64 images without alt text", {
    html <- '<html><body><img src="data:image/png;base64,iVBORw0KGgoAAAANS"/></body></html>'
    result <- remove_base64_images(html)

    expect_type(result, "character")
    expect_false(grepl("data:image", result))
    expect_true(grepl("\\[Image\\]", result))
    expect_false(grepl("\\[Image: ", result))
  })

  it("preserves regular image URLs", {
    html <- '<html><body><img src="https://example.com/image.png" alt="external"/></body></html>'
    result <- remove_base64_images(html)

    expect_true(grepl("https://example.com/image.png", result))
    expect_false(grepl("\\[Image", result))
  })

  it("handles multiple images", {
    html <- paste0(
      '<html><body>',
      '<img src="data:image/png;base64,abc" alt="first"/>',
      '<img src="https://example.com/img.png" alt="second"/>',
      '<img src="data:image/jpeg;base64,xyz" alt="third"/>',
      '</body></html>'
    )
    result <- remove_base64_images(html)

    expect_false(grepl("data:image/png", result))
    expect_false(grepl("data:image/jpeg", result))
    expect_true(grepl("\\[Image: first\\]", result))
    expect_true(grepl("\\[Image: third\\]", result))
    expect_true(grepl("https://example.com/img.png", result))
  })

  it("handles other data: URI schemes", {
    # Should also catch data:image/svg+xml and other data: URIs
    html <- '<html><body><img src="data:image/svg+xml;base64,PHN2Zy8+" alt="svg"/></body></html>'
    result <- remove_base64_images(html)

    expect_false(grepl("data:image/svg", result))
    expect_true(grepl("\\[Image: svg\\]", result))
  })

  it("returns original input on invalid HTML", {
    html <- c("not", "valid", "html")
    result <- remove_base64_images(html)

    expect_identical(result, html)
  })

  it("handles HTML with no images", {
    html <- '<html><body><p>Just text, no images</p></body></html>'
    result <- remove_base64_images(html)

    expect_type(result, "character")
    expect_true(grepl("Just text, no images", result))
  })

  it("handles empty alt attributes", {
    html <- '<html><body><img src="data:image/png;base64,abc" alt=""/></body></html>'
    result <- remove_base64_images(html)

    # Empty alt text should use generic placeholder
    expect_true(grepl("\\[Image\\]", result))
    expect_false(grepl("\\[Image: \\]", result))
  })
})

# Tests for pandoc_html_simplify() ----------------------------------------

describe("pandoc_html_simplify()", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  it("converts HTML to markdown", {
    html <- c("<h1>Title</h1>", "<p>Paragraph text</p>")
    result <- pandoc_html_simplify(html)

    expect_type(result, "character")
    expect_true(any(grepl("# Title", result)))
    expect_true(any(grepl("Paragraph text", result)))
  })

  it("removes base64 images", {
    html <- c(
      "<h1>Document</h1>",
      '<img src="data:image/png;base64,iVBORw0KGgo" alt="chart"/>',
      "<p>Text after image</p>"
    )
    result <- pandoc_html_simplify(html)

    expect_false(any(grepl("data:image", result)))
    # Pandoc escapes brackets in markdown, so check for the content
    expect_true(any(grepl("Image", result)))
    expect_true(any(grepl("chart", result)))
  })

  it("preserves regular images", {
    html <- c(
      "<h1>Document</h1>",
      '<img src="https://example.com/photo.jpg" alt="photo"/>'
    )
    result <- pandoc_html_simplify(html)

    # Regular images might be converted to markdown image syntax
    expect_true(any(grepl("example.com/photo.jpg", result)))
  })

  it("handles tables", {
    html <- c(
      "<table>",
      "<tr><th>Col1</th><th>Col2</th></tr>",
      "<tr><td>A</td><td>B</td></tr>",
      "</table>"
    )
    result <- pandoc_html_simplify(html)

    # Should use pipe tables format
    expect_true(any(grepl("\\|", result)))
  })
})

# Tests for simplify_help_tables() ----------------------------------------

describe("simplify_help_tables()", {
  it("simplifies argument tables to paragraph format", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td><td><p>A numeric vector.</p></td></tr>',
      '<tr><td><code>y</code></td><td><p>Another vector.</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_type(result, "character")
    expect_false(grepl('<table role="presentation">', result, fixed = TRUE))
    expect_true(grepl(
      "<code>x</code>: A numeric vector.",
      result,
      fixed = TRUE
    ))
    expect_true(grepl("<code>y</code>: Another vector.", result, fixed = TRUE))
  })

  it("preserves tables without role='presentation'", {
    html <- paste0(
      '<html><body>',
      '<table>',
      '<tr><td>Data</td><td>Value</td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_true(grepl("<table>", result))
  })

  it("handles multiple paragraphs in description", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '<tr><td><code>data</code></td>',
      '<td><p>First paragraph.</p><p>Second paragraph.</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_true(grepl(
      "<code>data</code>: First paragraph. Second paragraph.",
      result,
      fixed = TRUE
    ))
  })

  it("handles rows without code tags", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td><td><p>Valid param</p></td></tr>',
      '<tr><td>Not a param</td><td>Should be skipped</td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_true(grepl("<code>x</code>: Valid param", result, fixed = TRUE))
    expect_false(grepl("Not a param", result))
  })

  it("normalizes whitespace in descriptions", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '<tr><td><code>param</code></td>',
      '<td><p>Text with\n  multiple   spaces\nand newlines.</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_true(grepl(
      "<code>param</code>: Text with multiple spaces and newlines.",
      result,
      fixed = TRUE
    ))
  })

  it("handles empty tables", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_type(result, "character")
  })

  it("handles tables with single cell rows", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td></tr>',
      '<tr><td><code>y</code></td><td><p>Valid</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_false(grepl("<code>x</code>:", result))
    expect_true(grepl("<code>y</code>: Valid", result, fixed = TRUE))
  })

  it("handles descriptions without paragraph tags", {
    html <- paste0(
      '<html><body>',
      '<table role="presentation">',
      '<tr><td><code>simple</code></td><td>Plain text description.</td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_true(grepl(
      "<code>simple</code>: Plain text description.",
      result,
      fixed = TRUE
    ))
  })

  it("returns original input on invalid HTML", {
    html <- c("not", "valid", "html")
    result <- simplify_help_tables(html)

    expect_identical(result, html)
  })

  it("handles HTML with no argument tables", {
    html <- '<html><body><p>Just text, no tables</p></body></html>'
    result <- simplify_help_tables(html)

    expect_type(result, "character")
    expect_true(grepl("Just text, no tables", result))
  })

  it("handles multiple argument tables", {
    html <- paste0(
      '<html><body>',
      '<h3>Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>x</code></td><td><p>First param</p></td></tr>',
      '</table>',
      '<h3>More Arguments</h3>',
      '<table role="presentation">',
      '<tr><td><code>y</code></td><td><p>Second param</p></td></tr>',
      '</table>',
      '</body></html>'
    )
    result <- simplify_help_tables(html)

    expect_true(grepl("<code>x</code>: First param", result, fixed = TRUE))
    expect_true(grepl("<code>y</code>: Second param", result, fixed = TRUE))
  })
})
