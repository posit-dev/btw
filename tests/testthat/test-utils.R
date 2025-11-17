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

# Tests for simplify_help_page_arguments() ----------------------------------------

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
    expect_false(grepl('<table role="presentation">', result, fixed = TRUE))
    expect_true(grepl("<h3><code>x</code></h3>", result, fixed = TRUE))
    expect_true(grepl("<h3><code>y</code></h3>", result, fixed = TRUE))
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
    expect_true(grepl('<table role="presentation">', result, fixed = TRUE))
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

    expect_true(grepl("<h3><code>.f</code></h3>", result, fixed = TRUE))
    expect_true(grepl("<ul>", result))
    expect_true(grepl("<li>Named function</li>", result))
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

    expect_true(grepl("<h3><code>x</code></h3>", result, fixed = TRUE))
    expect_false(grepl("<h3>Not a param</h3>", result))
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

    expect_false(grepl("<h3><code>x</code></h3>", result))
    expect_true(grepl("<h3><code>y</code></h3>", result, fixed = TRUE))
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

    expect_true(grepl(
      "<h3><code>simple</code></h3>",
      result,
      fixed = TRUE
    ))
    expect_true(grepl("<p>Plain text description.</p>", result, fixed = TRUE))
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
    expect_true(grepl("<h3><code>x</code></h3>", result, fixed = TRUE))
    # Value table should not be transformed
    expect_true(grepl('<td><code>y</code></td>', result, fixed = TRUE))
  })
})
