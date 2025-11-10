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

test_that("remove_base64_images() removes base64 images with alt text", {
  html <- '<html><body><img src="data:image/png;base64,iVBORw0KGgoAAAANS" alt="test image"/></body></html>'
  result <- remove_base64_images(html)

  expect_type(result, "character")
  expect_false(grepl("data:image", result))
  expect_true(grepl("\\[Image: test image\\]", result))
  expect_true(grepl("<span>", result))
})

test_that("remove_base64_images() removes base64 images without alt text", {
  html <- '<html><body><img src="data:image/png;base64,iVBORw0KGgoAAAANS"/></body></html>'
  result <- remove_base64_images(html)

  expect_type(result, "character")
  expect_false(grepl("data:image", result))
  expect_true(grepl("\\[Image\\]", result))
  expect_false(grepl("\\[Image: ", result))
})

test_that("remove_base64_images() preserves regular image URLs", {
  html <- '<html><body><img src="https://example.com/image.png" alt="external"/></body></html>'
  result <- remove_base64_images(html)

  expect_true(grepl("https://example.com/image.png", result))
  expect_false(grepl("\\[Image", result))
})

test_that("remove_base64_images() handles multiple images", {
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

test_that("remove_base64_images() handles other data: URI schemes", {
  # Should also catch data:image/svg+xml and other data: URIs
  html <- '<html><body><img src="data:image/svg+xml;base64,PHN2Zy8+" alt="svg"/></body></html>'
  result <- remove_base64_images(html)

  expect_false(grepl("data:image/svg", result))
  expect_true(grepl("\\[Image: svg\\]", result))
})

test_that("remove_base64_images() returns original input on invalid HTML", {
  html <- c("not", "valid", "html")
  result <- remove_base64_images(html)

  expect_identical(result, html)
})

test_that("remove_base64_images() handles HTML with no images", {
  html <- '<html><body><p>Just text, no images</p></body></html>'
  result <- remove_base64_images(html)

  expect_type(result, "character")
  expect_true(grepl("Just text, no images", result))
})

test_that("remove_base64_images() handles empty alt attributes", {
  html <- '<html><body><img src="data:image/png;base64,abc" alt=""/></body></html>'
  result <- remove_base64_images(html)

  # Empty alt text should use generic placeholder
  expect_true(grepl("\\[Image\\]", result))
  expect_false(grepl("\\[Image: \\]", result))
})

# Tests for pandoc_html_simplify() ----------------------------------------

test_that("pandoc_html_simplify() converts HTML to markdown", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  html <- c("<h1>Title</h1>", "<p>Paragraph text</p>")
  result <- pandoc_html_simplify(html)

  expect_type(result, "character")
  expect_true(any(grepl("# Title", result)))
  expect_true(any(grepl("Paragraph text", result)))
})

test_that("pandoc_html_simplify() removes base64 images", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

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

test_that("pandoc_html_simplify() preserves regular images", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  html <- c(
    "<h1>Document</h1>",
    '<img src="https://example.com/photo.jpg" alt="photo"/>'
  )
  result <- pandoc_html_simplify(html)

  # Regular images might be converted to markdown image syntax
  expect_true(any(grepl("example.com/photo.jpg", result)))
})

test_that("pandoc_html_simplify() handles tables", {
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

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
