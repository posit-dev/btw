test_that("md_table()", {
  expect_snapshot(cat(md_table(mtcars[1:4, 1:4])))
})

test_that("md_code_block()", {
  expect_snapshot(md_code_block("markdown", c("```r", "runif(1)", "```")))

  expect_snapshot(md_code_block("markdown", c("````r", "runif(1)", "````")))
})

test_that("escape_markdown_html() escapes raw html outside code", {
  expect_identical(
    escape_markdown_html("<script>alert(1)</script> hi"),
    "&lt;script&gt;alert(1)&lt;/script&gt; hi"
  )
  expect_identical(escape_markdown_html("```\nx <- 1\n```"), "```\nx <- 1\n```")
  expect_identical(escape_markdown_html("```r\nx <- 1\n```"), "```r\nx <- 1\n```")
  expect_identical(
    escape_markdown_html("`x <- 1` plain <b>"),
    "`x <- 1` plain &lt;b&gt;"
  )
  expect_identical(
    escape_markdown_html("`a` <script>alert(1)</script> `b`"),
    "`a` &lt;script&gt;alert(1)&lt;/script&gt; `b`"
  )
  expect_identical(
    escape_markdown_html("a `` <script>alert(1)</script> ` b"),
    "a `` &lt;script&gt;alert(1)&lt;/script&gt; ` b"
  )
  expect_identical(escape_markdown_html("`` `<div>` ``"), "`` `<div>` ``")
  expect_identical(escape_markdown_html("~~~\nx <- 1\n~~~"), "~~~\nx <- 1\n~~~")
  expect_identical(
    escape_markdown_html("```\ncode\n````\nafter <b>"),
    "```\ncode\n````\nafter &lt;b&gt;"
  )
})
