test_that("btw_tool_result()", {
  expect_s3_class(btw_tool_result("value"), "btw::BtwToolResult")
  expect_s3_class(btw_tool_result("value"), "ellmer::ContentToolResult")

  expect_equal(btw_tool_result("value")@value, "value")

  expect_equal(btw_tool_result("value")@extra$data, NULL)
  expect_equal(btw_tool_result("value", data = "data")@extra$data, "data")

  expect_equal(
    btw_tool_result("value", data = "data", other = "stuff")@extra$other,
    "stuff"
  )
})
