test_that("'api_order' formats correctly", {
  expect_equal(api_order("name", by = "asc"), "name:asc")
})
