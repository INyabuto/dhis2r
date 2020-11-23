test_that("'api_filter' works well", {
  expect_equal(api_filter("name", "eq", "TZ FP A360 - Conversion rate < 20"), "name:eq:TZ FP A360 - Conversion rate < 20")
  expect_equal(api_filter("name", "ilike", "TZ FP A360 - Conversion rate < 20"), "name:ilike:TZ FP A360 - Conversion rate < 20")
})
