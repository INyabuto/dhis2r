test_that("'queries' are well parsed", {
  expect_error(api_query(fields = "name", "id"))
  expect_equal(api_query(filter="name", fields = api_fields("name","id")), paste0("?", "filter=name", "&fields=name,id"))
})
