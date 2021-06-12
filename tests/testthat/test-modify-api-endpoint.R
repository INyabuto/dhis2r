test_that("'modify_api_endpoint' well modifies an api link", {
  #expect_equal(modify_api_endpoint(), paste("api",api_version(),"endpoints", sep = "/"))
  expect_equal(modify_api_endpoint(endpoint = "dataElements"), paste("api","dataElements", sep = "/"))
  expect_equal(modify_api_endpoint(endpoint = "dataElements", fields = c("name","id"), order = api_order("name", "asc")),
               paste("api","dataElements","?fields=name,id&order=name:asc", sep = "/"))
  expect_equal(modify_api_endpoint("categories", id = "IDA", ref = "categoryOptions", ref_id = "IDB"), paste(
    "api", "categories", "IDA", "categoryOptions", "IDB", sep = "/"
  ))
})
