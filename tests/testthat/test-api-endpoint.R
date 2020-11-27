test_that("'api_endpoint' well modifies an api link", {
  #expect_equal(modify_api_endpoint(), paste("api",api_version(),"endpoints", sep = "/"))
  expect_equal(modify_api_endpoint(endpoint = "dataElements"), paste("api",api_version(),"dataElements", sep = "/"))
  expect_equal(modify_api_endpoint(endpoint = "dataElements", fields = c("name","id"), order = api_order("name", "asc")),
               paste("api",api_version(),"dataElements","?fields=name,id&order=name:asc", sep = "/"))
  expect_equal(modify_api_endpoint("categories", id = "IDA", ref = "categoryOptions", ref_id = "IDB"), paste(
    "api", api_version(), "categories", "IDA", "categoryOptions", "IDB", sep = "/"
  ))
})
