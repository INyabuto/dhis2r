test_that("'api_endpoint' well generates api links", {
  expect_equal(api_endpoint(), paste("api",api_version(),"resources", sep = "/"))
  expect_equal(api_endpoint(resource = "dataElements"), paste("api",api_version(),"dataElements", sep = "/"))
  expect_equal(api_endpoint(resource = "dataElements", fields = c("name","id"), order = api_order("name", "asc")),
               paste("api",api_version(),"dataElements","?fields=name,id&order=name:asc", sep = "/"))
})
