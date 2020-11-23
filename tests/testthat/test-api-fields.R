test_that("fields are well formatted", {
  expect_equal(api_fields("name"), "name")
  expect_equal(api_fields("name", organisationUnits = c("name","id")), "name,organisationUnits[name,id]")
})
