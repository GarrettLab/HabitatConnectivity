
test_that("System test - Test function to retreive crop raster data", {
  expect_no_condition(geohabnet::cropharvest_rast("avocado", "monfreda"))
})

test_that("System test - Test Senstivity analysis run on default configuration", {
  expect_no_condition(geohabnet::sensitivity_analysis(), message = "Sensitivity analysis completed")
})

test_that("API test - crop search", {
  ret <- geohabnet::search_crop("wheat")
  expect_equal(ret, c("monfreda", "spam"))
  expect_no_condition(geohabnet::search_crop("wheat"), message = "testing API")
})
