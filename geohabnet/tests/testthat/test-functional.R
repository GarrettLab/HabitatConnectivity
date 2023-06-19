library("testthat")

test_that("System test - Test function to retreive crop raster data", {
  expect_no_condition(geohabnet::get_cropharvest_raster("avocado", "monfreda"))
})

test_that("System test - Test Senstivity analysis run on default configuration", {
  expect_no_condition(geohabnet::senstivity_analysis(), message = "Senstivity analysis completed")
})
