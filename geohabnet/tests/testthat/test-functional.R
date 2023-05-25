library("testthat")

test_that("System test - Test function to retreive crop raster data", {
  expect_no_condition(geohabnet::get_cropharvest_raster("avocado"))
})

test_that("System test - Test Senstivity analysis run on default configuration", {
  expect_no_condition(geohabnet::link_threshold(), message = "Senstivity analysis completed")
})