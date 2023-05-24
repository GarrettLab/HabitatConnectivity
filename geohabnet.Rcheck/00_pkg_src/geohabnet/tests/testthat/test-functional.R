library("testthat")

test_that("Test function to retreive crop raster data", {
  expect_no_condition(geohabnet::getCropHarvestRaster("avocado"))
})

test_that("Test Senstivity analysis run on default configuration", {
  
  expect_no_condition(geohabnet::SenstivityAnalysis(), message = "Senstivity analysis completed")
})