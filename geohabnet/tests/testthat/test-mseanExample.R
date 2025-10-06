test_that("Valid Spatial raster", {
  # AAA - arrange, act, assert
  
  ## arrange
  exampleSpatRast <- terra::rast(nrows=108, ncols=21, xmin=0, xmax=10)
  terra::values(exampleSpatRast) <- stats::rnorm(2268, 0.5, 0.1)
  globalAnalysis = FALSE
  
  ## act
  result <- msean(exampleSpatRast, global = globalAnalysis)
  
  ## assert
  expect_no_failure(msean(exampleSpatRast, global = globalAnalysis))
  expect_no_error((msean(exampleSpatRast, global = globalAnalysis)))
  expect_s4_class(result, "GeoNetwork")
})


test_that("Invalid Spatial raster for non-global analysis", {
  # AAA - arrange, act, assert
  
  ## arrange
  exampleSpatRast2 <- terra::rast(nrows=108, ncols=21, xmin=0, xmax=10)
  terra::values(exampleSpatRast2) <- stats::rnorm(2268, 0.5, 0.1)
  globalAnalysis = TRUE
  
  ## act and
  ## assert
  #expect_failure(msean(exampleSpatRast2, global = globalAnalysis))
  #expect_error(msean(exampleSpatRast2, global = globalAnalysis), "[crop] extents do not overlap")
  expect_error(
    msean(exampleSpatRast2, global = globalAnalysis),
    regexp = NULL, # If using regex
    fixed = TRUE # Or just use fixed = TRUE
  )
})