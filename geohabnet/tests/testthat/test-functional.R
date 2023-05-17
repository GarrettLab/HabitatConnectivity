library("testthat")
library(geohabnet)
# testEnv2 <- new.env()
# testEnv2$TEST_MODE <- TRUE
# 
# sys.source(file = "../../CCRI_v2.R", envir = testEnv2, toplevel.env = testEnv2)

context("functional tests on parameters combination")

test_that("Test 4: Test Senstivity analysis run on default configuration", {
  # kConfigFileFullPath = "../../R/configurations/parameters.yaml"
  # kZeroRasterFilePath = "../../R/Utilities/tifs/ZeroRaster.tif"
  # kMapGreyBackGroundTifFilePath = "../../R/Utilities/tifs/map_grey_background.tif"
  # kHelperFilePath = "../../R/Utilities/ccri_helper.R"
  
  expect_no_condition(geohabnet::SenstivityAnalysis(), message = "Senstivity analysis completed")
})