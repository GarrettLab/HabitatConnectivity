library("testthat")
testEnv2 <- new.env()
testEnv2$TEST_MODE <- TRUE

sys.source(file = "../../CCRI_v2.R", envir = testEnv2, toplevel.env = testEnv2)

test_that("Test 4: Test Senstivity analysis run on default configuration", {
  testEnv2$kConfigFileFullPath = "../../configurations/parameters.yaml"
  testEnv2$kZeroRasterFilePath = "../../ZeroRaster.tif"
  testEnv2$kMapGreyBackGroundTifFilePath = "../../map_grey_background.tif"
  testEnv2$kHelperFilePath = "../../Utilities/ccri_helper.R"
  
  expect_no_condition(testEnv2$SenstivityAnalysis(), message = "Senstivity analysis completed")
})