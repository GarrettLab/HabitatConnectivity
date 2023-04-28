library("testthat")
testEnv2 <- new.env()
sys.source(file = "../../CCRI_v2.R", envir = testEnv2, toplevel.env = testEnv2, chdir = TRUE)

test_that("Test 4: Test Senstivity analysis run on default configuration", {
  #testEnv2$kConfigFileFullPath = "../../configurations/parameters.yaml"
  #testEnv2$kZeroRasterFilePath = "../../ZeroRaster.tif"
  # testEnv2$kMapGreyBackGroundTifFilePath = "../../map_grey_background.tif"
  # testEnv2$kUtilitiesDirPath = "../../Utilities"
  # 
  # cat(paste(c(testEnv2$kUtilitiesDirPath, testEnv2$kHelperFileName), collapse = "/"))
  expect_no_condition(testEnv2$SenstivityAnalysis(), message = "Senstivity analysis completed")
})