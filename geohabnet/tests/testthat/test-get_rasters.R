test_that("Valid host parameter", {
  expect_no_error(get_rasters(system.file("ex/logo.tif", package = "terra")))
})