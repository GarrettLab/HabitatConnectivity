test_that("get rasters with invalid tif", {
  expect_error(get_rasters("invalid file"))
})

test_that("Valid host parameter", {
  expect_no_error(get_rasters(system.file("ex/logo.tif", package = "terra")))
})