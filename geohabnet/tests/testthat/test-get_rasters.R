test_that("get rasters with invalid tif", {
  expect_error(get_rasters(list(file = c("invalid file"))))
})
