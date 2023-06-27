library(testthat)

test_that("get helper files", {
  expect_true(file.exists(.get_helper_filepath("parameters")))
  expect_true(file.exists(.get_helper_filepath("zero_raster")))
  expect_true(file.exists(.get_helper_filepath("map_grey_background")))
})
