test_that("get rasters with valid tif", {
  expect_no_condition(get_rasters(list(file = c(.get_helper_filepath("avocado")))))
})

test_that("get rasters with invalid tif", {
  expect_error(get_rasters(list(file = c("invalid file"))))
})
