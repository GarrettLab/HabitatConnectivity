test_that("get rasters with valid file", {
  expect_no_condition(get_rasters(
    list(
      file = c(
        system.file(
        "tifs",
        package = "geohabnet",
        "avocado_HarvestedAreaFraction.tif", mustWork = TRUE
        )))))
})

test_that("get rasters with invalid file", {
  expect_error(get_rasters(list(file = c("invalid file"))))
})
