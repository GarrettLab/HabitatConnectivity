library(testthat)

test_that("get helper files", {
  expect_true(file.exists(.get_helper_filepath(.kparameters_file_type)))
  expect_true(file.exists(.get_helper_filepath(.kzeroraster_fname)))
  expect_true(file.exists(.get_helper_filepath(.kmapgreybackground_fname)))
})
