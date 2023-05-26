library("testthat")

# Tests for Loading parameters via configuration file ---------------------

test_that("Test 1: Loading default parameters configuration", {
  default_file <- .get_helper_filepath("parameters")
  expect_true(file.exists(default_file))
  expect_no_condition(load_parameters(filepath = default_file), message = "Default configuration exists and loaded")
})

test_that("Test 2: Loading custom parameters configuration", {
  custom_file <- .get_helper_filepath("parameters")
  expect_true(file.exists(custom_file))
  expect_no_condition(load_parameters(filepath = custom_file), message = "Loaded custom parameters - parameters.yaml")
})

test_that("Test 3: Loading invalid parameters configuration", {
  wrong_path <- "some random path that does not exist"
  expect_false(file.exists(wrong_path))
  expect_error(LoadParameters(filepath = wrong_path),
               message = "Failed to load parameters configuration from provided path")
})

test_that("Test 4: Test with default parameters", {
  param_file <- get_parameters()
  expect_true(file.exists(param_file))
  file.remove(param_file)
})

test_that("Test 5: Test to fetch currently used parameters.yaml", {
  save_path <- getwd()
  param_file <- get_parameters(out_path = save_path)
  expect_true(file.exists(param_file))
  file.remove(param_file)
})

test_that("Test 6: Test to set new parameters.yaml", {
  new_param_file <- "params.yaml"
  after_setting_new_param <- .get_helper_filepath("parameters")
  expect_no_condition(set_parameters(new_param_file))
  expect_true(file.exists(after_setting_new_param))
})
