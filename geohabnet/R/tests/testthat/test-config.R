library("testthat")
testEnv <- new.env()
sys.source(file = "../../CCRI_v2.R", envir = testEnv, toplevel.env = testEnv)


# Tests for Loading parameters via configuration file ---------------------

test_that("Test 1: Loading default parameters configuration", {
  default_file <- "../../configurations/parameters.yaml"
  expect_no_condition(testEnv$LoadConfig(filePath = default_file), message = "Default configuration exists and loaded")
})

test_that("Test 2: Loading custom parameters configuration", {
  custom_file <- "../../configurations/parameters.yaml"
  expect_no_condition(testEnv$LoadConfig(filePath = custom_file), message = "Loaded custom parameters - parameters.yaml")
})

test_that("Test 2: Loading invalid parameters configuration", {
  wrong_path <- "some random path that does not exist"
  expect_error(testEnv$LoadConfig(filePath = wrong_path), message = "Failed. to load parameters configuration from provided path")
})