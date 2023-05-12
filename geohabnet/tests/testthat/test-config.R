library("testthat")
library(geohabnet)
# testEnv <- new.env()
# sys.source(file = "../../R/CCRI_v2.R", envir = testEnv, toplevel.env = testEnv)


# Tests for Loading parameters via configuration file ---------------------
context("parameters configuration tests")

test_that("Test 1: Loading default parameters configuration", {
  default_file <- "../../R/configurations/parameters.yaml"
  expect_true(file.exists(default_file))
  expect_no_condition(LoadConfig(filePath = default_file), message = "Default configuration exists and loaded")
})

test_that("Test 2: Loading custom parameters configuration", {
  custom_file <- "../../R/configurations/parameters.yaml"
  expect_true(file.exists(custom_file))
  expect_no_condition(LoadConfig(filePath = custom_file), message = "Loaded custom parameters - parameters.yaml")
})

test_that("Test 2: Loading invalid parameters configuration", {
  wrong_path <- "some random path that does not exist"
  expect_false(file.exists(wrong_path))
  expect_error(LoadConfig(filePath = wrong_path), message = "Failed. to load parameters configuration from provided path")
})