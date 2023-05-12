library(testthat)
library(this.path)

testEnv3 <- new.env()

test_filename <- paste(this.dir(), "../../R/Utilities/ccri_helper.R", sep = "/")
sys.source(file = test_filename, envir = testEnv3, toplevel.env = testEnv3)

context("utilities function tests")

test_that("calculate_metrics_weight returns correct weights", {
  
  expect_equal(
    unname(testEnv3$calculate_metrics_weight(node_strength = TRUE)),
    c(0, 1, 0, 0)
  )
  
  expect_equal(
    unname(testEnv3$calculate_metrics_weight(sum_of_nearest_neighbors = TRUE)),
    c(0, 0, 1, 0)
  )
  
  expect_equal(
    unname(testEnv3$calculate_metrics_weight(eigenvector_centrality = TRUE)),
    c(0, 0, 0, 1)
  )
  
  expect_equal(
    unname(testEnv3$calculate_metrics_weight(node_strength = TRUE, sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE)),
    c(0, 3, 3, 3)
  )
  
  expect_equal(
    unname(testEnv3$calculate_metrics_weight(betweenness_metric = TRUE, sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE)),
    c(2, 0, 4, 4)
  )
  
  expect_equal(
    unname(testEnv3$calculate_metrics_weight(betweenness_metric = TRUE, node_strength = TRUE, sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE)),
    c(2, 6, 6, 6)
  )
})
