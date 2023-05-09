library(testthat)
library(this.path)

testEnv3 <- new.env()
testEnv3$TEST_MODE <- TRUE

test_filename <- paste(this.dir(), "../../Utilities/ccri_helper.R", sep = "/")
sys.source(file = test_filename, envir = testEnv3, toplevel.env = testEnv3)

# Define the test cases
test_that("calculate_metrics_weight returns correct weights", {
  expect_equal(
    testEnv3$calculate_metrics_weight(node_strength = TRUE),
    c(STR_BETWEENNESS = 0, STR_NODE_STRENGTH = 1, STR_NEAREST_NEIGHBORS_SUM = 0, STR_EIGEN_VECTOR_CENTRALITY = 0)
  )
  
  expect_equal(
    testEnv3$calculate_metrics_weight(sum_of_nearest_neighbors = TRUE),
    c(STR_BETWEENNESS = 0, STR_NODE_STRENGTH = 0, STR_NEAREST_NEIGHBORS_SUM = 1, STR_EIGEN_VECTOR_CENTRALITY = 0)
  )
  
  expect_equal(
    testEnv3$calculate_metrics_weight(eigenvector_centrality = TRUE),
    c(STR_BETWEENNESS = 0, STR_NODE_STRENGTH = 0, STR_NEAREST_NEIGHBORS_SUM = 0, STR_EIGEN_VECTOR_CENTRALITY = 1)
  )
  
  expect_equal(
    testEnv3$calculate_metrics_weight(node_strength = TRUE, sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE),
    c(STR_BETWEENNESS = 0, STR_NODE_STRENGTH = 3, STR_NEAREST_NEIGHBORS_SUM = 3, STR_EIGEN_VECTOR_CENTRALITY = 3)
  )
  
  expect_equal(
    testEnv3$calculate_metrics_weight(betweenness_metric = TRUE, sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE),
    c(STR_BETWEENNESS = 2, STR_NODE_STRENGTH = 0, STR_NEAREST_NEIGHBORS_SUM = 4, STR_EIGEN_VECTOR_CENTRALITY = 4)
  )
  
  expect_equal(
    testEnv3$calculate_metrics_weight(betweenness_metric = TRUE, node_strength = TRUE, sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE),
    c(STR_BETWEENNESS = 2, STR_NODE_STRENGTH = 6, STR_NEAREST_NEIGHBORS_SUM = 6, STR_EIGEN_VECTOR_CENTRALITY = 6)
  )
})
