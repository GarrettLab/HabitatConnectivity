source("Utilities/strings.R")

cat(getwd())

# utility functions for CCRI ----------------------------------------------


valid_vector_input <- function(vector_to_check) {
  if (!is.vector(vector_to_check) || length(vector_to_check) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

check_metrics <- function(metrics_list) {
  valid_metrics <- c("betweeness", "node_strength", "sum_of_nearest_neighbors", "eigenvector_centrality")
  is_valid <- lapply(valid_metrics, function(x) x %in% metrics_list)
  names(is_valid) <- valid_metrics
  return(is_valid)
}

calculate_metrics_weight <- function(betweenness_metric = FALSE, node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE) {
  
  # initialize weights and counters
  weight <- 1
  num_of_metrics <- sum(c(node_strength, sum_of_nearest_neighbors, eigenvector_centrality))
  
  # calculate weights for each metric
  if (betweenness_metric) {
    w1 <- weight/2
    weight <- weight/2
  } else {
    w1 <- 0
  }
  
  w2 <- if (node_strength & num_of_metrics > 0) weight/num_of_metrics else 0
  w3 <- if (sum_of_nearest_neighbors & num_of_metrics > 0) weight/num_of_metrics else 0
  w4 <- if (eigenvector_centrality & num_of_metrics > 0) weight/num_of_metrics else 0
  
  # handle division by zero
  if (w1 == 0) w1 <- NA
  if (w2 == 0) w2 <- NA
  if (w3 == 0) w3 <- NA
  if (w4 == 0) w4 <- NA
  
  # return the weights as a vector
  return(c(STR_BETWEENNESS = w1, STR_NODE_STRENGTH = w2, STR_NEAREST_NEIGHBORS_SUM = w3, STR_EIGEN_VECTOR_CENTRALITY = w4))
}

