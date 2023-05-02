library(this.path)

source(paste(this.dir(), "strings.R", sep = "/"))

# utility functions for CCRI ----------------------------------------------


valid_vector_input <- function(vector_to_check) {
  if (!is.vector(vector_to_check) || length(vector_to_check) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

check_metrics <- function(metrics_list) {
  valid_metrics <- c(STR_BETWEENNESS, STR_NODE_STRENGTH, STR_NEAREST_NEIGHBORS_SUM, STR_EIGEN_VECTOR_CENTRALITY)
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
  if (w1 != 0) w1 <- as.integer(1/w1)
  if (w2 != 0) w2 <- as.integer(1/w2)
  if (w3 != 0) w3 <- as.integer(1/w3)
  if (w4 != 0) w4 <- as.integer(1/w4)
  
  weights <- c(w1, w2, w3, w4)
  names(weights) <- c(STR_BETWEENNESS, STR_NODE_STRENGTH, STR_NEAREST_NEIGHBORS_SUM, STR_EIGEN_VECTOR_CENTRALITY)
  
  # return the weights as a vector
  return(weights)
}

