
# utility functions for CCRI ----------------------------------------------


valid_vector_input <- function(vector_to_check) {
  if (!is.vector(vector_to_check) || length(vector_to_check) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

check_metrics <- function(metrics_list) {
  valid_metrics <- c("betweeness", "node_strength", "sum_of_nearest_neghbors", "eigenvector_centrality")
  is_valid <- lapply(valid_metrics, function(x) x %in% metrics_list)
  names(is_valid) <- valid_metrics
  return(is_valid)
}

