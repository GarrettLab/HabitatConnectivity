library(this.path)
library(here)
library(yaml)

# #source(paste(this.dir(), "strings.R", sep = "/"))
# cat(paste("this:", this.dir(), "strings.R", sep = "/"))
# cat(paste("here- ", here::here(), "strings.R", sep = "/"))
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

#' Adjust the extent of two raster objects and return them as a list
#'
#' This function takes two raster objects, `mean_index_raster_diff` and `zeroExtentRaster`,
#' and adjusts the extent of `zeroExtentRaster` to match `mean_index_raster_diff` if their
#' extents are different. The adjusted raster objects are returned as a list with named elements.
#'
#' @param mean_index_raster_diff A raster object representing the mean index difference
#' @param zeroExtentRaster A raster object with an extent to be adjusted
#' @return A list with adjusted raster objects: `raster1` and `raster2`
#' @export
adjust_rasterpair_extent <- function(mean_index_raster_diff, zeroExtentRaster) {
  ext_mean <- extent(mean_index_raster_diff)
  ext_zero <- extent(zeroExtentRaster)
  
  # If the extents are different, adjust the extent of `zeroExtentRaster` to match `mean_index_raster_diff`
  if (!isTRUE(ext_mean == ext_zero)) {
    zeroExtentRaster <- crop(zeroExtentRaster, ext_mean)
  }
  
  return(list(raster1 = mean_index_raster_diff, raster2 = zeroExtentRaster))
}

.check_yaml_structure <- function(existing_yaml_file, provided_yaml_file) {
  # Read the existing YAML file
  existing_yaml <- yaml::yaml.load_file(existing_yaml_file)
  
  # Read the provided YAML file
  provided_yaml <- yaml::yaml.load_file(provided_yaml_file)
  
  # Compare the structure of the YAML files
  if (!identical(names(existing_yaml), names(provided_yaml))) {
    stop("The provided YAML file does not match the structure of the existing YAML file.")
  }
  
  # Recursively check the structure of nested sections
  check_nested_structure <- function(existing_section, provided_section, section_name) {
    if (!identical(names(existing_section), names(provided_section))) {
      stop(paste("The", section_name, "section in the provided YAML file does not match the structure of the existing YAML file."))
    }
    
    # Check the structure of nested sections
    for (key in names(existing_section)) {
      if (is.list(existing_section[[key]]) && is.list(provided_section[[key]])) {
        check_nested_structure(existing_section[[key]], provided_section[[key]], paste(section_name, key, sep = " > "))
      }
    }
  }
  
  # Check the structure of nested sections
  for (key in names(existing_yaml)) {
    if (is.list(existing_yaml[[key]]) && is.list(provided_yaml[[key]])) {
      check_nested_structure(existing_yaml[[key]], provided_yaml[[key]], key)
    }
  }
  
  # If the function reaches this point, the YAML structures match
  return(TRUE)
}

