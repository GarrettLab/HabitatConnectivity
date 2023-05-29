library(yaml)

# Global constants --------------------------------------------------------

.kparameters_file_type <- "parameters"
.kzeroraster_file_type <- "zero_raster"
.kmapgreybackground_file_type <- "map_grey_background"

# utility functions for CCRI ----------------------------------------------

.valid_vector_input <- function(vector_to_check) {
  if (!is.vector(vector_to_check) || length(vector_to_check) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

#' Check if metrics in the list are valid
#'
#' @param metrics_list A character vector of metrics to check
#' @return A named logical vector indicating if each metric is valid or not
#' @examples
#' # return list of valid metrics
#' check_metrics(list("betweeness", "invalid_metric"))
#' @export
check_metrics <- function(metrics_list) {
  valid_metrics <- c(
    STR_BETWEENNESS, STR_NODE_STRENGTH,
    STR_NEAREST_NEIGHBORS_SUM, STR_EIGEN_VECTOR_CENTRALITY
  )
  is_valid <- lapply(valid_metrics, function(x) x %in% metrics_list)
  names(is_valid) <- valid_metrics
  return(is_valid)
}


#' calculate weights for each metric
#' @param betweenness_metric A logical value indicating if the betweenness metric
#'  should be used
#' @param node_strength A logical value indicating if the node strength metric
#' should be used
#' @param sum_of_nearest_neighbors A logical value indicating if the sum of
#' nearest neighbors metric should be used
#' @param eigenvector_centrality A logical value indicating if the eigenvector
#' centrality metric should be used
#' @return A named vector of weights for each metric
#' @examples
#' # return weights for each metric
#' calculate_metrics_weight(betweenness_metric = TRUE, node_strength = TRUE,
#'                         sum_of_nearest_neighbors = TRUE, eigenvector_centrality = TRUE)
#' @export
calculate_metrics_weight <- function(betweenness_metric = FALSE,
                                     node_strength = FALSE,
                                     sum_of_nearest_neighbors = FALSE,
                                     eigenvector_centrality = FALSE) {
  # initialize weights and counters
  weight <- 1
  num_of_metrics <- sum(c(
    node_strength, sum_of_nearest_neighbors,
    eigenvector_centrality
  ))
  # calculate weights for each metric
  if (betweenness_metric) {
    w1 <- weight / 2
    weight <- weight / 2
  } else {
    w1 <- 0
  }

  w2 <- if (node_strength && num_of_metrics > 0) {
    weight / num_of_metrics
  } else {
    0
  }

  w3 <- if (sum_of_nearest_neighbors && num_of_metrics > 0) {
    weight / num_of_metrics
  } else {
    0
  }

  w4 <- if (eigenvector_centrality && num_of_metrics > 0) {
    weight / num_of_metrics
  } else {
    0
  }


  # handle division by zero
  if (w1 != 0) w1 <- as.integer(1 / w1)
  if (w2 != 0) w2 <- as.integer(1 / w2)
  if (w3 != 0) w3 <- as.integer(1 / w3)
  if (w4 != 0) w4 <- as.integer(1 / w4)

  weights <- c(w1, w2, w3, w4)
  names(weights) <- c(
    STR_BETWEENNESS, STR_NODE_STRENGTH,
    STR_NEAREST_NEIGHBORS_SUM, STR_EIGEN_VECTOR_CENTRALITY
  )

  # return the weights as a vector
  return(weights)
}

.get_helper_filepath <- function(file_type) {
  if (file_type == "parameters") {
    file_path <- system.file("parameters.yaml",
      package = "geohabnet",
      mustWork = TRUE
    )
  } else if (file_type == "zero_raster") {
    file_path <- system.file("tifs", "ZeroRaster.tif",
      package = "geohabnet",
      mustWork = TRUE
    )
  } else if (file_type == "map_grey_background") {
    file_path <- system.file("tifs", "map_grey_background.tif",
      package = "geohabnet", mustWork = TRUE
    )
  } else {
    stop("Invalid file_type parameter. Supported options are 'config',
         'zero_raster', and 'map_grey_background'.")
  }

  return(file_path)
}

# Recursively check the structure of nested sections
.check_nested_structure <- function(existing_section, provided_section,
                                    section_name) {
  if (!identical(names(existing_section), names(provided_section))) {
    stop(paste("The", section_name, "section in the provided YAML file does not
               match the structure of the existing YAML file."))
  }

  # Check the structure of nested sections
  for (key in names(existing_section)) {
    if (is.list(existing_section[[key]]) && is.list(provided_section[[key]])) {
      .check_nested_structure(
        existing_section[[key]], provided_section[[key]],
        paste(section_name, key, sep = " > ")
      )
    }
  }
}

# Check the structure of the provided YAML file, and make sure it matches the structure of the existing YAML file
#' @param existing_yaml_file Path to the existing YAML file
#' @param provided_yaml_file Path to the provided YAML file
#' @return TRUE if the structures match, FALSE otherwise
.check_yaml_structure <- function(existing_yaml_file, provided_yaml_file) {
  # Read the existing YAML file
  existing_yaml <- yaml::yaml.load_file(existing_yaml_file)

  # Read the provided YAML file
  provided_yaml <- yaml::yaml.load_file(provided_yaml_file)

  # Compare the structure of the YAML files
  if (!identical(names(existing_yaml), names(provided_yaml))) {
    stop("The provided YAML file does not match the structure of the existing
         YAML file.")
  }
  # Check the structure of nested sections
  for (key in names(existing_yaml)) {
    if (is.list(existing_yaml[[key]]) && is.list(provided_yaml[[key]])) {
      .check_nested_structure(existing_yaml[[key]], provided_yaml[[key]], key)
    }
  }
  # If the function reaches this point, the YAML structures match
  return(TRUE)
}

#' Load Parameters from YAML File
#'
#' This function loads parameters from a YAML file and stores them in an object.
#'
#' @param filepath Path to the YAML file containing the parameters. By default, it
#'   takes the value of ".kparameters_file_type" which is set to "parameters.yaml".
#'
#' @return object with parameters and values
#'
#' @importFrom config get
#'
#' @examples
#' # Load parameters from default file
#' load_parameters()
#'
#' @export
load_parameters <- function(filepath = .get_helper_filepath(.kparameters_file_type)) {
  return(config::get(file = filepath))
}

.get_cs_host_names <- function(param_config = load_parameters()) {
  return(paste(param_config$`CCRI parameters`$Crops, collapse = ", "))
}

.get_map_grey_background_extent <- function(geoscale) {
      map_grey_background <- raster::raster(.get_helper_filepath(.kmapgreybackground_file_type))
      map_grey_background_ext <- raster::crop(map_grey_background, geoscale)
      return(map_grey_background_ext)
  }
