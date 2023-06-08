.get_param_file_path <- function() {
  return(system.file("parameters.yaml", package = "geohabnet", mustWork = TRUE))
}

.get_directoryfromuser <- function() {
  return(easycsv::choose_dir())
}

.open_file_selection_prompt <- function() {
  return(file.choose())
}

.copy_file <- function(from, to) {
  file.copy(from = from, to = to, copy.mode = FALSE, overwrite = TRUE)
}

.extract_hosts <- function(params = load_parameters()) {
  monfredas <- params$`CCRI parameters`$Hosts$monfreda
  spams <- params$`CCRI parameters`$Hosts$monfreda
  crops <- list()
  if(!is.null(monfredas) && !is.list(monfredas)) {
    crops[['monfreda']] <- monfredas
  }
  if(!is.null(spams) && !is.list(spams)) {
    crops[['spam']] <- spams
  }
  return(crops)
}

#' Get Parameters
#'
#' Retrieves the parameters and copies the parameter file to the specified
#' output path.
#'
#' @param iwindow Logical. If TRUE, prompts the user to select the output
#' directory using a file chooser window.
#' @param out_path Character. The output path where the parameter file will be
#' copied.
#' @return Character. The path to the copied parameter file.
#' @export
get_parameters <- function(iwindow = FALSE, out_path = getwd()) {
  if (interactive() && iwindow) {
    out_path <- .get_directoryfromuser()
  }

  params_file_path <- .get_param_file_path()
  .copy_file(params_file_path, out_path)
  print("parameters fetched successfully")
  return(file.path(paste(out_path, "parameters.yaml", sep = "/")))
}

#' Set Parameters
#'
#' This function allows you to set the parameters by replacing the existing
#' parameters file with a new one. Use [get_parameters()] to modify the parameter values.
#'
#' @param new_parameters_file The path to the new parameters file.
#' @param iwindow Logical indicating whether to prompt the user to select the
#' new parameters file using a file selection window. Defaults to FALSE.
#' @return None
#' @export
set_parameters <- function(new_parameters_file, iwindow = FALSE) {
  if (iwindow && interactive()) {
    new_parameters_file <- .open_file_selection_prompt()
  }

  current_params_file <- .get_param_file_path()
  cat(current_params_file)
  cat(file.exists(current_params_file))
  if (.check_yaml_structure(
    existing_yaml_file = current_params_file,
    provided_yaml_file = new_parameters_file
  )) {
    .copy_file(new_parameters_file, current_params_file)
  }
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
  cat(filepath)
  cat(file.exists(filepath))
  return(config::get(file = filepath))
}

#' Set Parameters function
#'
#' This function allows you to override existing parameters with new values.
#'
#' @seealso [load_parameters()] [set_parameters()]
#' @param dispersal_parameter_beta Numeric vector of dispersal parameter beta values
#' @param dispersal_parameter_gamma Numeric vector of dispersal parameter gamma values
#' @param aggregation_strategy Character vector of aggregation strategies
#' @param hosts Character vector of hosts
#' @param host_density_threshold Numeric vector of host density threshold values
#' @param link_threshold Numeric vector of link threshold values
#' @param resolution Numeric vector of resolution values
#' @param global_analysis Logical vector of global analysis values
#' @param west_extent Numeric vector of west extent values
#' @param east_extent Numeric vector of east extent values
#' @param custom_extent List of custom extent values
#' @param metrics_inv_powerlaw Character vector of inv_powerlaw metrics
#' @param metrics_neg_exponential Character vector of neg_exponential metrics
#' @return TRUE if the parameters were set successfully, FALSE otherwise
#' @export
#' @examples
#' \dontrun{
#' # Set parameters
#' set_parameters_object()
#' # Set parameters with custom beta values
#' set_parameters_object(dispersal_parameter_beta = c(0.5, 1, 1.5))
#' }
set_parameters_object <- function(dispersal_parameter_beta = c(0.5, 1, 1.5),
                                  dispersal_parameter_gamma = c(0.05, 1, 0.2, 0.3),
                                  aggregation_strategy = c("sum", "mean"),
                                  hosts = c("avocado"),
                                  host_density_threshold = c(0.0015, 0.002, 0.0025),
                                  link_threshold = c(0, 0.000001, 0.0006),
                                  resolution = 24,
                                  global_analysis = FALSE,
                                  west_extent = c(-24, -180, -58, 60),
                                  east_extent = c(-140, -34, --58, 60),
                                  custom_extent = list(c(-115, -75, -5, 32)),
                                  metrics_inv_powerlaw =
                                    c("betweeness", "node_strength",
                                    "sum_of_nearest_neighbors", "eigenvector_centrality"),
                                  metrics_neg_exponential = c("betweeness", "node_strength",
                                    "sum_of_nearest_neighbors", "eigenvector_centrality")) {

  file_path <- .get_param_file_path()
  param_obj <- yaml::read_yaml(file = file_path)$default

  param_obj$`CCRI parameters`$`dispersal parameter beta` <- dispersal_parameter_beta
  param_obj$`CCRI parameters`$`dispersal parameter gamma` <- dispersal_parameter_gamma
  param_obj$`CCRI parameters`$`aggregation strategy` <- aggregation_strategy
  param_obj$`CCRI parameters`$`hosts` <- hosts
  param_obj$`CCRI parameters`$`host density threshold` <- host_density_threshold
  param_obj$`CCRI parameters`$`link threshold` <- link_threshold
  param_obj$`CCRI parameters`$`resolution` <- resolution
  param_obj$`CCRI parameters`$`Longitude_Latitude`$`global analysis` <- global_analysis
  param_obj$`CCRI parameters`$`Longitude_Latitude`$`west extent` <- west_extent
  param_obj$`CCRI parameters`$`Longitude_Latitude`$`east extent` <- east_extent
  param_obj$`CCRI parameters`$`Longitude_Latitude`$`custom extent` <- custom_extent
  param_obj$`CCRI parameters`$`NetworkMetrics`$`inv_powerlaw` <- metrics_inv_powerlaw
  param_obj$`CCRI parameters`$`NetworkMetrics`$`neg_exponential` <- metrics_neg_exponential

  yaml::write_yaml(x = param_obj, file = file_path)

  return(TRUE)
}
