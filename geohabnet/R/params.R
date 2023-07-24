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
  spams <- params$`CCRI parameters`$Hosts$mapspam
  crops <- list()
  if (!is.null(monfredas) && !is.list(monfredas)) {
    crops[["monfreda"]] <- monfredas
  }
  if (!is.null(spams) && !is.list(spams)) {
    crops[["mapspam"]] <- spams
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
  return(config::get(file = filepath))
}
