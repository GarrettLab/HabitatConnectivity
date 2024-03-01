
#' Get Parameters
#'
#' Retrieves the parameters and copies the parameter file to the specified
#' output path.
#' @param out_path character. The output path where the parameter file will be
#' copied. Default is temporary directory [tempdir()]
#' @param iwindow logical. If `TRUE`, prompts the user to select the output
#' directory using a file chooser window. Default is `FALSE`
#' @return character. The path to the copied parameter file.
#' @export
#' @details
#' Using configuration file is an alternative to [sean()]
#'
#' @seealso [set_parameters()]
#'
#' @examples
#' get_parameters()
#' get_parameters(out = tempdir())
#'
get_parameters <- function(out_path = tempdir(), iwindow = FALSE) {
  if (interactive() && iwindow) {
    out_path <- .get_directoryfromuser()
  }

  params_file_path <- .param_fp()
  .copy_file(params_file_path, out_path)
  .showmsg("parameters fetched successfully")
  return(file.path(paste(out_path, basename(params_file_path), sep = "/")))
}

#' Set Parameters
#'
#' This function allows you to set the parameters by replacing the existing
#' parameters file with a new one. Use [get_parameters()] to modify the parameter values.
#'
#' @param new_params The path to the new parameters file.
#' @param iwindow Logical indicating whether to prompt the user to select the
#' new parameters file using a file selection window. Defaults to FALSE.
#' @return None
#' @export
#'
#' @examples
#' param_fp <- get_parameters()
#' set_parameters(param_fp)
#'
set_parameters <- function(new_params, iwindow = FALSE) {
  if (iwindow && interactive()) {
    new_params <- .open_file_selection_prompt()
  }

  init_param <- .default_param()
  if (.check_yaml_structure(
    existing_yaml_file = init_param,
    provided_yaml_file = new_params
  )) {
    .copy_file(new_params, .param_fp())
  }
}

#' Load Parameters from YAML File
#'
#' This function loads parameters from a YAML file and stores them in an object.
#'
#' @param filepath Path to the YAML file containing the parameters. By default, it
#'   takes the value of `parameters.yaml` in R user's directory.
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
load_parameters <- function(filepath = .param_fp()) {
  return(config::get(file = filepath))
}

#' Get resolution value
#'
#' Resolution stored in `parameter.yaml`.
#' If not present it will result default value.
#' @return Numeric. Resolution from `parameters.yaml`. Default is 24.
#' @export
reso <- function() {
  reso <- load_parameters()$Resolution
  reso <- if (is.null(reso) || is.na(reso)) {
    12
  } else {
    reso
  }
  return(reso)
}

#' Reset parameters.yaml
#'
#' Resets the values in the `parameters.yaml`
#' file to the default initial values.
#' @return Logical. `TRUE` if function was successfully executed
#' @export
#' @examples
#' reset_params()
#'
reset_params <- function() {
  .copy_file(.default_param(), .param_fp())
  return(TRUE)
}

#' Dispersal kernels
#'
#' -`[inv_powerlaw()]` Get parameters and values pertaining to the inverse power law model.
#' -`[neg_exp()]` Get parameters and values pertaining to the negative exponential model.
#'
#' @param params Object.[load_parameters()] by default.
#' @return List with parameters and values. See details.
#' @details
#' This list object has following values used in running analysis
#' -`beta` Parameter for calculating the inverse power law.
#' -`gamma` Parameter for calculating the negative exponential.
#' -`metrics` Each of these metrics is applied to the adjacency matrix produced in the intermediate step.
#' -`cutoff` Currently used as a parameter to calculate centrality in the network - [betweeness()] and [closeness()].
#' As defined in [igraph::betweenness()], it's the maximum length to consider when calculating centrality.
#' If zero or negative, then there is no such limit.
#'
#' @seealso [supported_metrics()]
#' @references Csardi G, Nepusz T (2006).
#' “The igraph software package for complex network research.” _InterJournal_, *Complex Systems*, 1695.
#' <https://igraph.org>.
#' @references Csárdi G, Nepusz T, Traag V, Horvát Sz, Zanini F, Noom D, Müller K (2024).
#' _igraph: Network Analysis and Visualization in R_.
#' \doi{10.5281/zenodo.7682609},
#' R package version 1.5.1, <https://CRAN.R-project.org/package=igraph>.
#' @export
#' @rdname Dispersal-kernels
inv_powerlaw <- function(params = load_parameters()) {
  return(list(beta = params$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta,
              metrics = params$`CCRI parameters`$NetworkMetrics$InversePowerLaw$metrics,
              weights = params$`CCRI parameters`$NetworkMetrics$InversePowerLaw$weights,
              cutoff = as.numeric(params$`CCRI parameters`$NetworkMetrics$InversePowerLaw$cutoff)))
}

#' @rdname Dispersal-kernels
neg_exp <- function(params = load_parameters()) {
  return(list(gamma = params$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma,
              metrics = params$`CCRI parameters`$NetworkMetrics$NegativeExponential$metrics,
              weights = params$`CCRI parameters`$NetworkMetrics$NegativeExponential$weights,
              cutoff = as.numeric(params$`CCRI parameters`$NetworkMetrics$NegativeExponential$cutoff)))
}

.param_fp <- function() {

  cfp <-  tools::R_user_dir("geohabnet", which = "config")
  if (!dir.exists(cfp)) {
    dir.create(cfp, recursive = TRUE)
  }

  cfp <- file.path(cfp, "parameters.yaml")
  if (!file.exists(cfp)) {
    .copy_file(.get_helper_filepath(.kparameters_file_type), cfp)
  }

  return(cfp)
}

.default_param <- function() {
  return(system.file("defaultParams.yaml", package = "geohabnet", mustWork = TRUE))
}

.get_directoryfromuser <- function() {
  return(easycsv::choose_dir())
}

.open_file_selection_prompt <- function() {
  return(file.choose())
}

.copy_file <- function(from, to) {
  file.copy(from = from, to = to, overwrite = TRUE)
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
