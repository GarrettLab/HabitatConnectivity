
#' Get Parameters
#'
#' This function retrieves the parameters and copies the parameter file to the specified
#' output path.
#' @param out_path character. The output path where the parameter file will be
#' copied. The default is a temporary directory [tempdir()]
#' @param iwindow logical. If window = `TRUE`, this will prompt the user to select the output
#' directory using a file chooser window. The default value is `FALSE`.
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
#' This function allows the user to set the parameters by replacing the existing
#' parameters file with a new one. Use [get_parameters()] to modify the parameter values.
#'
#' @param new_params The path to the new parameters file.
#' @param iwindow Logical, indicating whether to prompt the user to select the
#' new parameters file using a file selection window. The default value of this parameter is set to FALSE.
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
#' Resolution stored in `parameter.yaml`. Here, resolution values refer
#' to the aggregation factor or granularity. Granularity is the number of small
#' grid cells that are aggregated into larger grid cells in each direction
#' (horizontally and vertically).
#' For example, the finest spatial resolution of the Monfreda and MAPSPAM dataset
#' in geohabnet is 5 minutes, a granularity value of 6 will result in maps with
#' a spatial resolution of 0.5 degrees.
#' If not provided, the resolution value used for the analysis is by default 12
#' (or two degrees when using the Monfreda and MAPSPAM dataset).
#' Otherwise, a single integer value for granularity equal to or greater
#' than one should be specified.
#' @return Numeric. Resolution from `parameters.yaml`. The default is 12.
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
#' @param params Object. This function uses the parameter values defined in [load_parameters()] by default.
#' If [load_parameters()] is not provided, the function will require the user to specify all arguments as listed below.
#' If both [load_parameters()] and values for the arguments listed below are provided, [load_parameters()] takes precedence over the specified arguments.
#' @param betas Numeric. Beta is the dispersal parameter used in the inverse power law to estimate a species' dispersal gradient. Please refer to Mundt et al (2009) for details on how to calculate this parameter. Any beta values should be positive. Smaller beta values indicate a higher likelihood of dispersal between nodes.
#' @param gammas Numeric. Gamma is the dispersal parameter used in the negative exponential to estimate a species' dispersal gradient. Any gamma values should be positive. Smaller gamma values indicate a higher likelihood of dispersal between nodes.
#' @param mets Character. There are seven network metrics supported by `geohabnet`:  "node_strength", "sum_of_nearest_neighbors", "eigenvector_centrality", "closeness", "betweeness", "degree", and "page_rank".
#' Each specified network metric is calculated for each location in the target region, based on the link weights between each pair of locations. Run, for example, [pagerank()] for details of each network metric.
#' @param we Numeric. This parameter indicates the weight(s) of each specified network metric, representing the importance of the network metric in the analysis. Since these weights represent percentages, any weight(s) should be between 0 and 100, and the sum of all specified weights should be 100.
#' @param linkcutoff Numeric. This parameter is only used to calculate [betweeness()] and [closeness()], and is equivalent to `cutoff` in these functions in the `igraph` package.
#' @return List with parameters and values. See details.
#' @details
#' Refer to Esker et al (2007) for a discussion on the characteristics of each dispersal gradient or kernel model (i.e., inverse power law and negative exponential). The resulting object produced by [load_parameters()] provides the following values used when running the analysis
#' -`beta` is a dispersal parameter for calculating the inverse power law model.
#' -`gamma` is a dispersal parameter for calculating the negative exponential model.
#' -`metrics` Each network metric is applied to the adjacency matrix produced in the intermediate step.
#' -`weights` The link weights that is used in the network analysis.
#' -`cutoff` Currently used as a parameter to calculate centrality in the network - [betweeness()] and [closeness()].
#' As defined in [igraph::betweenness()], it's the maximum length to consider when calculating centrality.
#' If zero or negative, then there is no such limit.
#'
#' @seealso [supported_metrics()]
#' @references Esker PD, Sparks AH, Antony G, Bates M, Dall' Acqua W, Frank EE, Huebel L, Segovia V, Garrett KA (2007).
#' “Ecology and Epidemiology in R: Modeling dispersal gradients.” *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-03}
#' @references Mundt CC, Sackett KE, Wallace LD, Cowger C, Dudley JP  (2009).
#' “Aerial Dispersal and Multiple-Scale Spread of Epidemic Disease.” *Ecohealth*.
#' \doi{https://doi.org/10.1007/s10393-009-0251-z}
#' @references Csardi G, Nepusz T (2006).
#' “The igraph software package for complex network research.” _InterJournal_, *Complex Systems*, 1695.
#' <https://igraph.org>.
#' @references Csárdi G, Nepusz T, Traag V, Horvát Sz, Zanini F, Noom D, Müller K (2024).
#' _igraph: Network Analysis and Visualization in R_.
#' \doi{10.5281/zenodo.7682609},
#' R package version 1.5.1, <https://CRAN.R-project.org/package=igraph>.
#' @export
#' @rdname Dispersal-kernels
inv_powerlaw <- function(params = load_parameters(), betas = NULL, mets = NULL, we = NULL, linkcutoff = NULL) {

  if (!is.null(params)) {
    return(list(beta = as.numeric(params$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta),
                metrics = as.character(params$`CCRI parameters`$NetworkMetrics$InversePowerLaw$metrics),
                weights = as.numeric(params$`CCRI parameters`$NetworkMetrics$InversePowerLaw$weights),
                cutoff = as.numeric(params$`CCRI parameters`$NetworkMetrics$InversePowerLaw$cutoff)))
  } else {
    return(list(beta = as.vector(betas),
                metrics = as.character(mets),
                weights = as.numeric(we),
                cutoff = as.numeric(linkcutoff)))
  }
}

#' @rdname Dispersal-kernels
neg_expo <- function(params = load_parameters(), gammas = NULL, mets = NULL, we = NULL, linkcutoff = NULL) {

  if (!is.null(params)) {
    return(list(gamma = as.numeric(params$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma),
                metrics = as.character(params$`CCRI parameters`$NetworkMetrics$NegativeExponential$metrics),
                weights = as.numeric(params$`CCRI parameters`$NetworkMetrics$NegativeExponential$weights),
                cutoff = as.numeric(params$`CCRI parameters`$NetworkMetrics$NegativeExponential$cutoff)))
  } else {
    return(list(gamma = as.numeric(gammas),
                metrics = as.character(mets),
                weights = as.numeric(we),
                cutoff = as.numeric(linkcutoff)))
  }
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

