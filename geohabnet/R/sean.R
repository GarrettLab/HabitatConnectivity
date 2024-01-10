#' @exportPattern ^[^\\.].*

# Utility functions -------------------------------------------------------

.loadparam_ifnull <- function() {
  if (is.null(the$parameters_config)) {
    the$parameters_config <- load_parameters()
  }
}

#----------- Extract cropland density data -----------------------
.extract_cropland_density <- function(cropharvest_agg_crop, host_density_threshold) {
  crop_values <- terra::values(cropharvest_agg_crop)
  max_val <- max(crop_values, na.rm = TRUE)
  if (max_val <= host_density_threshold) {
    stop(paste("host density threshold: ", host_density_threshold,
               " is greater than the max value: ", max_val, " of aggregate raster"))
  }
  crop_cells_above_threshold <- which(crop_values > host_density_threshold)
  thresholded_crop_values <- crop_values[crop_cells_above_threshold]
  return(list(agg_crop = cropharvest_agg_crop,
              crop_value = thresholded_crop_values,
              crop_values_at = crop_cells_above_threshold))
}

# Initialize --------------------------------------------------
#' Only meant to global variables
#' @keywords internal
the <- new.env(parent = emptyenv())
the$is_initialized <- FALSE
the$parameters_config <- NULL
the$distance_matrix <- NULL
the$cropharvest_aggtm <- NULL
the$cropharvest_agglm_crop <- NULL
the$cropharvest_aggtm_crop <- NULL
the$gan <- list(sum = list("east" = NULL, "west" = NULL),
                mean = list("east" = NULL, "west" = NULL))

.resetgan <- function() {
  the$cropharvest_aggtm <- NULL
  the$cropharvest_agglm_crop <- NULL
  the$cropharvest_aggtm_crop <- NULL
  the$gan <- list(sum = list("east" = NULL, "west" = NULL),
                  mean = list("east" = NULL, "west" = NULL))
  invisible()
}

.gan_table <- function(row, col, val) {
  the$gan[[row]][[col]] <- val
  invisible()
}

.crop_rast <- function(agg_method, cropharvest_agg, resolution, geo_scale) {
  postagg_rast <- if (agg_method == "sum") {

    the$cropharvest_aggtm <- cropharvest_agg / resolution / resolution # TOTAL MEAN
    # crop cropland area for the given extent
    the$cropharvest_aggtm_crop <- terra::crop(the$cropharvest_aggtm, .to_ext(geo_scale))
    the$cropharvest_aggtm_crop
  } else if (agg_method == "mean") {

    the$cropharvest_agglm <- cropharvest_agg
    # crop cropland area for the given extent
    the$cropharvest_agglm_crop <- terra::crop(the$cropharvest_agglm, .to_ext(geo_scale))
    the$cropharvest_agglm_crop
  } else {
    stop("aggregation strategy is not supported")
  }
  return(postagg_rast)
}

.init_cd <- function(cropharvest_raster,
                     resolution = reso(),
                     geo_scale,
                     host_density_threshold = 0,
                     agg_method = "sum",
                     dist_method = "geodesic") {

  # aggregation will be cached
  cropharvest_agg <- .apply_agg(cropharvest_raster,
                                resolution,
                                agg_method)

  temp_rast <- .crop_rast(agg_method,
                          cropharvest_agg,
                          resolution,
                          geo_scale)
  density_data <- .extract_cropland_density(temp_rast,
                                            host_density_threshold)

  if (is.null(density_data) || (!is.list(density_data))) {
    stop("unable to extract density data, longitude/latitude")
  }

  # Prepare arguments elements values for the CCRI functions

  # save the latitude and longitude as new matrix
  latilongimatr <- terra::xyFromCell(density_data$agg_crop, cell = density_data$crop_values_at)

  latilongimatr <- as.matrix(latilongimatr)
  temp_matrix <- .cal_dist(latilongimatr, dist_method)

  the$distance_matrix <- temp_matrix

  the$is_initialized <- TRUE
  return(density_data)
}

# Aggregate -----------------------------------------------------

.ccri_powerlaw <- function(betas,
                           link_threshold = 0,
                           metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                           rast,
                           crop_cells_above_threshold = NULL,
                           thresholded_crop_values = NULL) {

  if (!.validate_index_cal(betas)) {
    return(0)
  }

  .loadparam_ifnull()

  pl_models <- lapply(betas, model_powerlaw,
                      link_threshold = link_threshold,
                      the$distance_matrix,
                      thresholded_crop_values,
                      adj_mat = NULL,
                      rast,
                      crop_cells_above_threshold,
                      metrics = metrics)

  return(pl_models)
}

.ccri_negative_exp <- function(gammas,
                               link_threshold = 0,
                               metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                               rast,
                               crop_cells_above_threshold = NULL,
                               thresholded_crop_values = NULL) {

  if (!.validate_index_cal(gammas)) {
    return(0)
  }

  .loadparam_ifnull()

  ne_models <- lapply(gammas,
    model_neg_exp,
    link_threshold = link_threshold,
    the$distance_matrix,
    thresholded_crop_values,
    adj_mat = NULL,
    rast,
    crop_cells_above_threshold,
    metrics = metrics
  )

  return(ne_models)
}


# Utility functions -------------------------------------------------------

.validate_index_cal <- function(vals) {
  ready <- TRUE
  if (!the$is_initialized) {
    stop("Not initialized. Call initializeCroplandData()")
  }
  stopifnot("dispersal values missing" = length(vals) > 0)
  if (!is.vector(vals)) {
    warning("argument is not a vector")
    ready <- FALSE
  }
  return(ready)
}

# CCRI functions ----------------------------------------------------------

.ccri <- function(
    link_threshold = 0,
    power_law_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
    negative_exponential_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
    rast,
    crop_cells_above_threshold,
    thresholded_crop_values) {

  .loadparam_ifnull()

  # TODO: parallelize them
  betas <- as.numeric(the$parameters_config$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta)

  if (length(betas) > 0) {
    stopifnot("beta values are not valid" = is.numeric(betas) == TRUE, is.vector(betas) == TRUE)
    pl_ret <- .ccri_powerlaw(betas,
                             link_threshold,
                             metrics = power_law_metrics,
                             rast,
                             crop_cells_above_threshold = crop_cells_above_threshold,
                             thresholded_crop_values = thresholded_crop_values)
  }

  gammas <- as.numeric(the$parameters_config$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma)
  if (length(gammas) > 0) {
    stopifnot("gamma values are not valid" = is.numeric(gammas) == TRUE, is.vector(gammas) == TRUE)
    ne_ret <- .ccri_negative_exp(gammas,
                                 link_threshold,
                                 metrics = negative_exponential_metrics,
                                 rast,
                                 crop_cells_above_threshold = crop_cells_above_threshold,
                                 thresholded_crop_values = thresholded_crop_values)
  }

  return(c(pl_ret, ne_ret))
}

# Sensitivity analysis ----------------------------------------------------

#' Sensitivity analysis across maps of habitat connectivity
#'
#' @description
#' This function performs a sensitivity analysis across different values of habitat connectivity for each location in a map.
#' For each combination of selected parameters, an index of habitat connectivity is calculated.
#' Some parameters are only accessible from `parameters.yaml` and uses value from here.
#' [sensitivity_analysis()] is a wrapper around [sean()] function.
#' - `msean()` is a wrapper around [sean()] function. It has additional argument to specify maps which are calculated
#' using [connectivity()] function. The maps are essentially the risk network.
#' @param ... arguments passed to [sean()]
#' @param link_threshold Numeric. A threshold value for link weight. 
#' All link weights that are below this threshold will be replaced with zero for the connectivity analysis.
#' Link weights represent the relative likelihood of pathogen, pest, or invasive species movement between a pair of host locations, 
#' which is calculated using gravity models based on host density (or availability) and dispersal kernels.
#' @param host_density_threshold Numeric. A threshold value for host density. 
#' All locations with a host density below the selected threshold will be excluded from the connectivity analysis, 
#' which focuses the analysis on the most important locations.
#' The values for the host density threshold can range between 0 and 1;
#' if 1 all locations will be excluded from the analysis and 0 will include all locations in the analysis.
#' Selecting a threshold for host density requires at least knowing what is the maximum value in the host density map to avoid excluding all locations in the analysis.

#' @inheritParams sa_onrasters
#' @return GeoRasters.
#' @export
#' @details
#' When `global = TRUE`, `geoscale` is ignored and [global_scales()] is used by default. 
#' The functions [sean()] and [msean()] perform the same sensitivity analysis, but they differ in their return value. 
#' The return value of [msean()] is `GeoNetwork`, which contains the result from applying the [connectivity()] function on the habitat connectivity indexes. 
#' Essentially, the risk maps.
#'
#' @value
#' Three spatRasters are produced with the following values. 
#' For each location in the area of interest, the mean in habitat connectivity across selected parameters is calculated.
#' For each location in the area of interest, the variance in habitat connectivity across selected parameters is calculated.
#' For each location in the area of interest, the difference between the rank of habitat connectivity and the rank of host density is calculated.
#' By default, each of these spatRasters is plotted for visualization.
#'
#' @seealso Uses [connectivity()]
#' @seealso Uses [msean()]
#' @inherit sensitivity_analysis references
#'
#' @examples
#' \donttest{
#' avocado <- cropharvest_rast("avocado", "monfreda")
#'
#' # global
#' ri <- sean(avocado) # returns a list of GeoRasters
#' mri <- msean(rast = avocado) # returns GeoNetwork object
#'
#' # non-global
#' # geoscale is a vector of xmin, xmax, ymin, ymax
#'
#' # returns GeoRasters object
#' ri <- sean(avocado, global = FALSE, geoscale = c(-115, -75, 5, 32))
#' ri
#'
#' # returns GeoNetwork object
#' mri <- msean(rast = avocado, global = FALSE, geoscale = c(-115, -75, 5, 32))
#' mri
#' }
sean <- function(rast,
                 global = TRUE,
                 geoscale = NULL,
                 agg_methods = c("sum", "mean"),
                 dist_method = "geodesic",
                 link_threshold = 0,
                 host_density_threshold = 0,
                 res = reso()) {

  stopifnot("Need atleast one aggregation method: " = length(agg_methods) >= 1)
  stopifnot("rast must be of type SpatRaster" = class(rast) == "SpatRaster")

  if (!global) {
    stopifnot("Non-global analysis requires both geoscale argument and global = FALSE" = !is.null(geoscale))
  }

  .resetgan()
  .loadparam_ifnull()

  mets <- get_param_metrics(the$parameters_config)

  sean_geo <- function(geoext) {
    .showmsg(
      paste(
        "\nRunning sensitivity analysis for the extent: [",
        paste(geoext, collapse = ", "),
        "],\n",
        "Link threshold: ",
        link_threshold,
        "\n",
        "Host density threshold: ",
        host_density_threshold,
        "\n"
      )
    )

    model_ret <- list()

    for (agg_method in agg_methods) {
      density_data <- .init_cd(rast,
                               res,
                               geoext,
                               host_density_threshold = host_density_threshold,
                               agg_method,
                               dist_method)

      model_ret <- c(model_ret,
                     .ccri(link_threshold,
                           power_law_metrics = mets$pl,
                           negative_exponential_metrics = mets$ne,
                           rast = density_data$agg_crop,
                           crop_cells_above_threshold = density_data$crop_values_at,
                           thresholded_crop_values = density_data$crop_value))
    }
    return(model_ret)
  }

  .addto_tab <- function(hemi) {
    .gan_table("sum", hemi, the$cropharvest_aggtm_crop)
    .gan_table("mean", hemi, the$cropharvest_agglm_crop)
    invisible()
  }

  rasters <- .rast_ro(global = global)

  if (global) {

    global_exts <- global_scales()

    graster <- .grast_ro()

    graster$east <- sean_geo(global_exts[[STR_EAST]])
    .addto_tab(STR_EAST)

    graster$west <- sean_geo(global_exts[[STR_WEST]])
    .addto_tab(STR_WEST)

    rasters$add_gr(graster)

  } else {
    rasters$rasters <- sean_geo(geoscale)
    rasters$global <- FALSE
  }
  the$is_initialized <- FALSE

  return(rasters)
}

#' @rdname sean
#' @return GeoNetwork.
msean <- function(...,
                  global = TRUE,
                  geoscale = NULL,
                  res = reso(),
                  outdir = tempdir()) {

  grasters <- sean(global = global, geoscale = geoscale, res = res, ...)

  gmap <- connectivity(grasters,
                       global,
                       geoscale,
                       res,
                       as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                       as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                       as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference),
                       outdir = outdir)

  return(new("GeoNetwork",
             rasters = grasters,
             me_rast = gmap@me_rast,
             me_out = gmap@me_out,
             var_rast = gmap@var_rast,
             var_out = gmap@var_out,
             diff_rast = gmap@diff_rast,
             diff_out = gmap@diff_out))

}

.sean_linkweights <- function(link_threshold = 0,
                              host_density_thresholds,
                              global = TRUE,
                              geoscale,
                              agg_methods,
                              rast,
                              res,
                              dist_method = "geodesic") {

  rasters <- lapply(host_density_thresholds,
                    function(threshold) {
                      sean(link_threshold = link_threshold,
                           host_density_threshold = threshold,
                           global = global,
                           geoscale = geoscale,
                           agg_methods = agg_methods,
                           dist_method = dist_method,
                           rast = rast,
                           res = res)
                    })

  newrast <- .rast_ro()
  lapply(rasters, function(x) newrast$com(x))

  return(newrast)
}

#' Run sensitivity analysis
#'
#'
#' @description
#' Same as [sensitivity_analysis()] but it takes raster object and other parameters as an input.
#' - `sa_onrasters()` is a wrapper around [sean()] function. Takes raster object and other parameters as an input.
#' - `msean_onrast()` same as [sa_onrasters()]. Use this for side effects + results.
#' Produces and plots the maps for the outcomes and results are returned as an object.
#' It produces and plots the maps for the outcomes and results are returned as an object.
#'
#' @param rast Raster object which will be used in analysis.
#' @seealso Use [get_rasters()] to obtain raster object.
#' @param global Logical. `TRUE` if global analysis, `FALSE` otherwise.
#' Default is `TRUE`
#' @param geoscale Numeric vector. Geographical coordinates
#' in the form of c(Xmin, Xmax, Ymin, Ymax)
#' @param link_thresholds Numeric vector. link threshold values
#' @param host_density_thresholds Numeric vector. host density threshold values
#' @param agg_methods vector. Aggregation methods
#' @param dist_method Character. One of the values from [dist_methods()]
#' @param res Numeric.
#' resolution at which operations will run.
#' Default is [reso()]
#' @param ... arguments passed to [sa_onrasters()]
#' @param outdir Character. Output directory for saving raster in TIFF format.
#' Default is [tempdir()].
#' @return A list of calculated CCRI indices after operations.
#' An index is generated for each combination of paramters.
#' One combination is equivalent to [sean()] function.
#' @export
#' @details
#' When `global = TRUE`, `geo_scale` is ignored.
#' Instead uses scales from [global_scales()].
#'
#' @examples
#' \donttest{
#' rr <- get_rasters(list(monfreda = c("avocado")))
#' res1 <- sa_onrasters(rr[[1]],
#'             global = FALSE,
#'             geoscale = c(-115, -75, 5, 32),
#'             c(0.0001, 0.00004),
#'             c(0.0001, 0.00005),
#'             c("sum", "mean"),
#'             res = 24)
#' res2 <- sa_onrasters(rr[[1]],
#'             global = TRUE,
#'             link_thresholds = c(0.000001),
#'             host_density_thresholds = c(0.00015),
#'             agg_methods = c("sum"),
#'             res = 24)
#' res3 <- msean_onrast(rast = rr[[1]],
#'           link_thresholds = c(0.000001),
#'           host_density_thresholds = c(0.00015))
#'}
#' @inherit sensitivity_analysis seealso references
#' @seealso [msean_onrast()]
#'
sa_onrasters <- function(rast,
                         global = TRUE,
                         geoscale,
                         link_thresholds,
                         host_density_thresholds,
                         agg_methods = c("sum", "mean"),
                         dist_method = "geodesic",
                         res = reso()) {

  .showmsg("New analysis started for given raster")

  .loadparam_ifnull()

  rasters <- lapply(link_thresholds,
                    function(lthreshold) {
                      .sean_linkweights(
                        link_threshold = lthreshold,
                        host_density_thresholds = host_density_thresholds,
                        global = global,
                        geoscale = geoscale,
                        agg_methods = agg_methods,
                        dist_method = dist_method,
                        rast = rast,
                        res = res
                      )
                    })


  newrast <- .rast_ro()
  lapply(rasters, function(x) newrast$com(x))

  return(newrast)
}

#' @rdname sa_onrasters
msean_onrast <- function(global = TRUE,
                         geoscale = NULL,
                         res = reso(),
                         outdir = tempdir(),
                         ...) {

  grast <- sa_onrasters(...,
                        global = global,
                        geoscale = geoscale,
                        res = res)

  gmap <- connectivity(grast,
                       global,
                       geoscale,
                       res,
                       as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                       as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                       as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference),
                       outdir)

  return(new("GeoNetwork",
             rasters = grast,
             me_rast = gmap@me_rast,
             me_out = gmap@me_out,
             var_rast = gmap@var_rast,
             var_out = gmap@var_out,
             diff_rast = gmap@diff_rast,
             diff_out = gmap@diff_out))
}

#' @title Sensitivity analysis for habitat connectivity
#'
#' @description
#' This function runs a sensitivity analysis on habitat connectivity calculated based on every combination of selected parameters.
#' Parameter values in [sensitivity_analysis()] should be provided using the function [set_parameters()]. 
#' If no parameters are provided, then the [sensitivity_analysis()] function will run the sensitivity analysis using a default set of parameter values, 
#' which is accessible through the function [get_parameters()].
#' To customize parameter values, open the parameters.yaml that was automatically downloaded when geohabnet was installed, 
#' change, remove, or add parameter values directly in the parameters.yaml and save it. 
#' Once the values have been changed manually, run geohabnet::set_parameters() to set the new parameter values, which will return TRUE if the parameters were set successfully.
#'
#' @details
#' For each location in a region, sensitivity_analysis() calculates the cropland connectivity risk index (CCRI) proposed by Xing et al. (2021).
#' By default, sensitivity_analysis() runs a sensitivity analysis on a global extent, see [global_scales()] for details.
#' This function also plots maps of the outcomes automatically, but it will suppress maps for outcomes if `maps = FALSE` or
#' [interactive()] is `FALSE`. 
#' The returned object is of class `GeoNetwork`, which contains two types of outcomes.
#' One outcome type corresponds to spatRasters representing the maps of habitat connectivity. 
#' The second type corresponds to adjacency matrices used to calculate the habitat connectivity,
#' where columns and rows represent locations in the maps and 
#' entries are the relative likelihood of pathogen or pest movement between each pair of nodes.
#' @param maps logical. `TRUE` if maps of outcomes are to be plotted, `FALSE` otherwise. If `TRUE`, three maps are possible: a map of mean habitat connectivity, a map of variance of habitat connectivity, and a map of the difference between the ranks in habitat connectivity and habitat density.
#' @param alert logical. `TRUE` if a beep sound is to be played once the analysis is completed, `FALSE` otherwise
#' @return GeoNetwork.
#' Errors are not handled.
#' @export
#'
#' @examples
#' \donttest{
#' # Run analysis on specified parameters.yaml
#' ss1 <- sensitivity_analysis()
#' ss2 <- sensitivity_analysis(FALSE, FALSE)
#' ss3 <- sensitivity_analysis(TRUE, FALSE)
#' }
#' @seealso
#' [sa_onrasters()]
#' [sean()]
#' [global_scales()]
#' [get_parameters()]
#' [set_parameters()]
#' [connectivity()]
#'
#' @references Yanru Xing, John F Hernandez Nopsa, Kelsey F Andersen, Jorge L Andrade-Piedra, Fenton D Beed,
#' Guy Blomme, Mónica Carvajal-Yepes, Danny L Coyne, Wilmer J Cuellar, Gregory A Forbes,
#' Jan F Kreuze, Jürgen Kroschel, P Lava Kumar, James P Legg, Monica Parker, Elmar Schulte-Geldermann,
#' Kalpana Sharma, Karen A Garrett,
#' _Global Cropland Connectivity: A Risk Factor for Invasion and Saturation by Emerging Pathogens and Pests_,
#' BioScience, Volume 70, Issue 9, September 2020, Pages 744–758,
#' \doi{10.1093/biosci/biaa067}
#' @references Hijmans R (2023). _terra: Spatial Data Analysis_.
#' R package version 1.7-46, \url{https://CRAN.R-project.org/package=terra}
sensitivity_analysis <- function(maps = TRUE, alert = TRUE) {

  #.resetglobals()
  .resetgan()
  the$is_initialized <- FALSE
  the$parameters_config <- load_parameters()

  # cutoff adjacency matrix
  cropland_thresholds <- the$parameters_config$`CCRI parameters`$HostDensityThreshold

  # crop data
  crop_rasters <- get_rasters(the$parameters_config$`CCRI parameters`$Hosts)
  agg_methods <- the$parameters_config$`CCRI parameters`$AggregationStrategy # list

  # resolution
  resolution <- the$parameters_config$`CCRI parameters`$Resolution

  # global analysis
  isglobal <- the$parameters_config$`CCRI parameters`$GeoExtent$global
  geoscale <- geoscale_param()

  rasters <- lapply(crop_rasters,
                    function(rast) {
                      sa_onrasters(rast = rast,
                                   global = isglobal,
                                   geoscale = geoscale,
                                   link_thresholds = the$parameters_config$`CCRI parameters`$LinkThreshold,
                                   host_density_thresholds = cropland_thresholds,
                                   agg_methods = agg_methods,
                                   dist_method = the$parameters_config$`CCRI parameters`$DistanceStrategy,
                                   res = resolution)
                    })

  newrast <- .rast_ro()
  lapply(rasters, function(x) newrast$com(x))
  #risk_indices <- risk_indices(newrast)

  gmap <- if (maps == TRUE) {
    connectivity(newrast,
                 isglobal,
                 geoscale,
                 resolution,
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference),
                 the$parameters_config$`CCRI parameters`$PriorityMaps$OutDir)
  }

  .showmsg("sensitivity analysis completed. Refer to maps for results.")
  if (alert == TRUE) {
    beepr::beep(2)
  }
  ret <- if (maps == TRUE) {
    new("GeoNetwork",
        rasters = newrast,
        me_rast = gmap@me_rast,
        me_out = gmap@me_out,
        var_rast = gmap@var_rast,
        var_out = gmap@var_out,
        diff_rast = gmap@diff_rast,
        diff_out = gmap@diff_out)
  } else {
    new("GeoNetwork",
        rasters = newrast)
  }
  return(ret)
}
