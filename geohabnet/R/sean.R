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
  #stopifnot("Not a spatRaster" = tolower(class(val)) == "spatraster")
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
                                     resolution = 12,
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
  density_data <- .extract_cropland_density(
    temp_rast,
    host_density_threshold)

  if (is.null(density_data) || (!is.list(density_data))) {
    stop("unable to extract density data, longitude/latitude")
  }

  # Prepare arguments elements values for the CCRI functions

  # save the latitude and longitude as new matrix
  latilongimatr <- terra::xyFromCell(density_data$agg_crop, cell = density_data$crop_values_at)
  #---- use Geosphere package, fun distVincentyEllipsoid() is used to calculate the distance, default distance is meter
  # reference of standard distance in meter for one degree
  #dvse <- geosphere::distVincentyEllipsoid(c(0, 0), cbind(1, 0))
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

  index_list <- lapply(betas, model_powerlaw,
                       link_threshold = link_threshold,
                       the$distance_matrix,
                       thresholded_crop_values,
                       adj_mat = NULL,
                       rast,
                       crop_cells_above_threshold,
                       metrics = metrics
                       )
  return(index_list)

  #the$result_index_list <- c(the$result_index_list, index_list)
  #invisible()
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

  index_list <- lapply(gammas,
                       model_neg_exp,
                       link_threshold = link_threshold,
                       the$distance_matrix,
                       thresholded_crop_values,
                       adj_mat = NULL,
                       rast,
                       crop_cells_above_threshold,
                       metrics = metrics
  )

  return(index_list)
  #the$result_index_list <- c(the$result_index_list, index_list)
  #invisible()
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
  risk_indexes <- list()

  # TODO: parallelize them
  betas <- as.numeric(the$parameters_config$`CCRI parameters`$DispersalKernelModels$InversePowerLaw$beta)

  if (length(betas) > 0) {
    stopifnot("beta values are not valid" = is.numeric(betas) == TRUE, is.vector(betas) == TRUE)
    risk_indexes <- c(risk_indexes,
                      .ccri_powerlaw(betas,
                                    link_threshold,
                                    metrics = power_law_metrics,
                                    rast,
                                    crop_cells_above_threshold = crop_cells_above_threshold,
                                    thresholded_crop_values = thresholded_crop_values
                      ))
  }

  gammas <- as.numeric(the$parameters_config$`CCRI parameters`$DispersalKernelModels$NegativeExponential$gamma)
  if (length(gammas) > 0) {
    stopifnot("gamma values are not valid" = is.numeric(gammas) == TRUE, is.vector(gammas) == TRUE)
    risk_indexes <- c(risk_indexes,
                      .ccri_negative_exp(gammas,
                                        link_threshold,
                                        metrics = negative_exponential_metrics,
                                        rast,
                                        crop_cells_above_threshold = crop_cells_above_threshold,
                                        thresholded_crop_values = thresholded_crop_values
                      ))
    }

  return(risk_indexes)
}

# Sensitivity analysis ----------------------------------------------------

#' Calculate sensitivity analysis on cropland harvested area fraction
#'
#'   This function calculates sensitivity analysis on cropland harvested area fraction based on provided parameters.
#'   It can be used as an entry point for sensitivity analysis.
#' @param link_threshold numeric. A threshold value for link
#' @param host_density_threshold A host density threshold value
#' @inheritParams sa_onrasters
#' @return A list of calculated CCRI values using negative exponential
#' @export
#' @details
#' When `global = TRUE`, `geoscale` is ignored and [global_scales()] is used
#'
#' @seealso Uses [connectivity()]
sean <- function(link_threshold = 0,
                 global = TRUE,
                 geoscale,
                 agg_methods = c("sum", "mean"),
                 dist_method = "geodesic",
                 rast,
                 host_density_threshold = 0,
                 reso = reso(),
                 maps = TRUE) {

  .loadparam_ifnull()

  mets <- get_param_metrics(the$parameters_config)

  sean_geo <- function(geoext) {
    message(
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

    lrisk_indexes <- list()

    for (agg_method in agg_methods) {
      density_data <- .init_cd(rast,
                               reso,
                               geoext,
                               host_density_threshold = host_density_threshold,
                               agg_method,
                               dist_method)

      lrisk_indexes <- c(lrisk_indexes,
                        .ccri(link_threshold,
                              power_law_metrics = mets$pl,
                              negative_exponential_metrics = mets$ne,
                              rast = density_data$agg_crop,
                              crop_cells_above_threshold =
                                density_data$crop_values_at,
                              thresholded_crop_values =
                                density_data$crop_value
                              ))
    }
    return(lrisk_indexes)
  }

  .addto_tab <- function(hemi) {
    .gan_table("sum", hemi, the$cropharvest_aggtm_crop)
    .gan_table("mean", hemi, the$cropharvest_agglm_crop)
    invisible()
  }

  risk_indexes <- if (global) {

    global_exts <- global_scales()

    east_indexes <- sean_geo(global_exts[[STR_EAST]])
    .addto_tab(STR_EAST)

    west_indexes <- sean_geo(global_exts[[STR_WEST]])
    .addto_tab(STR_WEST)
    list(east = east_indexes, west = west_indexes)
  } else {
    sean_geo(geoscale)
  }

  if (maps == TRUE) {
    connectivity(risk_indexes,
                 global,
                 geoscale,
                 reso,
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }
  the$is_initialized <- FALSE
  return(risk_indexes)
}

.sean_linkweights <- function(link_threshold = 0,
                              host_density_thresholds,
                              global = TRUE,
                              geoscale,
                              agg_methods,
                              rast,
                              reso,
                              dist_method = "geodesic",
                              maps = TRUE) {

  risk_indexes <- lapply(host_density_thresholds,
                                function(threshold) {
                                  invisible(
                                    sean(
                                      link_threshold = link_threshold,
                                      host_density_threshold = threshold,
                                      global = global,
                                      geoscale = geoscale,
                                      agg_methods = agg_methods,
                                      dist_method = dist_method,
                                      rast = rast,
                                      reso = reso,
                                      maps = FALSE
                                    )
                                  )
                                })

  risk_indexes <- .flatten_ri(global, risk_indexes)
  if (maps == TRUE) {

    connectivity(risk_indexes,
                 global = global,
                 geoscale,
                 reso,
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }
  return(risk_indexes)
}

#' Run senstivity analysis
#'
#' Same as [sensitivity_analysis()] but it takes raster object and other parameters as an input.
#' @param rast Raster object which will be used in analysis.
#' @seealso Use [get_rasters()] to obtain raster object.
#' @param global Logical. `TRUE` if global analysis, `FALSE` otherwise.
#' Default is `TRUE`
#' @param geoscale Vector. Geographical coordinates
#' in the form of c(Xmin, Xmax, Ymin, Ymax)
#' @param link_thresholds vector. link threshold values
#' @param host_density_thresholds vector. host density threshold values
#' @param agg_methods vector. Aggregation methods
#' @param dist_method character. One of the values from [dist_methods()]
#' @param reso numeric.
#' resolution at which operations will run.
#' Default is [reso()]
#' @param maps logical. `TRUE` if maps are to be plotted, `FALSE` otherwise
#' @return A list of calculated CCRI indices after operations.
#' An index is generated for each combination of paramters.
#' One combination is equivalent to [sean()] function.
#' @export
#' @details
#' When `global = TRUE`, `geo_scale` is ignored.
#' Instead uses scales from [global_scales()].
#'
#' @examples
#' \dontrun{
#' rr <- get_rasters(list(monfreda = c("coffee")))
#' sa_onrasters(rr[[1]],
#'             global = FALSE,
#'             geoscale = c(-115, -75, 5, 32),
#'             c(0.0001, 0.00004),
#'             c(0.0001, 0.00005),
#'             c("sum", "mean"),
#'             reso = 24)
#' sa_onrasters(rr[[1]],
#'             global = TRUE,
#'             geoscale = c(-115, -75, 5, 32),
#'             0.0001,
#'             0.00001,
#'             c("sum"),
#'             reso = 24)
#'}
#' @inherit sensitivity_analysis seealso
sa_onrasters <- function(rast,
                         global = TRUE,
                         geoscale,
                         link_thresholds,
                         host_density_thresholds,
                         agg_methods = c("sum", "mean"),
                         dist_method = "geodesic",
                         reso = reso(),
                         maps = TRUE) {

  cat("New analysis started for given raster")

  .loadparam_ifnull()

  risk_indexes <- lapply(link_thresholds,
                                function(lthreshold) {
                                  invisible(
                                    .sean_linkweights(
                                      link_threshold = lthreshold,
                                      host_density_thresholds = host_density_thresholds,
                                      global = global,
                                      geoscale = geoscale,
                                      agg_methods = agg_methods,
                                      dist_method = dist_method,
                                      rast = rast,
                                      reso = reso,
                                      maps = FALSE
                                    )
                                  )
                                })


  risk_indexes <- .flatten_ri(global, risk_indexes)

  if (maps == TRUE) {
    connectivity(risk_indexes,
                 global,
                 geoscale,
                 reso,
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }
  return(risk_indexes)
}

#' @title Calculate sensitivity analysis on parameters
#'
#' @description
#' This function runs sensitivity analysis on parameters based on
#' parameters provided through [set_parameters()].
#' It can be used as an entry point for CCRI.
#' By default, it runs analysis on global sclaes[global_scales()].
#' After analysis is complete,
#' it will suppress maps for outcomes if `maps = FALSE` or
#' [interactive()] is `FALSE`.
#' @param maps logical. `TRUE` if maps are to be plotted, `FALSE` otherwise
#' @param alert logical. `TRUE` if beep sound is to be played, `FALSE` otherwise
#' @return logical. `TRUE` if analysis is completed, `FALSE` otherwise.
#' Errors are not handled.
#' @export
#' @examples
#' \dontrun{
#' # Run analysis on specified parameters.yaml
#' sensitivity_analysis()
#' sensitivity_analysis(FALSE, FALSE)
#' sensitivity_analysis(TRUE, FALSE)
#' }
#' @seealso
#' [sa_onrasters()]
#' [sean()]
#' [global_scales()]
#' [get_parameters()]
#' [set_parameters()]
#' [connectivity()]
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

  risk_indexes <- lapply(crop_rasters,
                         invisible(function(rast) {
                           sa_onrasters(
                             rast = rast,
                             global = isglobal,
                             geoscale = geoscale,
                             link_thresholds = the$parameters_config$`CCRI parameters`$LinkThreshold,
                             host_density_thresholds = cropland_thresholds,
                             agg_methods = agg_methods,
                             dist_method = the$parameters_config$`CCRI parameters`$DistanceStrategy,
                             reso = resolution,
                             maps = FALSE
                           )
                         }))

  risk_indexes <- .flatten_ri(isglobal, risk_indexes)

  if (maps == TRUE) {
    connectivity(risk_indexes,
                 isglobal,
                 geoscale,
                 resolution,
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
                 as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }

  message("sensitivity analysis completed. Refer to maps for results.")
  if (alert == TRUE) {
    beepr::beep(2)
  }

  return(TRUE)
}
