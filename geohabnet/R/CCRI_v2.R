#' @exportPattern ^[^\\.].*

# Utility functions -------------------------------------------------------

.loadparam_ifnotnull <- function() {
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
the$result_index_list <- list()
the$distance_matrix <- NULL
the$cropharvest_aggtm <- NULL
the$cropharvest_agglm_crop <- NULL
the$cropharvest_aggtm_crop <- NULL

#' Initialize cropland data with given parameters, it will be later used to calculate CCRI and other functions
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map  (default: 12)
#' @param geo_scale A list of longitude and latitude values for cropland analysis
#' @param host_density_threshold A threshold value for cropland density (default: 0)
#' @param agg_method A method to aggregate cropland raster (default: "sum")
#' @export
#' @details
#' This function also creates global variables which are result of applying aggregate functions into raster.
#' These global variables are used when applying algorithms - [ccri_powerlaw()] and [ccri_negative_exp()].
#'
initialize_cropland_data <- function(cropharvest_raster, resolution = 12, geo_scale,
                                     host_density_threshold = 0, agg_method = "sum") {

  # aggregation
  cropharvest_agg <- terra::aggregate(cropharvest_raster, fact = resolution, fun = agg_method,
                                       na.action = stats::na.omit)
  density_data <- NULL
  if (agg_method == "sum") {
    the$cropharvest_aggtm <- cropharvest_agg / resolution / resolution # TOTAL MEAN
    # crop cropland area for the given extent
    the$cropharvest_aggtm_crop <- terra::crop(the$cropharvest_aggtm, geo_scale)
    density_data <- .extract_cropland_density(the$cropharvest_aggtm_crop, host_density_threshold)
  } else if (agg_method == "mean") {
    the$cropharvest_agglm <- cropharvest_agg
    # crop cropland area for the given extent
    the$cropharvest_agglm_crop <- terra::crop(the$cropharvest_agglm, geo_scale)
    density_data <- .extract_cropland_density(the$cropharvest_agglm_crop, host_density_threshold)
  }

  if (is.null(density_data) || (!is.list(density_data))) {
    stop("unable to extract density data, longitude/latitude")
  }

  # Prepare arguments elements values for the CCRI functions
  #cropdata1 <- data.frame(density_data$longitude, density_data$latitude, density_data$crop_value)
  # adjustConstant <- 2 # to adjust the distance and make sure the distance >1
  # save the latitude and longitude as new matrix
  latilongimatr <- terra::xyFromCell(density_data$agg_crop, cell = density_data$crop_values_at)
  #---- use Geosphere package, fun distVincentyEllipsoid() is used to calculate the distance, default distance is meter
  # reference of standard distance in meter for one degree
  dvse <- geosphere::distVincentyEllipsoid(c(0, 0), cbind(1, 0))
  latilongimatr <- as.matrix(latilongimatr)
  temp_matrix <- matrix(-999, nrow(latilongimatr), nrow(latilongimatr))

  # TODO: set round limit
  for (i in seq_len(nrow(latilongimatr))) {
    temp_matrix[i, ] <- geosphere::distVincentyEllipsoid(round(latilongimatr[i, ], 5), latilongimatr) / dvse
  }

  the$distance_matrix <- temp_matrix

  the$is_initialized <- TRUE
  return(density_data)
}


# Aggregate -----------------------------------------------------


#' Calculate inverse power law
#' @param dispersal_parameter_beta_vals A list of beta values
#' @param link_threshold A threshold value for links.
#' @param metrics A list 2 vectors - metrics and weights.
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated inverse power law
#' @export
#' @inherit calculate_ccri details
#'
ccri_powerlaw <- function(dispersal_parameter_beta_vals,
                          link_threshold = 0,
                          metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                          crop_cells_above_threshold = NULL,
                          thresholded_crop_values = NULL) {

  if (!.validate_index_cal(dispersal_parameter_beta_vals)) {
    return(0)
  }

  .loadparam_ifnotnull()

  index_list <- lapply(dispersal_parameter_beta_vals, model_powerlaw,
                       link_threshold = link_threshold,
                       the$distance_matrix,
                       thresholded_crop_values,
                       adj_mat = NULL,
                       the$cropharvest_aggtm_crop,
                       crop_cells_above_threshold,
                       metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw
  )

  the$result_index_list <- c(the$result_index_list, index_list)
  invisible()
}

#' Calculate negative exponential
#' @param dispersal_parameter_gamma_vals A list of gamma values
#' @param link_threshold A threshold value for link
#' @inheritParams ccri_powerlaw
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated negative exponential
#' @export
ccri_negative_exp <- function(dispersal_parameter_gamma_vals,
                              link_threshold = 0,
                              metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                              crop_cells_above_threshold = NULL,
                              thresholded_crop_values = NULL) {

  if (!.validate_index_cal(dispersal_parameter_gamma_vals)) {
    return(0)
  }

  .loadparam_ifnotnull()

  index_list <- lapply(dispersal_parameter_gamma_vals,
                       model_neg_exp,
                       link_threshold = link_threshold,
                       the$distance_matrix,
                       thresholded_crop_values,
                       adj_mat = NULL,
                       the$cropharvest_aggtm_crop,
                       crop_cells_above_threshold,
                       metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw
  )

  the$result_index_list <- c(the$result_index_list, index_list)
  invisible()
}


# Utility functions -------------------------------------------------------

.validate_index_cal <- function(vals_list) {
  ready <- TRUE
  if (!the$is_initialized) {
    stop("Not initialized. Call initializeCroplandData()")
  }
  if (!is.list(vals_list)) {
    warning("argument is not a list")
    ready <- FALSE
  }
  return(ready)
}

#' Get geographical scales from the parameters
#' This function returns a list of geographical scales set global and custom extent in parameters.yaml
#' @return A list of geographical scales
#' @export
get_geographic_scales <- function() {
  perform_global_analysis <- the$parameters_config$`CCRI parameters`$Longitude_Latitude$Global
  geo_scales <- list()
  if (perform_global_analysis) {
    geo_scales <- list(
      the$parameters_config$`CCRI parameters`$Longitude_Latitude$EastExt,
      the$parameters_config$`CCRI parameters`$Longitude_Latitude$WestExt
    )
  }
  custom_scales <- the$parameters_config$`CCRI parameters`$Longitude_Latitude$CustomExt
  if (!(is.null(custom_scales) || is.na(custom_scales) || length(custom_scales) == 0)) {
    geo_scales <- c(geo_scales, lapply(custom_scales, as.numeric))
  }

  return(geo_scales)
}

# CCRI functions ----------------------------------------------------------
#' Calculate Cropland Connectivity Risk Index (CCRI)
#'
#'  This function calculates CCRI for given parameters using power law and negative exponential.
#'  It's required to call [initialize_cropland_data()] before calling this function.
#' @param link_threshold A threshold value for link
#' @param power_law_metrics A list of 2 vectors - power law metrics and weights
#' @param negative_exponential_metrics A list of of 2 vectors - negative exponential metrics
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated CCRI values which can be accessed using `the$result_index_list`
#' @export
#' @details
#' Network metrics should be passed as a list of vectors e.g. `list(metrics = c("betweeness"), weights = c(100))`.
#' Default values are fetched from `parameters.yaml` and arguments uses the same structure.
#' @seealso [get_param_metrics()], [sensitivity_analysis_on_params()]
calculate_ccri <- function(
    link_threshold = 0,
    power_law_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
    negative_exponential_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
    crop_cells_above_threshold, thresholded_crop_values) {

  .loadparam_ifnotnull()

  # TODO: parallelize them
  ccri_powerlaw(the$parameters_config$`CCRI parameters`$DispersalParameterBeta,
                link_threshold,
                metrics = power_law_metrics,
                crop_cells_above_threshold = crop_cells_above_threshold,
                thresholded_crop_values = thresholded_crop_values)

  ccri_negative_exp(the$parameters_config$`CCRI parameters`$DispersalParameterGamma,
                    link_threshold,
                    metrics = negative_exponential_metrics,
                    crop_cells_above_threshold = crop_cells_above_threshold,
                    thresholded_crop_values = thresholded_crop_values)
}

# Sensitivity analysis ----------------------------------------------------
#' Calculate sensitivity analysis on cropland harvested area fraction
#' This function calculates sensitivity analysis on cropland harvested area fraction based on provided parameters.
#' It can be used as entry point for sensitivity analysis.
#' @param link_threshold A threshold value for link
#' @param geo_scale A list of longitude and latitude values for cropland analysis
#' @param aggregate_methods A list of aggregation methods. It can be sum or mean.
#' @param cropharvest_raster A raster object for cropland harvest
#' @param host_density_threshold A host density threshold value
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
#' @seealso [plot_maps()]
sensitivity_analysis_on_params <- function(link_threshold = 0,
                                           geo_scale,
                                           aggregate_methods = c("sum", "mean"),
                                           cropharvest_raster,
                                           host_density_threshold = 0,
                                           resolution = 24) {
  cat("\nRunning senstivity analysis for the extent: [", geo_scale, "],
      Link threshold: ", link_threshold,
      "Host density threshold: ", host_density_threshold, "\n")

  .loadparam_ifnotnull()

  geo_areaext <- terra::ext(as.numeric(unlist(geo_scale))) # list
  the$result_index_list <- list()

  mets <- get_param_metrics(the$parameters_config)

  for (agg_method in aggregate_methods) {
    cropland_density_info <- initialize_cropland_data(cropharvest_raster, resolution, geo_areaext,
                             host_density_threshold = host_density_threshold, agg_method)

    calculate_ccri(link_threshold,
                   power_law_metrics =
                     mets$pl,
                   negative_exponential_metrics =
                     mets$ne,
                  crop_cells_above_threshold =
                    cropland_density_info$crop_values_at,
                  thresholded_crop_values =
                    cropland_density_info$crop_value)
  }

  plot_maps(the$result_index_list,
            geo_scale,
            resolution,
            as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
            as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
            as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
            )

  the$is_initialized <- FALSE
  invisible(3)
}

#' Calculate sensitivity analysis on cropland harvested area fraction
#'   This function calculates sensitivity analysis on cropland harvested area fraction
#'   based on geographical scale threshold and other provided parameters.
#'   It can be used as entry point for sensitivity analysis.
#' @param link_thresholds A list of threshold values for link
#' @param host_density_thresholds A list of host density threshold values
#' @param geo_scale longitude and latitude values for cropland analysis
#' @param aggregate_methods A list of aggregation methods
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
sensitivity_analysis_on_geoscale <- function(link_thresholds,
                                             host_density_thresholds,
                                             geo_scale,
                                             aggregate_methods = c("sum", "mean"),
                                             cropharvest_raster,
                                             resolution) {
  lapply(link_thresholds,
         function(lthreshold) {
           invisible(
             sensitivity_analysis_on_link_weight(
               link_threshold = lthreshold,
               host_density_thresholds = host_density_thresholds,
               geo_scale = geo_scale,
               aggregate_methods = aggregate_methods,
               cropharvest_raster = cropharvest_raster,
               resolution = resolution
               ))})
}

#' Calculate sensitivity analysis on cropland harvested area fraction
#' This function calculates sensitivity analysis on cropland harvested area fraction based on link weight threshold
#' and other provided parameters.
#' It can be used as entry point for sensitivity analysis.
#' @param link_threshold A threshold value for link
#' @param host_density_thresholds A list of host density threshold values
#' @param geo_scale A list of longitude and latitude values for cropland analysis
#' @param aggregate_methods A list of aggregation methods
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
#' @inherit senstivity_analysis seealso
sensitivity_analysis_on_link_weight <- function(link_threshold = 0,
                                                host_density_thresholds,
                                                geo_scale,
                                                aggregate_methods,
                                                cropharvest_raster,
                                                resolution) {

  lapply(host_density_thresholds, function(threshold) {
    invisible(
      sensitivity_analysis_on_params(
        link_threshold = link_threshold,
        host_density_threshold = threshold,
        geo_scale = geo_scale,
        aggregate_methods = aggregate_methods,
        cropharvest_raster = cropharvest_raster,
        resolution = resolution
      )
    )
  })
}

#' Run analysis
#' @param cropharvest_raster Raster object which will be used in analysis.
#' @seealso Use [get_rasters()] to obtain raster object.
#' @param geo_scales List of geographical scales to be used in analysis.
#' The rasters will be cropped to provided geographical scale.
#' Independent analysis is run on each sale.
#' @inherit sensitivity_analysis_on_geoscale
#' @export
#' @examples
#' rr <- get_rasters(list(monfreda = c("coffee")))
#' sa_onrasters(rr[[1]],
#'             geo_scales = list(c(-115, -75, 5, 32)),
#'             c(0.0001, 0.00004),
#'             c(0.0001, 0.00005),
#'             c("sum", "mean"),
#'             resolution = 12)
#' sa_onrasters(rr[[1]],
#'             geo_scales = list(c(-115, -75, 5, 32)),
#'             0.0001,
#'             0.00001,
#'             c("sum"),
#'             resolution = 12)
#'
#' @inherit senstivity_analysis seealso
sa_onrasters <- function(cropharvest_raster,
                         geo_scales,
                         link_thresholds,
                         host_density_thresholds,
                         aggregate_methods = c("sum", "mean"),
                         resolution) {

  cat("New analysis started for given raster")

  .loadparam_ifnotnull()

  lapply(geo_scales,
         function(geoscale) {
           invisible(
             sensitivity_analysis_on_geoscale(
               geo_scale = geoscale,
               link_thresholds = link_thresholds,
               host_density_thresholds = host_density_thresholds,
               aggregate_methods = aggregate_methods,
               cropharvest_raster = cropharvest_raster,
               resolution = resolution
               ))})
}

#' @title Calculate sensitivity analysis on parameters
#' @description
#' This function runs sensitivity analysis on parameters based on provided parameters through [set_parameters()].
#' It can be used as entry point for sensitivity analysis.
#' Plots results of sensitivity analysis.
#' @export
#' @examples
#' \dontrun{
#' # Run analysis on specified parameters.yaml
#' senstivity_analysis()
#' }
#' @seealso
#' [sa_onrasters()]
#' [sensitivity_analysis_on_link_weight]
#' [sensitivity_analysis_on_geoscale()]
#' [sensitivity_analysis_on_params()]
#' [plot_maps()]
senstivity_analysis <- function() {

  the$is_initialized <- FALSE
  the$parameters_config <- load_parameters()

  # cutoff adjacency matrix
  cropland_thresholds <- the$parameters_config$`CCRI parameters`$HostDensityThreshold

  # crop data
  crop_rasters <- get_rasters(the$parameters_config$`CCRI parameters`$Hosts)
  agg_methods <- the$parameters_config$`CCRI parameters`$AggregationStrategy # list

  # maps
  geo_scales <- get_geographic_scales()

  # resolution
  resolution <- the$parameters_config$`CCRI parameters`$Resolution

  lapply(crop_rasters,
        invisible(
          function(rast) {
            sa_onrasters(
              cropharvest_raster = rast,
              geo_scales = geo_scales,
              link_thresholds = the$parameters_config$`CCRI parameters`$LinkThreshold,
              host_density_thresholds = cropland_thresholds,
              aggregate_methods = agg_methods,
              resolution = resolution
              )}))
}
