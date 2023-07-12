#' @exportPattern ^[^\\.].*

# Utility functions -------------------------------------------------------


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

#' Global cropland density map
#' Only when user has enabled global analysis
#' @param map_grey_background_extent A raster object for map's grey background
#' @param resolution resolution to plot raster and map
#' @export
global_analysis <- function(map_grey_background_extent, resolution =
                              the$parameters_config$`CCRI parameters`$Resolution) {
  # ```{r, fig.width=20, fig.height=10, dpi=400}

  crop_ext <- terra::ext(-180, 180, -60, 80)
  cropharvest_aggtm_crop1 <- terra::crop(the$cropharvest_aggtm, crop_ext)
  zr_world_mean <- range(0.1, max(terra::values(cropharvest_aggtm_crop1)))

  # Removing pixels outside boundary
  mean_index_raster_val <- terra::values(cropharvest_aggtm_crop1)
  # structure(mean_index_raster_val)

  zeroid <- which(mean_index_raster_val == 0)
  the$cropharvest_aggtm[zeroid] <- NaN

  zero_raster <- terra::rast(.get_helper_filepath(.kzeroraster_file_type))
  cam_zero <- terra::crop(zero_raster, crop_ext)
  mean_index_raster <- terra::disagg(cropharvest_aggtm_crop1, fact = c(resolution, resolution), method = "")
  mean_index_raster_cam <-  terra::crop(mean_index_raster,  geoscale) + .cal_zerorast(mean_index_raster, resolution)

  # Plotting cropland density
  map_grey_background <- terra::rast(.get_helper_filepath(.kmapgreybackground_file_type))

  map_grey_background_cam <- terra::crop(map_grey_background, terra::ext(-180, 180, -60, 80))
  crop_names <- .get_cs_host_names(the$parameters_config)
  plot(map_grey_background_cam,
    col = "grey75", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
    main = paste("Mean in crop area fraction:", crop_names), cex.main = 1.6
  )

  plot(mean_index_raster_cam,
    main = paste("Crop area density: ", crop_names),
    col = .get_palette()(), zlim = zr_world_mean, xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, add = TRUE
  )
}

#' intialize cropland data with geiven paramters, it will be later used to calculate CCRI and other functions
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map  (default: 12)
#' @param geo_scale A list of longitude and latitude values for cropland analysis
#' @param host_density_threshold A threshold value for cropland density (default: 0)
#' @param agg_method A method to aggregate cropland raster (default: "sum")
#' @export
#' @details
#' This function also creates global variables which are result of applying aggregate functions into raster.
#' Theese global variables are used when applying allgorithms - ipl[ccri_powerlaw()] and ne[ccri_negative_exp()].
#'
initialize_cropland_data <- function(cropharvest_raster, resolution = 12, geo_scale,
                                     host_density_threshold = 0, agg_method = "sum") {

  #----------- aggregration -----------------------------
  cropharvest_agg <- terra::aggregate(cropharvest_raster, fact = resolution, fun = agg_method,
                                       na.action = stats::na.omit)
  density_data <- NULL
  if (agg_method == "sum") {
    the$cropharvest_aggtm <- cropharvest_agg / resolution / resolution # TOTAL MEAN
    terra::plot(the$cropharvest_aggtm, col = .get_palette()) # map of cropland density
    #----------- crop cropland area for the given extent ----------
    the$cropharvest_aggtm_crop <- terra::crop(the$cropharvest_aggtm, geo_scale)
    terra::plot(the$cropharvest_aggtm_crop, col = .get_palette()) # TODO: don't show this
    density_data <- .extract_cropland_density(the$cropharvest_aggtm_crop, host_density_threshold)
  } else if (agg_method == "mean") {
    cropharvest_agglm <- cropharvest_agg
    terra::plot(cropharvest_agglm, col = .get_palette())
    #----------- crop cropland area for the given extent ----------
    the$cropharvest_agglm_crop <- terra::crop(cropharvest_agglm, geo_scale)
    terra::plot(the$cropharvest_agglm_crop, col = .get_palette())
    density_data <- .extract_cropland_density(the$cropharvest_agglm_crop, host_density_threshold)
  }

  if (is.null(density_data) || (!is.list(density_data))) {
    stop("unable to extract density data, longitude/latitude")
  }

  # Prepare arguments elements values for the CCRI funtions
  #cropdata1 <- data.frame(density_data$longitude, density_data$latitude, density_data$crop_value)
  # adjustConstant <- 2 # to adjust the distance and make sure the distance >1
  latilongimatr <- terra::xyFromCell(density_data$agg_crop, cell = density_data$crop_values_at) # save the latitude and longitude as new matrix
  #---- use Geosphere package, fun distVincentyEllipsoid() is used to calculate the distance, default distance is meter
  # reference of standard distance in meter for one degree
  dvse <- geosphere::distVincentyEllipsoid(c(0, 0), cbind(1, 0))
  latilongimatr <- as.matrix(latilongimatr)
  temp_matrix <- matrix(-999, nrow(latilongimatr), nrow(latilongimatr))

  # TODO: set round limit programitcally
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
#' @return A list of calculated CCRI values
#' @export
#' @details
#' Network metrics should be passed as a list of vectors e.g. `list(metrics = c("betweeness"), weights = c(100))`.
#' Default values are fetched from `parameters.yaml` and arguments uses the same structure.
#' @seealso [get_param_metrics()], [sensitivity_analysis_on_geoextent_scale()]
calculate_ccri <- function(
    link_threshold = 0,
    power_law_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
    negative_exponential_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
    crop_cells_above_threshold, thresholded_crop_values) {

  # TODO: parallelize them
  ccri_powerlaw(the$parameters_config$`CCRI parameters`$DispersalParameterBeta, link_threshold,
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
sensitivity_analysis_on_geoextent_scale <- function(link_threshold = 0, geo_scale,
                                                    aggregate_methods = c("sum", "mean"), cropharvest_raster,
                                                    host_density_threshold = 0, resolution = 24) {
  cat("\nRunning senstivity analysis for the extent: [", geo_scale, "],
      Link threshold: ", link_threshold,
      "Host density threshold: ", host_density_threshold, "\n")

  geo_areaext <- terra::ext(as.numeric(unlist(geo_scale))) # list
  the$result_index_list <- list()

  mets <- get_param_metrics(the$parameters_config$`CCRI parameters`$NetworkMetrics)

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
            as.logical(resolution, the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
            as.logical(resolution, the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
            as.logical(resolution, the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
            )

  the$is_initialized <- FALSE
  invisible(3)
}

#' Calculate sensitivity analysis on cropland harvested area fraction
#' This function calculates sensitivity analysis on cropland harvested area fraction based on provided parameters.
#' It can be used as entry point for sensitivity analysis.
#' @param link_thresholds A list of threshold values for link
#' @param host_density_thresholds A list of host density threshold values
#' @param geo_scale longitude and latitude values for cropland analysis
#' @param aggregate_methods A list of aggregation methods
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
sensitivity_analysis_on_cropland_threshold <- function(link_thresholds,
                                                       host_density_thresholds,
                                                       geo_scale, aggregate_methods = c("sum", "mean"),
                                                       cropharvest_raster,
                                                       resolution) {
  lapply(link_thresholds, sensitivity_analysis_on_link_weight,
    host_density_thresholds = host_density_thresholds,
    geo_scale = geo_scale, aggregate_methods = aggregate_methods,
    cropharvest_raster = cropharvest_raster, resolution = resolution
  )
}

#' Calculate sensitivity analysis on cropland harvested area fraction
#' This function calculates sensitivity analysis on cropland harvested area fraction based on provided parameters.
#' It can be used as entry point for sensitivity analysis.
#' @param link_threshold A threshold value for link
#' @param host_density_thresholds A list of host density threshold values
#' @param geo_scale A list of longitude and latitude values for cropland analysis
#' @param aggregate_methods A list of aggregation methods
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
sensitivity_analysis_on_link_weight <- function(link_threshold = 0,
                                                host_density_thresholds, geo_scale,
                                                aggregate_methods,
                                                cropharvest_raster,
                                                resolution) {

  lapply(host_density_thresholds, sensitivity_analysis_on_geoextent_scale,
    link_threshold = link_threshold, geo_scale = geo_scale,
    aggregate_methods = aggregate_methods,
    cropharvest_raster = cropharvest_raster, resolution = resolution
  )
}

#' Run analysis
#' @param cropharvest_raster Raster object which will be used in analysis.
#' @seealso Use [get_rasters()] to obtain raster object.
#' @param geo_scales List of geographical scales to be used in analysis.
#' The rasters will be cropped to provided geographical scale.
#' Independent analysis is run on each sale.
#' @inherit sensitivity_analysis_on_cropland_threshold
#' @export
sa_onrasters <- function(cropharvest_raster,
                         geo_scales,
                         link_thresholds,
                         host_density_thresholds,
                         aggregate_methods = c("sum", "mean"),
                         resolution) {

  cat("New analysis started for given raster")

  lapply(geo_scales,
         sensitivity_analysis_on_cropland_threshold,
         link_thresholds = link_thresholds,
         host_density_thresholds = host_density_thresholds,
         aggregate_methods = aggregate_methods,
         cropharvest_raster = cropharvest_raster,
         resolution = resolution)
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

  lapply(crop_rasters, sa_onrasters,
         geo_scales = geo_scales,
         link_thresholds = the$parameters_config$`CCRI parameters`$LinkThreshold,
         host_density_thresholds = cropland_thresholds,
         aggregate_methods = agg_methods,
         resolution = resolution
         )
}
