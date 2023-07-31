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
the$distance_matrix <- NULL
the$cropharvest_aggtm <- NULL
the$cropharvest_agglm_crop <- NULL
the$cropharvest_aggtm_crop <- NULL
the$gan <- list(sum = list("east" = NULL, west = NULL),
                  mean = list("east" = NULL, west = NULL))

.gan_table <- function(row, col, val) {
  stopifnot("Not a spatRaster"= tolower(class(val)) == "spatraster") 
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

#' Initialization of crop data
#'
#'   Initialize cropland data with given parameters, it will be later used to calculate CCRI and other functions
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
initialize_cropland_data <- function(cropharvest_raster,
                                     resolution = 12,
                                     geo_scale,
                                     host_density_threshold = 0,
                                     agg_method = "sum") {

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
#' @param betas Vector. Dispersal model beta values.
#' @param link_threshold A threshold value for links.
#' @param metrics A list 2 vectors - metrics and weights.
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated inverse power law
#' @export
#' @inherit ccri details
#' @details
#' This function by itself doesn't run any analysis.
#' It applies inverse powerlaw to the results of [ccri()].
#' If the values required are not loaded into environment, then this function will result in error.
#'
ccri_powerlaw <- function(betas,
                          link_threshold = 0,
                          metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                          rast,
                          crop_cells_above_threshold = NULL,
                          thresholded_crop_values = NULL) {

  if (!.validate_index_cal(betas)) {
    return(0)
  }

  .loadparam_ifnotnull()

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

#' Calculate negative exponential
#' @param gammas Vector. Dispersal model gamma values.
#' @param link_threshold A threshold value for link
#' @inheritParams ccri_powerlaw
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated negative exponential
#' @export
ccri_negative_exp <- function(gammas,
                              link_threshold = 0,
                              metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                              rast,
                              crop_cells_above_threshold = NULL,
                              thresholded_crop_values = NULL) {

  if (!.validate_index_cal(gammas)) {
    return(0)
  }

  .loadparam_ifnotnull()

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
#' @seealso [get_param_metrics()], [sean()]
ccri <- function(
    link_threshold = 0,
    power_law_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
    negative_exponential_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
    rast,
    crop_cells_above_threshold,
    thresholded_crop_values) {

  .loadparam_ifnotnull()
  risk_indexes <- list()

  # TODO: parallelize them
  betas <- as.numeric(the$parameters_config$`CCRI parameters`$DispersalKernelModels$beta)

  if (length(betas) > 0) {
    stopifnot("beta values are not valid" = is.numeric(betas) == TRUE, is.vector(betas) == TRUE)
    risk_indexes <- c(risk_indexes,
                      ccri_powerlaw(betas,
                                    link_threshold,
                                    metrics = power_law_metrics,
                                    rast,
                                    crop_cells_above_threshold = crop_cells_above_threshold,
                                    thresholded_crop_values = thresholded_crop_values
                      ))
  }

  gammas <- as.numeric(the$parameters_config$`CCRI parameters`$DispersalKernelModels$gamma)
  if (length(gammas) > 0) {
    stopifnot("gamma values are not valid" = is.numeric(gammas) == TRUE, is.vector(gammas) == TRUE)
    risk_indexes <- c(risk_indexes,
                      ccri_negative_exp(the$parameters_config$`CCRI parameters`$DispersalKernelModels$gamma,
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
#' @param link_threshold A threshold value for link
#' @param global Logical. `TRUE` if global analysis, `FALSE` otherwise. Default is `TRUE`
#' @param geoscale A geographical extent in form of (Xmin, Xmax, Ymin, Ymax)
#' @param aggregate_methods A list of aggregation methods. It can be sum or mean
#' @param cropharvest_raster A raster object for cropland harvest
#' @param host_density_threshold A host density threshold value
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
#' @details
#' When `global = TRUE`, `geoscale` is ignored and [global_scales()] is used 
#'
#' @seealso Uses [plot_maps()]
sean <- function(link_threshold = 0,
                 global = TRUE,
                 geoscale,
                 aggregate_methods = c("sum", "mean"),
                 rast,
                 host_density_threshold = 0,
                 resolution = 24,
                 maps = TRUE) {

  .loadparam_ifnotnull()

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

    for (agg_method in aggregate_methods) {
      density_data <- initialize_cropland_data(rast,
                                               resolution,
                                               geoext,
                                               host_density_threshold = host_density_threshold,
                                               agg_method)
      lrisk_indexes <- c(lrisk_indexes,
                        ccri(link_threshold,
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

    east_indexes <- sean_geo(global_exts[["east"]])
    .addto_tab("east")

    west_indexes <- sean_geo(global_exts[["west"]])
    .addto_tab("west")
    list(east = east_indexes, west = west_indexes)
  } else {
    sean_geo(geoscale)
  }

  if (maps == TRUE) {
    plot_maps(risk_indexes,
              global,
              geoscale,
              resolution,
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }
  the$is_initialized <- FALSE
  return(risk_indexes)
}

#' Calculate sensitivity analysis on cropland harvested area fraction
#'
#' This function calculates sensitivity analysis on cropland harvested area fraction based on link weight threshold
#' and other provided parameters.
#' It can be used as entry point for sensitivity analysis.
#' @param link_threshold A threshold value for link
#' @param host_density_thresholds A list of host density threshold values
#' @param global `TRUE` if global analysis, `FALSE` otherwise. Default is `TRUE`
#' @param geo_scale A list of longitude and latitude values for cropland analysis
#' @param aggregate_methods A list of aggregation methods
#' @param cropharvest_raster A raster object for cropland harvest
#' @param resolution resolution to plot raster and map
#' @return A list of calculated CCRI values using negative exponential
#' @export
#' @inherit sensitivity_analysis seealso
sean_linkweights <- function(link_threshold = 0,
                             host_density_thresholds,
                             global = TRUE,
                             geo_scale,
                             aggregate_methods,
                             rast,
                             resolution,
                             maps = TRUE) {

  risk_indexes <- lapply(host_density_thresholds,
                                function(threshold) {
                                  invisible(
                                    sean(
                                      link_threshold = link_threshold,
                                      host_density_threshold = threshold,
                                      global = global,
                                      geoscale = geo_scale,
                                      aggregate_methods = aggregate_methods,
                                      rast = rast,
                                      resolution = resolution,
                                      maps = FALSE
                                    )
                                  )
                                })

  risk_indexes <- .flatten_ri(global, risk_indexes)
  if (maps == TRUE) {

    plot_maps(risk_indexes,
              global = global,
              geo_scale,
              resolution,
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }
  return(risk_indexes)
}

#' Run analysis
#' @param rast Raster object which will be used in analysis.
#' @seealso Use [get_rasters()] to obtain raster object.
#' @param global Logical. `TRUE` if global analysis, `FALSE` otherwise. Default is `TRUE`
#' Default is `TRUE`. when `TRUE`, `geoscale` is ignored
#' @param geo_scale Vector. Geographical scale which is coordinates in the form of c(Xmin, Xmax, Ymin, Ymax)
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
#' @inherit sensitivity_analysis seealso
sa_onrasters <- function(rast,
                         global = TRUE,
                         geo_scale,
                         link_thresholds,
                         host_density_thresholds,
                         aggregate_methods = c("sum", "mean"),
                         resolution,
                         maps = TRUE) {

  cat("New analysis started for given raster")

  .loadparam_ifnotnull()

  risk_indexes <- lapply(link_thresholds,
                                function(lthreshold) {
                                  invisible(
                                    sean_linkweights(
                                      link_threshold = lthreshold,
                                      host_density_thresholds = host_density_thresholds,
                                      global = global,
                                      geo_scale = geo_scale,
                                      aggregate_methods = aggregate_methods,
                                      rast = rast,
                                      resolution = resolution,
                                      maps = FALSE
                                    )
                                  )
                                })


  risk_indexes <- .flatten_ri(global, risk_indexes)

  if (maps == TRUE) {
    plot_maps(risk_indexes,
              global,
              geo_scale,
              resolution,
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
#' This function runs sensitivity analysis on parameters based on provided parameters through [set_parameters()].
#' It can be used as entry point for sensitivity analysis.
#' Plots results of sensitivity analysis.
#' @export
#' @examples
#' \dontrun{
#' # Run analysis on specified parameters.yaml
#' sensitivity_analysis()
#' }
#' @seealso
#' [sa_onrasters()]
#' [sean_linkweights]
#' [sean()]
#' [plot_maps()]
sensitivity_analysis <- function(maps = TRUE, notify = TRUE) {

  #.resetglobals()
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
                             geo_scale = geoscale,
                             link_thresholds = the$parameters_config$`CCRI parameters`$LinkThreshold,
                             host_density_thresholds = cropland_thresholds,
                             aggregate_methods = agg_methods,
                             resolution = resolution,
                             maps = FALSE
                           )
                         }))

  risk_indexes <- .flatten_ri(isglobal, risk_indexes)

  if (maps == TRUE) {
    plot_maps(risk_indexes,
              isglobal,
              geoscale,
              resolution,
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$MeanCC),
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Variance),
              as.logical(the$parameters_config$`CCRI parameters`$PriorityMaps$Difference)
    )
  }

  message("sensitivity analysis completed. Refer to maps for results.")
  if (notify == TRUE) {
    beepr::beep(2)
  }

  return(TRUE)
}
