#' @exportPattern ^[^\\.].*

# Utility functions -------------------------------------------------------

#' @title Get raster object for crop
#' @description Get cropland information in a form of raster object from data source for crop
#' @param crop_name Name of the crop
#' @param data_source Data source for cropland information
#' @return Raster object
#' @export
#' @examples
#' get_cropharvest_raster("avocado", "monfreda")
get_cropharvest_raster <- function(crop_name, data_source) {
  # supported sources
  sources <- get_supported_sources()
  if (!(data_source %in% sources)) {
    stop(paste("data source: ", data_source, " is not supported"))
  }
  cropharvest_r <- .get_cropharvest_raster_helper(crop_name = crop_name, data_source = data_source)
  cropharvest_r <- raster::raster(terra::sources(cropharvest_r))
  return(cropharvest_r)
}

#' Get raster object from tif file
#'
#' This is a wrapper of \code{raster::raster()} and generates a raster object if provided with a TIF file.
#'
#' @param path_to_tif TIF file
#' @return Raster object
#' @examples
#' \dontrun{
#' # Generate raster for usage
#' get_crop_raster_fromtif(system.file("avocado_HarvestedAreaFraction.tif", "tifs",
#'                                    package = "geohabnet", mustWork = TRUE))
#' }
get_crop_raster_fromtif <- function(path_to_tif) {
  stopifnot(file.exists(path_to_tif), "Not a valid path",
            stringr::str_sub(path_to_tif, start = -4) == ".tif", "Not a tif file")
  return(raster::raster(path_to_tif))
}


#' @title Get sum of rasters for individual crops
#' 
#' @description
#' Takes crop names and returns raster object which is sum of raster of individual crops.
#' Currently, only supports crops listed in
#' [geodata::monfredaCrops()], [geodata::spamCrops()]
#' If crop is present in multiple sources, then their mean is calculated.
#' @param crop_names A named list of source along with crop names
#' @return Raster object which is sum of all the individual crop rasters
#' @export
#' @examples
#' \dontrun{
#' get_cropharvest_raster_sum(list(monfreda = c("wheat", "barley"), spam = c("wheat", "potato")))
#' }
get_cropharvest_raster_sum <- function(crop_names) {
  if (!is.list(crop_names) || length(crop_names) == 0) {
    stop("Input 'crop_names' must be a non-empty list of crop names.")
  }

  # output: list("wheat" = c("monfreda", "spam"), "barley" = c("monfreda"), "potato" = c("spam"))
  crops <- list()

  for (src in get_supported_sources()) {
    for (crop_name in crop_names[[src]]) {
      crops[[crop_name]] <- c(crops[[crop_name]], src)
    }
  }

  # calculate sum of rasters

  # iterate named lists
  # crop names
  nams <- names(crops)
  cropharvests <- list()
  for (i in seq_along(crops)) {
    single_crop_rasters <- list()
    for (j in crops[[i]]) {
      single_crop_rasters <- append(single_crop_rasters, get_cropharvest_raster(nams[i], j))
    }
    len_scr <- length(single_crop_rasters)
    if (len_scr > 1) {
      cropharvests <- c(cropharvests, raster::calc(raster::stack(single_crop_rasters), fun = sum) / len_scr)
    } else {
      cropharvests <- c(cropharvests, single_crop_rasters)
    }
  }

  return(Reduce("+", cropharvests))
}

.extract_lon_lat <- function(crop_cells_above_threshold, cropharvest_agg_crop) {
  #----------- Extract xy corrdination for "povalue" cells ---------
  lon <- NULL # xmin
  lat <- NULL # ymax

  for (i in seq_along(crop_cells_above_threshold)) {
    temp <- raster::extentFromCells(cropharvest_agg_crop, crop_cells_above_threshold[i])
    av_xmin0 <- temp[1]
    lon <- c(lon, av_xmin0)
    av_ymax0 <- temp[4]
    lat <- c(lat, av_ymax0)
  }
  return(list(longitude = lon, latitude = lat))
}

#----------- Extract cropland density data -----------------------
.extract_cropland_density <- function(cropharvest_agg_crop, host_density_threshold) {
  crop_values <- raster::getValues(cropharvest_agg_crop)
  max_val <- max(crop_values, na.rm = TRUE)
  if (max_val <= host_density_threshold) {
    stop(paste("host density threshold: ", host_density_threshold,
               " is greater than the max value: ", max_val, " of aggregate raster"))
  }
  crop_cells_above_threshold <- which(crop_values > host_density_threshold)
  thresholded_crop_values <- crop_values[crop_cells_above_threshold]
  return(c(.extract_lon_lat(crop_cells_above_threshold, cropharvest_agg_crop),
           list(crop_value = thresholded_crop_values, crop_values_at = crop_cells_above_threshold)))
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

  cropharvest_aggtm_crop1 <- raster::crop(the$cropharvest_aggtm, raster::extent(-180, 180, -60, 80))
  zr_world_mean <- range(0.1, max(raster::getValues(cropharvest_aggtm_crop1)))

  # Removing pixels outside boundary
  mean_index_raster_val <- raster::getValues(cropharvest_aggtm_crop1)
  # structure(mean_index_raster_val)

  zeroid <- which(mean_index_raster_val == 0)
  the$cropharvest_aggtm[zeroid] <- NaN

  zero_raster <- raster::raster(.get_helper_filepath(.kzeroraster_file_type))
  cam_zero <- raster::crop(zero_raster, raster::extent(-180, 180, -60, 80))
  mean_index_raster <- raster::disaggregate(cropharvest_aggtm_crop1, fact = c(resolution, resolution), method = "")
  mean_index_raster_cam <- mean_index_raster + cam_zero

  # Plotting cropland density
  map_grey_background <- raster::raster(.get_helper_filepath(.kmapgreybackground_file_type))

  map_grey_background_cam <- raster::crop(map_grey_background, raster::extent(-180, 180, -60, 80))
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
initialize_cropland_data <- function(cropharvest_raster, resolution = 12, geo_scale,
                                     host_density_threshold = 0, agg_method = "sum") {

  #----------- aggregration -----------------------------
  cropharvest_agg <- raster::aggregate(cropharvest_raster, fact = resolution, fun = agg_method,
                                       na.action = stats::na.omit)
  density_data <- NULL
  if (agg_method == "sum") {
    the$cropharvest_aggtm <- cropharvest_agg / resolution / resolution # TOTAL MEAN
    raster::plot(the$cropharvest_aggtm, col = .get_palette()) # map of cropland density
    #----------- crop cropland area for the given extent ----------
    the$cropharvest_aggtm_crop <- raster::crop(the$cropharvest_aggtm, geo_scale)
    raster::plot(the$cropharvest_aggtm_crop, col = .get_palette()) # TODO: don't show this
    density_data <- .extract_cropland_density(the$cropharvest_aggtm_crop, host_density_threshold)
  } else if (agg_method == "mean") {
    cropharvest_agglm <- cropharvest_agg
    raster::plot(cropharvest_agglm, col = .get_palette())
    #----------- crop cropland area for the given extent ----------
    the$cropharvest_agglm_crop <- raster::crop(cropharvest_agglm, geo_scale)
    raster::plot(the$cropharvest_agglm_crop, col = .get_palette())
    density_data <- .extract_cropland_density(the$cropharvest_agglm_crop, host_density_threshold)
  }

  if (is.null(density_data) || (!is.list(density_data))) {
    stop("unable to extract density data, longitude/latitude")
  }

  #---------------------------------------------------------------
  # Prepare arguments elements values for the CCRI funtions
  cropdata1 <- data.frame(density_data$longitude, density_data$latitude, density_data$crop_value)
  # adjustConstant <- 2 # to adjust the distance and make sure the distance >1
  latilongimatr <- cropdata1[, c(1:2)] # save the latitude and longitude as new matrix
  #---- use Geosphere package, fun distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
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
#' @param link_threshold A threshold value for link
#' @param betweenness_metric A boolean value to calculate betweenness metric
#' @param node_strength A boolean value to calculate node strength
#' @param sum_of_nearest_neighbors A boolean value to calculate sum of nearest neighbors
#' @param eigenvector_centrality A boolean value to calculate eigenvector centrality
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated inverse power law
#' @export
ccri_powerlaw <- function(dispersal_parameter_beta_vals, link_threshold = 0, betweenness_metric = FALSE,
                          node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE,
                          crop_cells_above_threshold = NULL, thresholded_crop_values = NULL) {
  if (!.validate_index_cal(dispersal_parameter_beta_vals)) {
    return(0)
  }

  index_list <- lapply(dispersal_parameter_beta_vals, ccri_powerlaw_function, link_threshold = link_threshold,
                       the$distance_matrix, thresholded_crop_values, the$cropharvest_aggtm_crop,
                       crop_cells_above_threshold, betweenness_metric = betweenness_metric,
                       node_strength = node_strength, sum_of_nearest_neighbors = sum_of_nearest_neighbors,
                       eigenvector_centrality = eigenvector_centrality
  )

  the$result_index_list <- c(the$result_index_list, index_list)
}

#' Calculate negative exponential
#' @param dispersal_parameter_gamma_vals A list of gamma values
#' @param link_threshold A threshold value for link
#' @param betweenness_metric A boolean value to calculate betweenness metric
#' @param node_strength A boolean value to calculate node strength
#' @param sum_of_nearest_neighbors A boolean value to calculate sum of nearest neighbors
#' @param eigenvector_centrality A boolean value to calculate eigenvector centrality
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated negative exponential
#' @export
ccri_negative_exponential <- function(dispersal_parameter_gamma_vals, link_threshold = 0, betweenness_metric = FALSE,
                                      node_strength = FALSE, sum_of_nearest_neighbors = FALSE,
                                      eigenvector_centrality = FALSE, crop_cells_above_threshold = NULL,
                                      thresholded_crop_values = NULL) {

  if (!.validate_index_cal(dispersal_parameter_gamma_vals)) {
    return(0)
  }

  index_list <- lapply(dispersal_parameter_gamma_vals,
    ccri_neg_exponential_function, link_threshold = link_threshold, the$distance_matrix, thresholded_crop_values,
    the$cropharvest_aggtm_crop, crop_cells_above_threshold, betweenness_metric, node_strength,
    sum_of_nearest_neighbors, eigenvector_centrality
  )

  the$result_index_list <- c(the$result_index_list, index_list)
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

#' Get geographical scales from the paramters
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

#' Calculate raster objects for given extent and resolution
#'  This function returns a list of zero raster and map grey background extent
#' @param geoscale A list of longitude and latitude values for cropland analysis
#' @param mean_index_raster A raster object for mean index raster
#' @param resolution resolution to plot raster and map
#' @return A list of zero raster and map grey background extent
#' @export
calculate_zero_raster <- function(geoscale, mean_index_raster,
                                  resolution = the$parameters_config$`CCRI parameters`$Resolution) {
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  # TODO: is there any other way to get 0 raster?
  zero_raster <- raster::raster(.get_helper_filepath(.kzeroraster_file_type))
  ext_zero <- raster::crop(zero_raster, geoscale)
  mean_index_raster <- raster::disaggregate(mean_index_raster, fact = c(resolution, resolution), method = "")
  mean_index_raster_ext <- mean_index_raster + ext_zero
  # TODO: remove this plot..use the one below with col = grey75
  raster::plot(mean_index_raster_ext,
    col = .get_palette(), zlim = c(0.000000000000, 1), xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, main = paste(
      "Mean cropland connectivity risk index from sensitivity analysis:",
      paste(the$parameters_config$`CCRI parameters`$Hosts, collapse = ",")
    ),
    cex.main = 0.7
  )
  raster::plot(rworldmap::countriesLow, add = TRUE)

  map_grey_background_ext <- .get_map_grey_background_extent(geoscale)

  raster::plot(map_grey_background_ext,
    col = "grey75", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
    main = paste(
      "Mean cropland connectivity risk index from sensitivity analysis:",
      paste(the$parameters_config$`CCRI parameters`$Hosts, collapse = ",")
    ), cex.main = 0.7
  )
  raster::plot(mean_index_raster_ext,
    col = .get_palette(), zlim = c(0.000000000000, 1), xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, add = TRUE
  )

  raster::plot(rworldmap::countriesLow, add = TRUE, border = "white")

  return(c(zero_raster_extent = ext_zero, map_grey_background_extent = map_grey_background_ext, use.names = TRUE))
}

#' Calculate variance of CCRI
#'    This function produces a map of variance of CCRI based on inpt parameters
#' @param indexes A list of index values
#' @param variance_mean_index_raster A raster object for variance mean index raster
#' @param zero_extent_raster A raster object for zero extent raster
#' @param map_grey_background_ext A raster object for map grey background extent
#' @param resolution resolution to plot raster and map
#' @export
ccri_variance <- function(indexes, variance_mean_index_raster, zero_extent_raster, map_grey_background_ext,
                          resolution = the$parameters_config$`CCRI parameters`$Resolution) {
  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  variance_mean_index_ext <- apply(do.call(cbind, indexes), 1, stats::var)


  variance_mean_index_raster[] <- variance_mean_index_ext
  z_var_w <- range(variance_mean_index_ext[which(variance_mean_index_ext > 0)])
  raster::plot(variance_mean_index_raster,
    col = .get_palette(), zlim = z_var_w, xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, main = paste(
      "Variance in Cropland Connectivity for range: ",
      paste(z_var_w, collapse = " to ")
    )
  )
  raster::plot(rworldmap::countriesLow, add = TRUE)

  #----------------------------------------------------

  variance_mean_index_raster_ext_disagg <- raster::disaggregate(variance_mean_index_raster,
    fact = c(resolution, resolution), method = ""
  )
  variance_mean_index_raster_ext_disagg <- variance_mean_index_raster_ext_disagg + zero_extent_raster


  # TODO: explore colors
  raster::plot(map_grey_background_ext,
    col = "grey75", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
    main = paste(
      "Variance in cropland connectivity risk index from sensitivity analysis:",
      paste(the$parameters_config$`CCRI parameters`$Hosts, collapse = ",")
    ), cex.main = 0.7
  )
  raster::plot(variance_mean_index_raster_ext_disagg,
    col = .get_palette(), zlim = z_var_w, xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, add = TRUE
  )
  raster::plot(rworldmap::countriesLow, add = TRUE)
}

# difference map ----------------------------------------------------------
#' Calculate difference map
#' This function produces a map of difference in rank of cropland harvested area fraction
#' @param mean_index_raster_diff A raster object for mean index raster difference
#' @param cropharvest_aggtm_crop A raster object for cropland harvest
#' @param cropharvest_agglm_crop A raster object for cropland harvest
#' @param zero_extent_raster A raster object for zero extent raster
#' @param map_grey_background_ext A raster object for map grey background extent
#' @param resolution resolution to plot raster and map
#' @export
calculate_difference_map <- function(mean_index_raster_diff, cropharvest_aggtm_crop, cropharvest_agglm_crop,
                                     zero_extent_raster, map_grey_background_ext,
                                     resolution = the$parameters_config$`CCRI parameters`$Resolution) {
  # difference map
  if (missing(cropharvest_aggtm_crop) || missing(cropharvest_agglm_crop)) {
    message("Either sum or mean aggregate is missing. Aborting diffrence calculation")
    return(NULL)
  }
  if (is.null(cropharvest_aggtm_crop) || is.null(cropharvest_agglm_crop)) {
    message("Either sum or mean aggregate is missing. Aborting diffrence calculation")
    return(NULL)
  }

  ccri_id <- which(mean_index_raster_diff[] > 0)
  meantotalland_w <- sum(cropharvest_aggtm_crop, cropharvest_agglm_crop, na.rm = TRUE) / 2

  meanindexcell_w <- mean_index_raster_diff[][ccri_id]
  meantotallandcell_w <- meantotalland_w[][ccri_id]

  # mean cropland minus mean index, negative value means importance of cropland reduce,
  # positive value means importance increase, zero means the importance of cropland doesn't change.
  rankdifferent_w <- rank(meantotallandcell_w * (-1)) - rank(meanindexcell_w * (-1))
  mean_index_raster_diff[] <- NaN
  mean_index_raster_diff[][ccri_id] <- rankdifferent_w

  maxrank_w <- max(abs(rankdifferent_w))
  zr2 <- range(-maxrank_w, maxrank_w)

  # TODO: not required
  paldif4 <- .get_palette_for_diffmap()

  raster::plot(mean_index_raster_diff, main = paste(
    "Difference in rank of cropland harvested area fraction and CCRI:",
    paste(the$parameters_config$`CCRI parameters`$Hosts, collapse = ",")
  ), col = paldif4, zlim = zr2, xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, cex.main = 0.7)
  raster::plot(rworldmap::countriesLow, add = TRUE)

  # mean_index_raster_diff[]
  #--------------------------------------------------
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  # ZeroRaster <- raster("ZeroRaster.tif")
  # West_Zero <- crop(ZeroRaster, west_ext)
  # rasters <- adjust_rasterpair_extent(mean_index_raster_diff, zeroExtentRaster)
  mean_index_raster_diff_disagg <- raster::disaggregate(mean_index_raster_diff,
    fact = c(resolution, resolution), method = ""
  )
  mean_index_raster_diff_disagg <- mean_index_raster_diff_disagg + zero_extent_raster

  crop_names <- .get_cs_host_names(the$parameters_config)
  # TODO: not required
  raster::plot(mean_index_raster_diff_disagg,
    main = paste("Difference in rank of cropland harvested area fraction and CCRI:", crop_names),
    col = paldif4, zlim = zr2, xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, cex.main = 0.7
  )
  raster::plot(rworldmap::countriesLow, add = TRUE)

  raster::plot(map_grey_background_ext,
    col = "grey65", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
    main = paste("Difference in rank of cropland harvested area fraction and CCRI:", crop_names), cex.main = 0.7
  )
  raster::plot(mean_index_raster_diff_disagg,
    col = paldif4, zlim = zr2, xaxt = "n", yaxt = "n",
    axes = FALSE, box = FALSE, add = TRUE
  )
}


# CCRI functions ----------------------------------------------------------
#' Calculate Cropland Connectivity Risk Index (CCRI)
#'  This function calculates CCRI for given parameters using power law and negative exponential.
#'  It's required to call [initialize_cropland_data()] before calling this function.
#' It returns a list of CCRI values.
#' @param link_threshold A threshold value for link
#' @param power_law_metrics A list of power law metrics
#' @param negative_exponential_metrics A list of negative exponential metrics
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param thresholded_crop_values A list of crop values above threshold
#' @return A list of calculated CCRI values
#' @export
calculate_ccri <- function(
    link_threshold = 0,
    power_law_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
    negative_exponential_metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
    crop_cells_above_threshold, thresholded_crop_values) {

  # TODO: parallelize them
  if (!.valid_vector_input(power_law_metrics)) {
    stop("Input 'power_law_metrics' must be a non-empty vector of metric names for inverse power law.")
  }
  if (!.valid_vector_input(negative_exponential_metrics)) {
    stop("Input 'neative_exponential_metrics' must be a non-empty vector of metric names for negative power law.")
  }
  opted_powerlaw_metrics <- check_metrics(power_law_metrics)
  ccri_powerlaw(the$parameters_config$`CCRI parameters`$DispersalParameterBeta, link_threshold,
                betweenness_metric = opted_powerlaw_metrics$betweeness,
                node_strength = opted_powerlaw_metrics$node_strength,
                sum_of_nearest_neighbors = opted_powerlaw_metrics$sum_of_nearest_neighbors,
                eigenvector_centrality = opted_powerlaw_metrics$eigenvector_centrality,
                crop_cells_above_threshold = crop_cells_above_threshold,
                thresholded_crop_values = thresholded_crop_values)

  opted_negative_exp_metrics <- check_metrics(negative_exponential_metrics)
  ccri_negative_exponential(the$parameters_config$`CCRI parameters`$DispersalParameterGamma,
                            link_threshold,
                            betweenness_metric = opted_negative_exp_metrics$betweeness,
                            node_strength = opted_negative_exp_metrics$node_strength,
                            sum_of_nearest_neighbors = opted_negative_exp_metrics$sum_of_nearest_neighbors,
                            eigenvector_centrality = opted_negative_exp_metrics$eigenvector_centrality,
                            crop_cells_above_threshold = crop_cells_above_threshold,
                            thresholded_crop_values = thresholded_crop_values)
}

#' Calculate CCRI using powerlaw for given parameters
#' This function calculates CCRI using powerlaw for given parameters based on provided metrics and parameters.
#' @param dispersal_parameter_beta A list of beta values
#' @param link_threshold A threshold value for link
#' @param distance_matrix distance matrix, generated during initialize_crop_data()
#' @param thresholded_crop_values crop values above threshold
#' @param crop_raster A raster object for cropland harvest
#' @param crop_cells_above_threshold crop cells above threshold. Only contains cells and not the the values.
#' @param betweenness_metric A boolean value to calculate betweenness metric
#' @param node_strength A boolean value to calculate node strength
#' @param sum_of_nearest_neighbors A boolean value to calculate sum of nearest neighbors
#' @param eigenvector_centrality A boolean value to calculate eigenvector centrality
#' @return A list of calculated CCRI values using powerlaw
#' @export
ccri_powerlaw_function <- function(dispersal_parameter_beta, link_threshold, distance_matrix = the$distance_matrix,
                                   thresholded_crop_values, crop_raster, crop_cells_above_threshold,
                                   betweenness_metric = FALSE, node_strength = FALSE,
                                   sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE) {
  ##############################################
  #### create adjacency matrix

  distancematr <- distance_matrix # pairwise distance matrix
  #---- end of code
  # use function C=AX^(-beta), here A=1, X=distancematr
  distancematrexp <- distancematr^(-dispersal_parameter_beta)
  cropmatr <- thresholded_crop_values # complete gravity model with crop data
  cropmatr1 <- matrix(cropmatr, , 1)
  cropmatr2 <- matrix(cropmatr, 1, )

  cropmatrix <- cropmatr1 %*% cropmatr2
  cropmatrix <- as.matrix(cropmatrix)
  # adjacecy matrix
  cropdistancematr <- distancematrexp * cropmatrix
  # adjacency matrix after threshold
  logicalmatr <- cropdistancematr > link_threshold
  stan <- cropdistancematr * logicalmatr
  # use round() because betweenness() may have problem when do the calculation
  stan <- round(stan, 6)
  # create adjacency matrix
  cropdistancematrix <- igraph::graph.adjacency(stan,
    mode = c("undirected"),
    diag = FALSE, weighted = TRUE
  )

  ##############################################
  #### CCRI is a weighted mean of 4 network metric
  ####
  metric_weights <- calculate_metrics_weight(
    betweenness_metric, node_strength,
    sum_of_nearest_neighbors, eigenvector_centrality
  )
  index <- NULL

  ##############################################
  ## sum of nearest neighbors degree

  if (sum_of_nearest_neighbors) {
    knnpref0 <- igraph::graph.knn(cropdistancematrix, weights = NA)$knn
    knnpref0[is.na(knnpref0)] <- 0
    degreematr <- igraph::degree(cropdistancematrix)
    knnpref <- knnpref0 * degreematr
    if (max(knnpref) == 0) {
      knnprefp <- 0
    } else if (max(knnpref) > 0) {
      knnprefp <-
        knnpref / max(knnpref) / metric_weights[[STR_NEAREST_NEIGHBORS_SUM]]
    }

    index <- ifelse(is.null(index), knnprefp, index + knnprefp)
  }

  ##############################################
  #### node degree, node strengh
  ####
  if (node_strength) {
    nodestrength <- igraph::graph.strength(cropdistancematrix)
    nodestrength[is.na(nodestrength)] <- 0
    if (max(nodestrength) == 0) {
      nodestr <- 0
    } else if (max(nodestrength) > 0) {
      nodestr <-
        nodestrength / max(nodestrength) / metric_weights[[STR_NODE_STRENGTH]]
    }

    index <- ifelse(is.null(index), nodestr, index + nodestr)
  }
  ##############################################
  #### betweenness centrality
  ####
  # weight method 0:
  # between<-betweenness(cropdistancematrix, weights = 1/E(cropdistancematrix)$weight)
  # weight method 1:
  #   between<-betweenness(cropdistancematrix, weights = -log(E(cropdistancematrix)$weight))
  # weight method 2:
  if (betweenness_metric) {
    between <- igraph::betweenness(cropdistancematrix,
      weights =
        (1 - 1 / exp(.get_weight_vector(
          cropdistancematrix
        )))
    )

    between[is.na(between)] <- 0
    if (max(between) == 0) {
      betweenp <- 0
    } else if (max(between) > 0) {
      betweenp <- between / max(between) / metric_weights[[STR_BETWEENNESS]]
    }

    index <- ifelse(is.null(index), betweenp, index + betweenp)
  }

  ##############################################
  #### eigenvector and eigenvalues
  ####
  if (eigenvector_centrality) {
    eigenvectorvalues <- igraph::evcent(cropdistancematrix)
    ev <- eigenvectorvalues$vector
    ev[is.na(ev)] <- 0
    if (max(ev) == 0) {
      evp <- 0
    } else if (max(ev) != 0) {
      evp <- ev / max(ev) / metric_weights[[STR_EIGEN_VECTOR_CENTRALITY]]
    }

    index <- ifelse(is.null(index), evp, index + evp)
  }

  indexpre <- crop_raster
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- index
  indexv <- indexpre
  return(indexv)
}

#' Calculate CCRI using negative exponential for given parameters
#' This function calculates CCRI using negative exponential
#' for given parameters based on provided metrics and parameters.
#' @param dispersal_parameter_gamma_val A list of gamma values
#' @param link_threshold A threshold value for link
#' @param distance_matrix distance matrix calculated during initialize_crop_data().
#' @param thresholded_crop_values crop values above threshold
#' @param crop_raster A raster object for crop raster
#' @param crop_cells_above_threshold A list of crop cells above threshold
#' @param betweenness_metric A boolean value to calculate betweenness metric
#' @param node_strength A boolean value to calculate node strength
#' @param sum_of_nearest_neighbors A boolean value to calculate sum of nearest neighbors
#' @param eigenvector_centrality A boolean value to calculate eigenvector centrality
#' @return A list of calculated CCRI values using negative exponential
#' @export
ccri_neg_exponential_function <- function(dispersal_parameter_gamma_val, link_threshold,
                                          distance_matrix = the$distance_matrix, thresholded_crop_values,
                                          crop_raster, crop_cells_above_threshold, betweenness_metric = FALSE,
                                          node_strength = FALSE, sum_of_nearest_neighbors = FALSE,
                                          eigenvector_centrality = FALSE) {
  ##############################################
  #### create adjacency matrix
  ####
  distancematr <- distance_matrix
  #---- end of code

  eulernumber <- exp(1)
  # exponential model
  distancematrexponential <-
    eulernumber^(-dispersal_parameter_gamma_val * distancematr)
  cropmatr <- thresholded_crop_values # complete gravity model with crop data
  cropmatr1 <- matrix(cropmatr, , 1) # complete gravity model with crop data
  cropmatr2 <- matrix(cropmatr, 1, )
  cropmatrix <- cropmatr1 %*% cropmatr2
  cropmatrix <- as.matrix(cropmatrix)
  cropdistancematr <- distancematrexponential * cropmatrix
  logicalmatr <- cropdistancematr > link_threshold
  stan <- cropdistancematr * logicalmatr
  # use round() because betweenness() may have problem when do the calculation
  stan <- round(stan, 6)
  # create adjacency matrix
  cropdistancematrix <- igraph::graph.adjacency(stan,
    mode = c("undirected"),
    diag = FALSE, weighted = TRUE
  )
  ##############################################
  #### create network for all the selected nodes
  ####
  # V(cropdistancematrix)$color=colororder
  igraph::V(cropdistancematrix)$label.cex <- 0.7
  igraph::E(cropdistancematrix)$weight * 4000
  igraph::E(cropdistancematrix)$color <- "red"

  metric_weights <- calculate_metrics_weight(
    betweenness_metric, node_strength,
    sum_of_nearest_neighbors,
    eigenvector_centrality
  )
  index <- NULL

  if (sum_of_nearest_neighbors) {
    knnpref0 <- igraph::graph.knn(cropdistancematrix, weights = NA)$knn
    knnpref0[is.na(knnpref0)] <- 0
    degreematr <- igraph::degree(cropdistancematrix)
    knnpref <- knnpref0 * degreematr
    if (max(knnpref) == 0) {
      knnprefp <- 0
    } else if (max(knnpref) > 0) {
      knnprefp <- knnpref / max(knnpref) / metric_weights[[STR_NEAREST_NEIGHBORS_SUM]]
    }

    index <- ifelse(is.null(index), knnprefp, index + knnprefp)
  }

  ##############################################
  #### node degree, node strength
  ####
  if (node_strength) {
    nodestrength <- igraph::graph.strength(cropdistancematrix)
    nodestrength[is.na(nodestrength)] <- 0
    if (max(nodestrength) == 0) {
      nodestr <- 0
    } else if (max(nodestrength) > 0) {
      nodestr <- nodestrength / max(nodestrength) / metric_weights[[STR_NODE_STRENGTH]]
    }

    index <- ifelse(is.null(index), nodestr, index + nodestr)
  }

  ##############################################
  #### betweenness centrality
  ####
  # weight method 0
  # between<-betweenness(cropdistancematrix,
  # weights = 1/E(cropdistancematrix)$weight)
  # weight method 1:
  #   between<-betweenness(cropdistancematrix,
  #   weights = -log(E(cropdistancematrix)$weight))
  # weight method 2:
  if (betweenness_metric) {
    between <- igraph::betweenness(
      cropdistancematrix,
      weights = (1 - 1 / exp(.get_weight_vector(cropdistancematrix)))
    )
    between[is.na(between)] <- 0
    if (max(between) == 0) {
      betweenp <- 0
    } else if (max(between) > 0) {
      betweenp <- between / max(between) / metric_weights[[STR_BETWEENNESS]]
    }

    index <- ifelse(is.null(index), betweenp, index + betweenp)
  }

  ##############################################
  #### eigenvector and eigenvalues
  ####
  if (eigenvector_centrality) {
    eigenvectorvalues <- igraph::evcent(cropdistancematrix)
    ev <- eigenvectorvalues$vector
    ev[is.na(ev)] <- 0
    if (max(ev) == 0) {
      evp <- 0
    } else if (max(ev) != 0) {
      evp <- ev / max(ev) / metric_weights[[STR_EIGEN_VECTOR_CENTRALITY]]
    }

    index <- ifelse(is.null(index), evp, index + evp)
  }

  ##############################################
  #### plot index layer
  ####

  indexpre <- crop_raster
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- index
  indexv <- indexpre
  return(indexv)
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
      "Host density threshold: ", host_density_threshold)

  geo_areaext <- raster::extent(as.numeric(unlist(geo_scale))) # list
  the$result_index_list <- list()

  for (agg_method in aggregate_methods) {
    cropland_density_info <- initialize_cropland_data(cropharvest_raster, resolution, geo_areaext,
                             host_density_threshold = host_density_threshold, agg_method)

    calculate_ccri(link_threshold,
                   power_law_metrics =
                     the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
                   negative_exponential_metrics =
                     the$parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
                  crop_cells_above_threshold =
                    cropland_density_info$crop_values_at,
                  thresholded_crop_values =
                    cropland_density_info$crop_value)
  }

  stacked_rasters <- raster::stack(the$result_index_list)
  mean_index_raster <- raster::calc(stacked_rasters, sum) / length(the$result_index_list)

  mean_index_raster_diff <- mean_index_raster
  variance_mean_index_raster <- mean_index_raster

  mean_index_raster_val <- raster::getValues(mean_index_raster)
  zeroid <- which(mean_index_raster_val == 0)
  mean_index_raster[zeroid] <- NaN

  terra::plot(mean_index_raster,
    col = .get_palette(), zlim = c(0, 1),
    main = paste("Mean cropland connectivity risk index from sensitivity analysis: ",
                 the$parameters_config$`CCRI parameters`$Crops, "resolution = ",
                 the$parameters_config$`CCRI parameters`$Resolution), cex.main = 0.7)
  raster::plot(rworldmap::countriesLow, add = TRUE)

  zero_raster_results <- calculate_zero_raster(geo_areaext, mean_index_raster, resolution = resolution)
  ccri_variance(
    lapply(the$result_index_list, raster::getValues),
    variance_mean_index_raster, zero_raster_results$zero_raster_extent,
    zero_raster_results$map_grey_background_extent)

  calculate_difference_map(
    mean_index_raster_diff, the$cropharvest_aggtm_crop, the$cropharvest_agglm_crop,
    zero_raster_results$zero_raster_extent, zero_raster_results$map_grey_background_extent)

  the$is_initialized <- FALSE
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

  # cuttoff adjacencey matrix
  cropland_thresholds <- the$parameters_config$`CCRI parameters`$HostDensityThreshold

  # crop data
  cropharvest <- get_cropharvest_raster_sum(the$parameters_config$`CCRI parameters`$Hosts) # list
  agg_methods <- the$parameters_config$`CCRI parameters`$AggregationStrategy # list

  # maps
  geo_scales <- get_geographic_scales()

  # resolution
  resolution <- the$parameters_config$`CCRI parameters`$Resolution

  lapply(geo_scales, sensitivity_analysis_on_cropland_threshold,
    link_thresholds = the$parameters_config$`CCRI parameters`$LinkThreshold,
    host_density_thresholds = cropland_thresholds, aggregate_methods = agg_methods,
    cropharvest_raster = cropharvest, resolution = resolution)
}
