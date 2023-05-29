#' @exportPattern ^[^\\.].*

# Setting color palettes--------------------------------------------------------

palette1 <- c(
  "#F4E156FF", "#F6D746FF", "#F8CD37FF", "#FAC329FF", "#FBB91EFF",
  "#FCAF13FF", "#FCA50BFF", "#FB9C06FF", "#FA9207FF", "#F8890CFF",
  "#F68013FF", "#F37819FF", "#F06F20FF", "#EC6727FF", "#E85F2EFF",
  "#E25834FF", "#DD5139FF", "#D74B3FFF", "#D04545FF", "#CA404AFF",
  "#C33B4FFF", "#BC3754FF", "#B43359FF", "#AC305EFF", "#A42C60FF",
  "#9B2964FF", "#932667FF", "#922568FF", "#902568FF", "#8F2469FF",
  "#8D2369FF", "#8C2369FF", "#8A226AFF", "#88226AFF", "#87216BFF",
  "#85216BFF", "#84206BFF", "#82206CFF", "#801F6CFF", "#7F1E6CFF",
  "#7D1E6DFF", "#7C1D6DFF", "#7A1D6DFF", "#781C6DFF", "#771C6DFF",
  "#751B6EFF", "#741A6EFF", "#721A6EFF", "#71196EFF", "#6E196EFF",
  "#6D186EFF", "#6B186EFF", "#6A176EFF", "#68166EFF", "#66166EFF",
  "#65156EFF", "#63156EFF", "#61136EFF", "#60136EFF", "#5E126EFF",
  "#5C126EFF", "#5B126EFF", "#59106EFF", "#58106EFF", "#560F6DFF",
  "#540F6DFF", "#530E6DFF", "#510E6CFF", "#500D6CFF", "#4D0D6CFF",
  "#4C0C6BFF", "#4A0C6BFF", "#490B6AFF", "#470B6AFF", "#450A69FF",
  "#440A68FF", "#420A68FF", "#400A67FF", "#3E0966FF", "#3D0965FF",
  "#3B0964FF", "#390963FF", "#380962FF", "#360961FF", "#340A5FFF",
  "#320A5EFF", "#310A5CFF", "#2F0A5BFF", "#2D0B59FF", "#2B0B57FF",
  "#290B55FF", "#280B53FF", "#250C51FF", "#240C4EFF", "#230C4BFF",
  "#200C49FF", "#1F0C47FF", "#1D0C44FF", "#1C0C42FF", "#1A0C40FF",
  "#190C3DFF", "#170C3BFF", "#150B38FF", "#150B36FF", "#130A33FF",
  "#110A31FF", "#11092EFF", "#0F092CFF", "#0D082AFF", "#0C0827FF",
  "#0B0725FF", "#0A0723FF", "#090620FF", "#08051EFF", "#07051CFF",
  "#060419FF", "#050418FF", "#040315FF", "#040312FF", "#030210FF",
  "#02020EFF", "#02020CFF", "#02010AFF", "#010108FF", "#010106FF",
  "#010005FF", "#000004FF", "#000004FF", "#000004FF"
)

# palette for different map
paldif <- colorspace::diverge_hcl(12, h = c(128, 330), c = 98, l = c(65, 90))

# Utility functions -------------------------------------------------------
# Calculate crop harvest raster -------------------------------------------

get_cropharvest_raster <- function(crop_name) {
  cropharvest <- geodata::crop_monfreda(
    crop = crop_name, path = tempdir(),
    var = "area_f"
  )
  cropharvest <- raster::raster(terra::sources(cropharvest))
  return(cropharvest)
}

get_cropharvest_raster_sum <- function(crop_names) {
  if (!is.vector(crop_names) || length(crop_names) == 0) {
    stop("Input 'crop_names' must be a non-empty vector of crop names.")
  }

  # sum of rasters
  cropharvests <- lapply(crop_names, get_cropharvest_raster)
  Reduce("+", cropharvests)
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
  crop_cells_above_threshold <- which(crop_values > host_density_threshold) # find the cells with value > 0.0001
  thresholded_crop_values <- crop_values[crop_cells_above_threshold]
  return(c(.extract_lon_lat(crop_cells_above_threshold, cropharvest_agg_crop),
           list(crop_value = thresholded_crop_values, crop_values_at = crop_cells_above_threshold)))
}

# Initialize --------------------------------------------------
is_initialized <<- FALSE
parameters_config <<- NULL
result_index_list <<- list()
distance_matrix <<- NULL
cropharvest_aggtm <<- NULL
cropharvest_agglm_crop <<- NULL
cropharvest_aggtm_crop <<- NULL

# Global cropland density map---------------------------------------------------------------
# Only when user has enabled global analysis
global_analysis <- function(map_grey_background_extent, resolution =
                              parameters_config$`CCRI parameters`$Resolution) {
  # ```{r, fig.width=20, fig.height=10, dpi=400}

  cropharvest_aggtm_crop1 <- raster::crop(cropharvest_aggtm, raster::extent(-180, 180, -60, 80))
  zr_world_mean <- range(0.1, max(raster::getValues(cropharvest_aggtm_crop1)))

  # Removing pixels outside boundary
  mean_index_raster_val <- raster::getValues(cropharvest_aggtm_crop1)
  # structure(mean_index_raster_val)

  zeroid <- which(mean_index_raster_val == 0)
  cropharvest_aggtm[zeroid] <- NaN

  zero_raster <- raster::raster(.get_helper_filepath(.kzeroraster_file_type))
  cam_zero <- raster::crop(zero_raster, raster::extent(-180, 180, -60, 80))
  mean_index_raster <- raster::disaggregate(cropharvest_aggtm_crop1, fact = c(resolution, resolution), method = "")
  mean_index_raster_cam <- mean_index_raster + cam_zero

  # Plotting cropland density
  map_grey_background <- raster::raster(.get_helper_filepath(.kmapgreybackground_file_type))

  map_grey_background_cam <- raster::crop(map_grey_background, raster::extent(-180, 180, -60, 80))
  crop_names <- .get_cs_host_names(parameters_config)
  plot(map_grey_background_cam,
    col = "grey75", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
    main = paste("Mean in crop area fraction:", crop_names), cex.main = 1.6
  )

  plot(mean_index_raster_cam,
    main = paste("Crop area density: ", crop_names),
    col = palette1, zlim = zr_world_mean, xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, add = TRUE
  )
}

initialize_cropland_data <- function(cropharvest_raster, resolution, geo_scale, host_density_threshold, agg_method) {

  #----------- aggregration -----------------------------
  cropharvest_agg <- raster::aggregate(cropharvest_raster, fact = resolution, fun = agg_method,
                                       na.action = stats::na.omit)
  density_data <- NULL
  if (agg_method == "sum") {
    cropharvest_aggtm <- cropharvest_agg / resolution / resolution # TOTAL MEAN
    raster::plot(cropharvest_aggtm, col = palette1) # map of cropland density
    #----------- crop cropland area for the given extent ----------
    cropharvest_aggtm_crop <<- raster::crop(cropharvest_aggtm, geo_scale)
    raster::plot(cropharvest_aggtm_crop, col = palette1) # TODO: don't show this
    density_data <- .extract_cropland_density(cropharvest_aggtm_crop, host_density_threshold)
  } else if (agg_method == "mean") {
    cropharvest_agglm <- cropharvest_agg
    raster::plot(cropharvest_agglm, col = palette1)
    #----------- crop cropland area for the given extent ----------
    cropharvest_agglm_crop <<- raster::crop(cropharvest_agglm, geo_scale)
    raster::plot(cropharvest_agglm_crop, col = palette1)
    density_data <- .extract_cropland_density(cropharvest_agglm_crop, host_density_threshold)
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

  distance_matrix <<- temp_matrix

  is_initialized <<- TRUE
  return(density_data)
}


validate_index_cal <- function(vals_list) {
  ready <- TRUE
  if (!is_initialized) {
    stop("Not initialized. Call initializeCroplandData()")
  }
  if (!is.list(vals_list)) {
    warning("argument is not a list")
    ready <- FALSE
  }
  return(ready)
}


# Aggregate -----------------------------------------------------

# inverse power law -------------------------------------------------------
ccri_powerlaw <- function(dispersal_parameter_beta_vals, link_threshold = 0, betweenness_metric = FALSE,
                          node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE,
                          crop_cells_above_threshold = NULL, thresholded_crop_values = NULL) {
  if (!validate_index_cal(dispersal_parameter_beta_vals)) {
    return(0)
  }

  index_list <- lapply(dispersal_parameter_beta_vals, ccri_powerlaw_function, link_threshold = link_threshold,
                       distance_matrix, thresholded_crop_values, cropharvest_aggtm_crop, crop_cells_above_threshold,
                       betweenness_metric = betweenness_metric, node_strength = node_strength,
                       sum_of_nearest_neighbors = sum_of_nearest_neighbors,
                       eigenvector_centrality = eigenvector_centrality
  )

  result_index_list <<- c(result_index_list, index_list)
  return(1)
}

# negative exponential function -------------------------------------------
ccri_negative_exponential <- function(dispersal_parameter_gamma_vals, link_threshold = 0, betweenness_metric = FALSE,
                                      node_strength = FALSE, sum_of_nearest_neighbors = FALSE,
                                      eigenvector_centrality = FALSE, crop_cells_above_threshold = NULL,
                                      thresholded_crop_values = NULL) {

  if (!validate_index_cal(dispersal_parameter_gamma_vals)) {
    return(0)
  }

  index_list <- lapply(dispersal_parameter_gamma_vals,
    ccri_neg_exponential_function, link_threshold = link_threshold, distance_matrix, thresholded_crop_values,
    cropharvest_aggtm_crop, crop_cells_above_threshold, betweenness_metric, node_strength,
    sum_of_nearest_neighbors, eigenvector_centrality
  )

  result_index_list <<- c(result_index_list, index_list)
  return(1)
}


# Utility functions -------------------------------------------------------

.get_weight_vector <- function(cropdistancematrix) {
  weight_vec <- igraph::E(cropdistancematrix)$weight
  weight_vec[is.na(weight_vec)] <- 0
  weight_vec <- weight_vec + 1e-10
  return(weight_vec)
}

get_geographic_scales <- function() {
  perform_global_analysis <- parameters_config$`CCRI parameters`$Longitude_Latitude$Global
  geo_scales <- list()
  if (perform_global_analysis) {
    geo_scales <- list(
      parameters_config$`CCRI parameters`$Longitude_Latitude$EastExt,
      parameters_config$`CCRI parameters`$Longitude_Latitude$WestExt
    )
  }
  custom_scales <- parameters_config$`CCRI parameters`$Longitude_Latitude$CustomExt
  if (!(is.null(custom_scales) || is.na(custom_scales) || length(custom_scales) == 0)) {
    geo_scales <- c(geo_scales, lapply(custom_scales, as.numeric))
  }

  return(geo_scales)
}


# method to calculate zero raster -----------------------------------------

calculate_zero_raster <- function(geoscale, mean_index_raster,
                                  resolution = parameters_config$`CCRI parameters`$Resolution) {
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  # TODO: is there any other way to get 0 raster?
  zero_raster <- raster::raster(.get_helper_filepath(.kzeroraster_file_type))
  ext_zero <- raster::crop(zero_raster, geoscale)
  mean_index_raster <- raster::disaggregate(mean_index_raster, fact = c(resolution, resolution), method = "")
  mean_index_raster_ext <- mean_index_raster + ext_zero
  # TODO: remove this plot..use the one below with col = grey75
  raster::plot(mean_index_raster_ext,
    col = palette1, zlim = c(0.000000000000, 1), xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, main = paste(
      "Mean cropland connectivity risk index from sensitivity analysis:",
      paste(parameters_config$`CCRI parameters`$Hosts, collapse = ",")
    ),
    cex.main = 0.7
  )
  raster::plot(rworldmap::countriesLow, add = TRUE)

  #------------------------------------------------------------
  map_grey_background <- raster::raster(.get_helper_filepath(.kmapgreybackground_file_type))

  # Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
  map_grey_background_ext <- raster::crop(map_grey_background, geoscale)
  raster::plot(map_grey_background_ext,
    col = "grey75", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
    main = paste(
      "Mean cropland connectivity risk index from sensitivity analysis:",
      paste(parameters_config$`CCRI parameters`$Hosts, collapse = ",")
    ), cex.main = 0.7
  )
  raster::plot(mean_index_raster_ext,
    col = palette1, zlim = c(0.000000000000, 1), xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, add = TRUE
  )

  raster::plot(rworldmap::countriesLow, add = TRUE, border = "white")

  return(c(zero_raster_extent = ext_zero, map_grey_background_extent = map_grey_background_ext, use.names = TRUE))
}

# Complete sensitivity analysis of Variance of CCRI -------------
ccri_variance <- function(indexes, variance_mean_index_raster, zero_extent_raster, map_grey_background_ext,
                          resolution = parameters_config$`CCRI parameters`$Resolution) {
  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  variance_mean_index_ext <- apply(do.call(cbind, indexes), 1, stats::var)


  variance_mean_index_raster[] <- variance_mean_index_ext
  z_var_w <- range(variance_mean_index_ext[which(variance_mean_index_ext > 0)])
  raster::plot(variance_mean_index_raster,
    col = palette1, zlim = z_var_w, xaxt = "n",
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
      paste(parameters_config$`CCRI parameters`$Hosts, collapse = ",")
    ), cex.main = 0.7
  )
  raster::plot(variance_mean_index_raster_ext_disagg,
    col = palette1, zlim = z_var_w, xaxt = "n",
    yaxt = "n", axes = FALSE, box = FALSE, add = TRUE
  )
  raster::plot(rworldmap::countriesLow, add = TRUE)
}

# difference map ----------------------------------------------------------

calculate_difference_map <- function(mean_index_raster_diff, cropharvest_aggtm_crop, cropharvest_agglm_crop,
                                     zero_extent_raster, map_grey_background_ext,
                                     resolution = parameters_config$`CCRI parameters`$Resolution) {
  # difference map
  if (missing(cropharvest_aggtm_crop) || missing(cropharvest_agglm_crop)) {
    message("Either sum or mean aggregate is missing. Aborting diffrence calculation")
    return(NULL)
  }
  if (is.null(cropharvest_aggtm_crop) || is.null(cropharvest_agglm_crop)) {
    message("Either sum or mean aggregate is missing. Aborting diffrence calculation")
    return(NULL)
  }

  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  paldif4 <- colorspace::diverge_hcl(51, c = 100, l = c(20, 90), power = 1.3)

  #-----------------------------------------------------
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
  raster::plot(mean_index_raster_diff, main = paste(
    "Difference in rank of cropland harvested area fraction and CCRI:",
    paste(parameters_config$`CCRI parameters`$Hosts, collapse = ",")
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

  crop_names <- .get_cs_host_names(parameters_config)
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

calculate_ccri <- function(
    link_threshold = 0,
    power_law_metrics = parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
    negative_exponential_metrics = parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
    crop_cells_above_threshold, thresholded_crop_values) {

  # TODO: parallelize them
  if (!.valid_vector_input(power_law_metrics)) {
    stop("Input 'power_law_metrics' must be a non-empty vector of metric names for inverse power law.")
  }
  if (!.valid_vector_input(negative_exponential_metrics)) {
    stop("Input 'neative_exponential_metrics' must be a non-empty vector of metric names for negative power law.")
  }
  opted_powerlaw_metrics <- check_metrics(power_law_metrics)
  ccri_powerlaw(parameters_config$`CCRI parameters`$DispersalParameterBeta, link_threshold,
    betweenness_metric = opted_powerlaw_metrics$betweeness,
    node_strength = opted_powerlaw_metrics$node_strength,
    sum_of_nearest_neighbors = opted_powerlaw_metrics$sum_of_nearest_neighbors,
    eigenvector_centrality = opted_powerlaw_metrics$eigenvector_centrality,
    crop_cells_above_threshold = crop_cells_above_threshold, thresholded_crop_values = thresholded_crop_values)

  opted_negative_exp_metrics <- check_metrics(negative_exponential_metrics)
  ccri_negative_exponential(parameters_config$`CCRI parameters`$DispersalParameterGamma,
    link_threshold,
    betweenness_metric =
      opted_negative_exp_metrics$betweeness,
    node_strength =
      opted_negative_exp_metrics$node_strength,
    sum_of_nearest_neighbors =
      opted_negative_exp_metrics$sum_of_nearest_neighbors,
    eigenvector_centrality =
      opted_negative_exp_metrics$eigenvector_centrality,
    crop_cells_above_threshold = crop_cells_above_threshold, thresholded_crop_values = thresholded_crop_values)
}

ccri_powerlaw_function <- function(dispersal_parameter_beta, link_threshold, distance_matrix,
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

# ```

# CCRI calculated by negative exponential function

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

ccri_neg_exponential_function <- function(dispersal_parameter_gamma_val, link_threshold, distance_matrix,
                                          thresholded_crop_values, crop_raster, crop_cells_above_threshold,
                                          betweenness_metric = FALSE, node_strength = FALSE,
                                          sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE) {
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

sensitivity_analysis_on_geoextent_scale <- function(
    link_threshold = 0, geo_scale, aggregate_methods, cropharvest_raster,
    host_density_threshold, resolution) {

    cat("\nRunning senstivity analysis for the extent: [", geo_scale, "],
        Link threshold: ", link_threshold,
        "Host density threshold: ", host_density_threshold)

  geo_areaext <- raster::extent(as.numeric(unlist(geo_scale))) # list
  result_index_list <<- list()

  for (agg_method in aggregate_methods) {
    cropland_density_info <- initialize_cropland_data(cropharvest_raster, resolution, geo_areaext,
                             host_density_threshold = host_density_threshold, agg_method)

    calculate_ccri(link_threshold,
      power_law_metrics = parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw,
      negative_exponential_metrics = parameters_config$`CCRI parameters`$NetworkMetrics$NegativeExponential,
      crop_cells_above_threshold = cropland_density_info$crop_values_at,
      thresholded_crop_values = cropland_density_info$crop_value)
  }

  stacked_rasters <- raster::stack(result_index_list)
  mean_index_raster <- raster::calc(stacked_rasters, sum) / length(result_index_list)

  mean_index_raster_diff <- mean_index_raster
  variance_mean_index_raster <- mean_index_raster

  mean_index_raster_val <- raster::getValues(mean_index_raster)
  zeroid <- which(mean_index_raster_val == 0)
  mean_index_raster[zeroid] <- NaN

  terra::plot(mean_index_raster,
    col = palette1, zlim = c(0, 1),
    main = paste("Mean cropland connectivity risk index from sensitivity analysis: ",
                 parameters_config$`CCRI parameters`$Crops, "resolution = ",
                 parameters_config$`CCRI parameters`$Resolution), cex.main = 0.7)
  raster::plot(rworldmap::countriesLow, add = TRUE)

  zero_raster_results <- calculate_zero_raster(geo_areaext, mean_index_raster, resolution = resolution)
  ccri_variance(
    lapply(result_index_list, raster::getValues),
    variance_mean_index_raster, zero_raster_results$zero_raster_extent,
    zero_raster_results$map_grey_background_extent)

  calculate_difference_map(
    mean_index_raster_diff, cropharvest_aggtm_crop, cropharvest_agglm_crop,
    zero_raster_results$zero_raster_extent, zero_raster_results$map_grey_background_extent)

  is_initialized <<- FALSE
  return(is_initialized)
}

sensitivity_analysis_on_cropland_threshold <- function(link_thresholds,
                                                       host_density_thresholds,
                                                       geo_scale, aggregate_methods,
                                                       cropharvest_raster,
                                                       resolution) {
  lapply(link_thresholds, sensitivity_analysis_on_link_weight,
    host_density_thresholds = host_density_thresholds,
    geo_scale = geo_scale, aggregate_methods = aggregate_methods,
    cropharvest_raster = cropharvest_raster, resolution = resolution
  )
}

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

senstivity_analysis <- function() {
  parameters_config <<- load_parameters()

  # cuttoff adjacencey matrix
  cropland_thresholds <- parameters_config$`CCRI parameters`$HostDensityThreshold

  # crop data
  cropharvest <- get_cropharvest_raster_sum(as.list(
    parameters_config$`CCRI parameters`$Hosts)) # list
  agg_methods <- parameters_config$`CCRI parameters`$AggregationStrategy # list

  # maps
  geo_scales <- get_geographic_scales()

  # resolution
  resolution <- parameters_config$`CCRI parameters`$Resolution

  lapply(geo_scales, sensitivity_analysis_on_cropland_threshold,
    link_thresholds = parameters_config$`CCRI parameters`$LinkThreshold,
    host_density_thresholds = cropland_thresholds, aggregate_methods = agg_methods,
    cropharvest_raster = cropharvest, resolution = resolution)
}
