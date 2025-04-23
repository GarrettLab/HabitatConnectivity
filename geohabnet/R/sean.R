#' @exportPattern ^[^\\.].*

# Utility functions -------------------------------------------------------

#----------- Extract habitat density data -----------------------
.extract_habitat_density <- function(cropharvest_agg_crop, host_density_threshold) {
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

.crop_rast <- function(agg_method, cropharvest_agg, resolution, geo_scale) {
  postagg_rast <- if (agg_method == "sum") {

    cropharvest_aggtm <- cropharvest_agg / resolution / resolution # TOTAL MEAN
    # crop host area for the given extent
    # TODO: redundant call to terra::crop... remove it
    cropharvest_aggtm_crop <- terra::crop(cropharvest_aggtm, .to_ext(geo_scale))
    cropharvest_aggtm_crop
  } else if (agg_method == "mean") {

    cropharvest_agglm <- cropharvest_agg
    # crop area for the given extent
    cropharvest_agglm_crop <- terra::crop(cropharvest_agglm, .to_ext(geo_scale))
    cropharvest_agglm_crop
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
  density_data <- .extract_habitat_density(temp_rast,
                                           host_density_threshold)

  if (is.null(density_data) || (!is.list(density_data))) {
    stop("unable to extract density data, longitude/latitude")
  }

  # Prepare arguments elements values for the habitat connectivity functions

  # save the latitude and longitude as new matrix
  latilongimatr <- terra::xyFromCell(density_data$agg_crop, cell = density_data$crop_values_at)

  latilongimatr <- as.matrix(latilongimatr)
  temp_matrix <- .cal_dist(latilongimatr, dist_method)

  density_data[[STR_DISTANCE_MATRIX]] <- temp_matrix

  return(density_data)
}

# Utility functions -------------------------------------------------------

.validate_index_cal <- function(vals) {
  ready <- TRUE

  stopifnot("dispersal values missing" = length(vals) > 0)
  if (!is.vector(vals)) {
    warning("argument is not a vector")
    ready <- FALSE
  }
  return(ready)
}

.check_dk <- function(ipl, ne_exp) {
  stopifnot("Need one dispersal parameter to continue analysis" = !is.null(ipl) || !is.null(ne_exp))
  return(TRUE)
}

# Functions to calculate habitat connectivity ----------------------------------
.hci_powerlaw <- function(betas,
                          link_threshold = 0,
                          metrics = NULL,
                          me_weights = NULL,
                          cutoff = -1,
                          rast,
                          crop_cells_above_threshold = NULL,
                          thresholded_crop_values = NULL,
                          distance_matrix = NULL) {

  if (!.validate_index_cal(betas)) {
    return(0)
  }

  .showmsg("Calculating powerlaw models")

  pl_models <- future.apply::future_lapply(betas, .model_powerlaw,
                                           link_threshold = link_threshold,
                                           distance_matrix,
                                           thresholded_crop_values,
                                           adj_mat = NULL,
                                           rast,
                                           crop_cells_above_threshold,
                                           metrics = metrics,
                                           me_weights = me_weights,
                                           cutoff = cutoff,
                                           future.seed = TRUE)

  return(pl_models)
}

.hci_negative_exp <- function(gammas,
                              link_threshold = 0,
                              metrics = NULL,
                              me_weights = NULL,
                              cutoff = -1,
                              rast,
                              crop_cells_above_threshold = NULL,
                              thresholded_crop_values = NULL,
                              distance_matrix = NULL) {

  if (!.validate_index_cal(gammas)) {
    return(0)
  }

  .showmsg("Calculating powerlaw models")

  ne_models <- future.apply::future_lapply(gammas,
                                           .model_neg_exp,
                                           link_threshold = link_threshold,
                                           distance_matrix,
                                           thresholded_crop_values,
                                           adj_mat = NULL,
                                           rast,
                                           crop_cells_above_threshold,
                                           metrics = metrics,
                                           me_weights = me_weights,
                                           cutoff = cutoff,
                                           future.seed = TRUE)

  return(ne_models)
}

.valid_dkparams <- function(mod, param) {
  # Substitute the field name into the expression
  expr <-
    !is.null(mod) &&
    !is.null(mod[[param]]) &&
    length(mod[[param]]) > 0

  # Evaluate the expression and return the result
  eval(expr)
}

.hci <- function(
    link_threshold = 0,
    ipl = NULL,
    ne_exp = NULL,
    rast,
    crop_cells_above_threshold,
    thresholded_crop_values,
    distance_matrix = NULL) {

  packed_sp <- terra::wrap(rast)

  .check_dk(ipl, ne_exp)

  future_pl_ret <- if (.valid_dkparams(ipl, "beta")) {
    future::future({
      .hci_powerlaw(
        ipl$beta,
        link_threshold,
        metrics = ipl$metrics,
        me_weights = ipl$weights,
        cutoff = ipl$cutoff,
        packed_sp,
        crop_cells_above_threshold = crop_cells_above_threshold,
        thresholded_crop_values = thresholded_crop_values,
        distance_matrix
      )
    }, seed = TRUE)
  }

  future_ne_ret <- if (.valid_dkparams(ne_exp, "gamma")) {
    future::future({
      .hci_negative_exp(
        ne_exp$gamma,
        link_threshold,
        metrics = ne_exp$metrics,
        me_weights = ne_exp$weights,
        cutoff = ne_exp$cutoff,
        packed_sp,
        crop_cells_above_threshold = crop_cells_above_threshold,
        thresholded_crop_values = thresholded_crop_values,
        distance_matrix
      )
    }, seed = TRUE)
  }

  stopifnot("Need one dispersal kernel parameter to continue analysis" =
              !is.null(future_pl_ret) || !is.null(future_ne_ret))

  # Combine the results after both functions have finished
  results <- if (!is.null(future_pl_ret) && !is.null(future_ne_ret)) {
    c(future::value(future_pl_ret), future::value(future_ne_ret))
  } else if (!is.null(future_pl_ret)) {
    future::value(future_pl_ret)
  } else if (!is.null(future_ne_ret)) {
    future::value(future_ne_ret)
  } else {
    stop("No results returned from dispersal kernel calculations")
  }

  return(results)
}


# Sensitivity analysis --------------------------------------------------------

#' Sensitivity analysis across maps of habitat connectivity
#'
#' @description
#' This function performs a sensitivity analysis across different values of habitat connectivity
#' for each location in a map.
#' For each combination of selected parameters, an index of habitat connectivity is calculated.
#' [sensitivity_analysis()] is a wrapper around [sean()] function.
#' - `msean()` is a wrapper around [sean()] function.
#' It has additional argument to specify maps which are calculated
#' using [connectivity()] function.
#' @param ... arguments passed to [sean()]
#' @param link_threshold Numeric. A threshold value for link weight.
#' All link weights that are below this threshold will be replaced with zero for the connectivity analysis.
#' Link weights represent the relative likelihood of pathogen, pest,
#' or invasive species movement between a pair of host locations,
#' which is calculated using gravity models based on host density (or availability) and dispersal kernels.
#' @param hd_threshold Numeric. A threshold value for habitat availability (e.g., cropland density or host density).
#' All locations with a host density below the selected threshold will be excluded from the connectivity analysis,
#' which focuses the analysis on the most important locations.
#' The values for the habitat availability threshold can range between 0 and 1;
#' if value is 1, all locations will be excluded from the analysis and 0 will include all locations in the analysis.
#' Selecting a threshold for, for example, host density requires at least knowing what is the maximum value
#' in the host density map to avoid excluding all locations in the analysis.
#' if value is 1, all locations will be excluded from the analysis and 0 will include all locations in the analysis.
#' Selecting a threshold for host density requires at least knowing what is the maximum value
#' in the host density map to avoid excluding all locations in the analysis.
#' @param agg_methods Character. One or both methods of spatial aggregation - SUM, MEAN.
#' Aggregation strategy for scaling the input raster to the desired resolution.
#' @param dist_method Character. The method to calculate the distance matrix.
#' @param res Numeric. The spatial aggregation factor that will be used to aggregate the raster layers of habitat availability from fine to coarse resolution. Default is [reso()] or 12.
#' @param inv_pl List. A named list of parameters for inverse power law. See details.
#' @param neg_exp List. A named list of parameters for inverse negative exponential. See details.
#' All locations with a host density below the selected threshold will be excluded from the connectivity analysis,
#' which focuses the analysis on the most important locations.
#' The values for the host density threshold can range between 0 and 1;
#' @inheritParams sa_onrasters
#' @return GeoRasters.
#' @export
#' @details
#' When `global = TRUE`, `geoscale` is ignored and [global_scales()] is used by default.
#'
#' The functions [sean()] and [msean()] perform the same sensitivity analysis, but they differ in their return value.
#' The return value of [msean()] is `GeoNetwork`,
#' which contains the result from applying the [connectivity()] function on the habitat connectivity indexes.
#' Essentially, the risk maps.
#'
#' If neither the inverse power law nor the negative exponential dispersal kernel is specified,
#' the function will return an error.
#'
#' In [msean()], three spatRasters are produced with the following values.
#' For each location in the area of interest,
#' the mean in habitat connectivity across selected parameters is calculated.
#' For each location in the area of interest,
#' the variance in habitat connectivity across selected parameters is calculated.
#' For each location in the area of interest,
#' the difference between the rank of habitat connectivity and the rank of host density is calculated.
#' By default, each of these spatRasters is plotted for visualization.
#' 
#'
#' @inherit Dispersal-kernels details
#'
#' @seealso Uses [connectivity()]
#' @seealso Uses [msean()] [inv_powerlaw()] [neg_expo()]
#' @inherit sensitivity_analysis references
sean <- function(rast,
                 global = TRUE,
                 geoscale = NULL,
                 agg_methods = c("sum", "mean"),
                 dist_method = "geodesic",
                 link_threshold = 0,
                 hd_threshold = 0,
                 res = reso(),
                 inv_pl = inv_powerlaw(
                   NULL,
                   betas = c(0.5, 1, 1.5),
                   mets = c(
                     "betweeness",
                     "NODE_STRENGTH",
                     "Sum_of_nearest_neighbors",
                     "eigenVector_centrAlitY"
                   ),
                   we = c(50, 15, 15, 20),
                   linkcutoff = -1
                 ),
                 neg_exp = neg_expo(
                   NULL,
                   gammas = c(0.05, 1, 0.2, 0.3),
                   mets = c(
                     "betweeness",
                     "NODE_STRENGTH",
                     "Sum_of_nearest_neighbors",
                     "eigenVector_centrAlitY"
                   ),
                   we = c(50, 15, 15, 20),
                   linkcutoff = -1
                 )) {

  stopifnot("Need atleast one aggregation method: " = length(agg_methods) >= 1)
  .stopifnot_sprast(rast)

  sean_geo <- function(geoext) {
    .showmsg(paste("\nRunning sensitivity analysis for the extent: [",
                   paste(geoext, collapse = ", "),
                   "],\n",
                   "Link threshold: ",
                   link_threshold,
                   "\n",
                   "Host density threshold: ",
                   hd_threshold,
                   "\n"))

    model_ret <- list()
    host_densityrasts <- list()
    up_rast <- .unpack_rast_ifnot(rast)

    for (agg_method in agg_methods) {
      density_data <- .init_cd(up_rast,
                               res,
                               geoext,
                               host_density_threshold = hd_threshold,
                               agg_method,
                               dist_method)

      ret <- .hci(link_threshold,
                          ipl = inv_pl,
                          ne_exp = neg_exp,
                          rast = density_data$agg_crop,
                          crop_cells_above_threshold = density_data$crop_values_at,
                          thresholded_crop_values = density_data$crop_value,
                          distance_matrix = density_data[[STR_DISTANCE_MATRIX]])

      # mapping adjacency matrix with its parameters
      for (model in ret) {
        x <- setprops(model, agg_method, hd_threshold, link_threshold)
        model_ret <- c(model_ret, x)
      }

      host_densityrasts <- c(host_densityrasts, density_data$agg_crop)
    }

    return(list(model_res = model_ret, host_density = .host_map(host_densityrasts)))
  }

  rasters <- .rast_ro(global = global)

  if (global) {

    global_exts <- global_scales()

    graster <- .grast_ro()

    ret <- sean_geo(global_exts[[STR_EAST]])
    graster$east <- ret$model_res

    east_density <- ret$host_density

    ret <- sean_geo(global_exts[[STR_WEST]])
    graster$west <- ret$model_res

    west_density <- ret$host_density

    rasters$add_gr(graster)
    rasters$set_hd(terra::wrap(terra::merge(east_density, west_density)))

  } else {

    # Here, use extent of input spatRaster if not provided in argument
    actualscale <- if (is.null(geoscale)) {
      as.vector(terra::ext(.unpack_rast_ifnot(rast)))
    } else {
      geoscale
    }
    ret <- sean_geo(actualscale)
    rasters$rasters <- ret$model_res
    rasters$set_hd(terra::wrap(ret$host_density))
    rasters$global <- FALSE
  }

  return(rasters)
}

#' @rdname sean
#' @param res Numeric. The spatial aggregation factor that will be used to aggregate the raster layer of habitat availability, from fine to coars resolution. Default is [reso()].
#' @return GeoNetwork.
msean <- function(rast,
                  global = TRUE,
                  geoscale = NULL,
                  res = reso(),
                  ...,
                  outdir = tempdir()) {


  grasters <- sean(rast,
                   global = global,
                   geoscale = geoscale,
                   res = res,
                   ...)


  gmap <- .connectivity(grasters,
                        global,
                        geoscale,
                        res,
                        TRUE,
                        TRUE,
                        TRUE,
                        outdir = outdir)

  return(new("GeoNetwork",
             host_density = grasters$host_density,
             rasters = grasters,
             me_rast = gmap@me_rast,
             me_out = gmap@me_out,
             var_rast = gmap@var_rast,
             var_out = gmap@var_out,
             diff_rast = gmap@diff_rast,
             diff_out = gmap@diff_out))

}

.sean_linkweights <- function(hd_thresholds,
                              ...) {

  rasters <- future.apply::future_lapply(hd_thresholds,
                                         function(threshold) {
                                           sean(hd_threshold = threshold,
                                                ...)
                                         }, future.seed = TRUE)

  newrast <- .rast_ro()
  lapply(rasters, function(x) newrast$com(x))
  # host density is common for all the param combinations,
  # it will be memory extensive to store it for each param combination
  # thus, we will store it only once assuming it is same
  # this was verified at time of implementation using terra::CompareGeom
  newrast$set_hd(rasters[[1]]$host_density)

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
#' in the form of c(Xmin, Xmax, Ymin, Ymax) which EPSG:4326 in coordinate reference system. If `geoscale` is NuLL,
#'  the extent is extracted from `rast`(SpatRaster) using [terra::ext()].
#' @param link_thresholds Numeric vector. link threshold values
#' @param hd_thresholds Numeric vector. host density threshold values
#' @param ... Additional parameters to be passed to [sean()].
#' @inheritParams sean
#' @param outdir Character. Output directory for saving raster in TIFF format.
#' Default is [tempdir()].
#' @return A list of calculated CCRI indices after operations.
#' An index is generated for each combination of paramters.
#' One combination is equivalent to [sean()] function.
#' @export
#' @details
#' Error not handled for non-overlapping extents.
#' @inherit sensitivity_analysis seealso references
#' @seealso [msean_onrast()]
#'
sa_onrasters <- function(rast,
                         link_thresholds = c(0),
                         hd_thresholds = c(0),
                         ...) {

  .showmsg("New analysis started for given raster")

  packed_sp <- terra::wrap(rast)

  rasters <- future.apply::future_lapply(link_thresholds,
                                         function(lthreshold) {
                                           .sean_linkweights(
                                             rast = packed_sp,
                                             link_threshold = lthreshold,
                                             hd_thresholds = hd_thresholds,
                                             ...
                                           )
                                         }, future.seed = TRUE)

  newrast <- .rast_ro()
  lapply(rasters, function(x) newrast$com(x))
  newrast$set_hd(rasters[[1]]$host_density)

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

  gmap <- .connectivity(grast,
                        global,
                        geoscale,
                        res,
                        TRUE,
                        TRUE,
                        TRUE,
                        outdir)

  return(new("GeoNetwork",
             host_density = grast$host_density,
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
#' This function runs a sensitivity analysis on habitat connectivity
#' calculated based on every combination of selected parameters.
#' Parameter values in [sensitivity_analysis()] should be provided using the function [set_parameters()].
#' If no parameters are provided, then the [sensitivity_analysis()] function will run the sensitivity analysis
#' using a default set of parameter values,
#' which is accessible through the function [get_parameters()].
#' To customize parameter values, open the parameters.yaml
#' that was automatically downloaded when geohabnet was installed,
#' change, remove, or add parameter values directly in the parameters.yaml and save it.
#' Once the values have been changed manually, run [set_parameters()] to set the new parameter values,
#' which will return TRUE if the parameters were set successfully.
#'
#' @details
#' For each location in a region, [sensitivity_analysis()]
#' calculates the habitat connectivity risk index (CCRI) proposed by Xing et al. (2021).
#' If you are providing a map of habitat availability (as opposed to simply cropland density or host availability), you could call the output of your sensitivity analysis as the habitat connectivity index, which is a broader term than CCRI. :)
#' By default, sensitivity_analysis() runs a sensitivity analysis on a global extent,
#' see [global_scales()] for details.
#' This function also plots maps of the outcomes automatically,
#' but it will suppress maps for outcomes if `maps = FALSE` or
#' [interactive()] is `FALSE`.
#' The returned object is of class `GeoNetwork`, which contains two types of outcomes.
#' One outcome type corresponds to spatRasters representing the maps of habitat connectivity.
#' The second type corresponds to adjacency matrices used to calculate the habitat connectivity,
#' where columns and rows represent locations in the maps and
#' entries are the relative likelihood of pathogen or pest movement between each pair of nodes.
#' @param maps logical. `TRUE` if maps of outcomes are to be plotted, `FALSE` otherwise.
#' If `TRUE`, three maps are possible: a map of mean habitat connectivity, a map of variance of habitat connectivity,
#' and a map of the difference between the ranks in habitat connectivity and habitat density.
#' @param alert logical. `TRUE` if a beep sound is to be played once the analysis is completed, `FALSE` otherwise
#' @return GeoNetwork. Check documentation of the [sean()] function for better explanation of the parameters used.
#' @export
#'
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
#' _Global Cropland connectivity: A Risk Factor for Invasion and Saturation by Emerging Pathogens and Pests_,
#' BioScience, Volume 70, Issue 9, September 2020, Pages 744–758,
#' \doi{10.1093/biosci/biaa067}
#' @references Hijmans R (2023). _terra: Spatial Data Analysis_.
#' R package version 1.7-46, \url{https://CRAN.R-project.org/package=terra}
sensitivity_analysis <- function(maps = TRUE, alert = TRUE) {

  cparams <- load_parameters()

  # cutoff adjacency matrix
  host_thresholds <- cparams$`CCRI parameters`$HostDensityThreshold

  # crop data
  host_fp <- cparams$`CCRI parameters`$Host
  stopifnot("Host must be a file that can be converted to raster. E.g. TIFF" = file.exists(host_fp))
  crop_rasters <- get_rasters(host_fp)
  
  agg_methods <- cparams$`CCRI parameters`$AggregationStrategy # list

  # resolution
  resolution <- cparams$`CCRI parameters`$Resolution

  # global analysis
  isglobal <- cparams$`CCRI parameters`$GeoExtent$global
  geoscale <- geoscale_param(cparams)

  # dispersal models
  pl <- inv_powerlaw(cparams)

  ne <- neg_expo(cparams)

  rasters <- lapply(crop_rasters,
                    function(rast) {
                      sa_onrasters(rast = rast,
                                   global = isglobal,
                                   geoscale = geoscale,
                                   link_thresholds = cparams$`CCRI parameters`$LinkThreshold,
                                   hd_thresholds = host_thresholds,
                                   agg_methods = agg_methods,
                                   dist_method = cparams$`CCRI parameters`$DistanceStrategy,
                                   res = resolution,
                                   inv_pl = pl,
                                   neg_exp = ne)
                    })

  newrast <- .rast_ro()
  lapply(rasters, function(x) newrast$com(x))
  newrast$set_hd(rasters[[1]]$host_density)

  gmap <- if (maps == TRUE) {
    .connectivity(newrast,
                  isglobal,
                  geoscale,
                  resolution,
                  as.logical(cparams$`CCRI parameters`$PriorityMaps$MeanCC),
                  as.logical(cparams$`CCRI parameters`$PriorityMaps$Variance),
                  as.logical(cparams$`CCRI parameters`$PriorityMaps$Difference),
                  cparams$`CCRI parameters`$PriorityMaps$OutDir)
  }

  .showmsg("sensitivity analysis completed. Refer to maps for results.")
  if (alert == TRUE) {
    beepr::beep(2)
  }

  ret <- if (maps == TRUE) {
    new("GeoNetwork",
        host_density = .unpack_rast_ifnot(newrast$host_density),
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
