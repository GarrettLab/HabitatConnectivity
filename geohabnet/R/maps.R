#' Calculate and plot maps
#'     Calculate mean, variance and difference. The result is produced in form of maps plotted with predefined settings.
#'     Currently, the settings for plot cannot be customized.
#' @param indexes list of raster
#' @param geoscale geographical scale
#' @param reso map resolution
#' @param pmean `TRUE` if map of mean should be plotted, `FALSE` otherwise
#' @param pvar `TRUE` if variance map should be plotted, `FALSE` otherwise
#' @param pdiff `TRUE` if difference map should be plotted, `FALSE` otherwise
#' @details
#' It will save all the opted plots using - `pmean`, `pvar` and `pdiff`.
#' It will save the file in [getwd()].
#'
#' @export
plot_maps <- function(indexes, geoscale, reso, pmean = TRUE, pvar = TRUE, pdiff = TRUE) {

  mean_rast <- ccri_mean(indexes, geoscale, reso, pmean)

  if (pvar == TRUE) {
    ccri_variance(
      lapply(indexes, terra::values),
      mean_rast,
      geoscale,
      reso)
  }

  if (pdiff == TRUE) {
    cal_diff_map(
      mean_rast,
      the$cropharvest_aggtm_crop,
      the$cropharvest_agglm_crop,
      geoscale,
      reso)
  }
}

# maps --------------------------------------------------------------------

#' Calculate mean of raster objects
#'   Overriding for [terra::mean()]. Calculates mean of list of rasters.
#' @param indexes raster list
#' @param geoscale geographical scale for plotting
#' @param reso resolution for plotting
#' @param plt `TRUE` if need to plot mean map, `FALSE` otherwise and `geoscale`, `reso` is ignored.
#' @export
ccri_mean <- function(indexes, geoscale = NULL, reso = NULL, plt = TRUE) {
  mean_index <- terra::app(terra::rast(indexes), sum) / length(indexes)
  mean_index_vals <- terra::values(mean_index)
  zeroid <- which(mean_index_vals == 0)
  mean_index[zeroid] <- NaN

  if (plt == TRUE) {
    .plot(mean_index,
          paste("Mean cropland connectivity risk index\n",
                            "resolution = ", the$parameters_config$`CCRI parameters`$Resolution),
          geoscale,
          zlim = c(0, 1),
          typ = "mean")
  }

  return(mean_index)
}

#' Calculate variance of CCRI
#'    This function produces a map of variance of CCRI based on input parameters
#' @param indexes A list of index values
#' @param rast A raster object. It will be used in calculating variance.
#' @param geoscale Geographical scale. This will be used in calculating difference.
#' @param resolution resolution to plot raster and map
#' @export
ccri_variance <- function(indexes,
                          rast,
                          geoscale,
                          resolution = the$parameters_config$`CCRI parameters`$Resolution) {
  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  var_rast <- apply(do.call(cbind, indexes), 1, stats::var, na.rm = TRUE)

  rast[] <- var_rast
  z_var_w <- range(var_rast[which(var_rast > 0)])

  var_disag_rast <- terra::disagg(rast, fact = c(resolution, resolution))
  var_disag_rast <- var_disag_rast + .cal_zerorast(var_disag_rast, resolution)

  .plot(var_disag_rast, "Variance in cropland connectivity", geoscale, zlim = z_var_w, typ = "variance")

  invisible(1)
}

#' Calculate difference map
#' This function produces a map of difference b/w mean and sum indexes in rank of cropland harvested area fraction.
#' @param mean_index_rast A raster object for mean index raster difference
#' @param cropharvest_aggtm_crop A raster object for cropland harvest
#' @param cropharvest_agglm_crop A raster object for cropland harvest
#' @param geoscale geographical scale with longitude and latitudes. It will be converted into extent.
#' See [get_geographic_scales()] and [terra::ext()].
#' @param resolution resolution to plot raster and map
#' @export
cal_diff_map <- function(mean_index_rast,
                         cropharvest_aggtm_crop,
                         cropharvest_agglm_crop,
                         geoscale,
                         resolution = the$parameters_config$`CCRI parameters`$Resolution) {
  # difference map
  if (missing(cropharvest_aggtm_crop) || missing(cropharvest_agglm_crop)) {
    message("Either sum or mean aggregate is missing. Aborting difference calculation")
    return(NULL)
  }
  if (is.null(cropharvest_aggtm_crop) || is.null(cropharvest_agglm_crop)) {
    message("Either sum or mean aggregate is missing. Aborting difference calculation")
    return(NULL)
  }

  ccri_id <- which(mean_index_rast[] > 0)
  meantotalland_w <- sum(cropharvest_aggtm_crop, cropharvest_agglm_crop, na.rm = TRUE) / 2

  meanindexcell_w <- mean_index_rast[][ccri_id]
  meantotallandcell_w <- meantotalland_w[][ccri_id]

  # mean cropland minus mean index, negative value means importance of cropland reduce,
  # positive value means importance increase, zero means the importance of cropland doesn't change.
  rankdifferent_w <- rank(meantotallandcell_w * (-1)) - rank(meanindexcell_w * (-1))
  mean_index_rast[] <- NaN
  mean_index_rast[][ccri_id] <- rankdifferent_w

  maxrank_w <- max(abs(rankdifferent_w))
  zr2 <- range(-maxrank_w, maxrank_w)

  paldif4 <- .get_palette_for_diffmap()

  diagg_rast <- terra::disagg(mean_index_rast,
                            fact = c(resolution, resolution))
  diagg_rast <- diagg_rast + .cal_zerorast(diagg_rast, resolution)

  .plot(diagg_rast, "Difference in rank of host density and host connectivity",
        geoscale, paldif4, zr2, typ = "difference")

  invisible(2)
}

# private functions -------------------------------------------------------

.plot <- function(rast, label, geoscale, colorss = .get_palette(), zlim, typ = "plot") {

  # Set the plot parameters
  graphics::par(mar = c(0, 0, 0, 0), bg = "aliceblue")

  # Plot the base map
  terra::plot(.cal_mgb(geoscale),
              col = "grey85", xaxt = "n", yaxt = "n", axes = FALSE, box = FALSE, legend = FALSE,
              main = label, cex.main = 0.9)

  # Plot the raster
  terra::plot(rast,
              col = colorss, zlim = zlim, xaxt = "n", yaxt = "n",
              axes = FALSE, box = FALSE, add = TRUE, lwd = 0.7, )
  # Plot the country boundaries
  world <- rnaturalearthdata::countries110
  terra::plot(world, col = NA, border = "black", add = TRUE)

  # Add a legend
  if (!is.null(zlim)) {
    graphics::legend("bottomright",
                     legend = round(zlim, 1),
                     fill = colorss,
                     bty = "n",
                     cex = 0.8,
                     border = "black"
    )
  }

  # Save the plot as a raster file
  fname <- paste(typ,
                 "_",
                 stringi::stri_rand_strings(1, 5),
                 ".tif", sep = "")
  terra::writeRaster(rast, overwrite = TRUE, filename = fname, gdal = c("COMPRESS=NONE"))
  cat("raster created", fname, sep = ": ")

  invisible(4)
}
