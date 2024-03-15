
# maps --------------------------------------------------------------------

#' Calculate mean of raster objects
#'
#'   Wrapper for [terra::mean()]. Calculates mean of list of rasters.
#' @inheritParams connectivity
#' @param indices List of SpatRasters.
#' This input represents the spatial raster collection for which mean is to be calculated.
#' @param plt `TRUE` if need to plot mean map, `FALSE` otherwise.
#' @return RiskMap. Contains result in the form of `SpatRaster` objects
#' and file path of the saved maps.
#'
#' @export
hci_mean <- function(indices,
                     global = FALSE,
                     east = NULL,
                     west = NULL,
                     geoscale = NULL,
                     res = reso(),
                     plt = TRUE,
                     outdir = tempdir()) {

  .cal_mean <- function(ext_indices) {
    mean_idx <- terra::app(terra::rast(ext_indices), sum, na.rm = TRUE) / length(ext_indices)
    mean_index_vals <- terra::values(mean_idx)
    zeroid <- which(mean_index_vals == 0)
    mean_idx[zeroid] <- NA

    return(mean_idx)
  }

  geoext <- geoscale

  mean_index <- if (global == TRUE) {

    .global_paramsok(global, east, west)

    east <- .cal_mean(east)
    west <- .cal_mean(west)
    geoext <- .global_ext()

    terra::merge(east, west, na.rm = TRUE)

  } else {
    stopifnot("Require geoscale parameter for non-global analysis" = !is.null(geoscale))

    .cal_mean(indices)
  }

  dis_mean_id <- terra::disagg(mean_index, fact = c(res, res))
  toplot <- dis_mean_id + .cal_zerorast(dis_mean_id, res)

  plt_ret <- if (plt == TRUE) {
    .plot(toplot,
          paste("Mean cropland connectivity risk index\n"),
          global,
          geoext,
          zlim = c(0, 1),
          typ = "mean",
          outdir = outdir)
  }

  return(new("RiskMap", map = "mean", riid = mean_index, spr = plt_ret[[1]], fp = plt_ret[[2]]))
}

#' Calculate variance of CCRI
#'
#'    This function produces a map of variance of CCRI based on input parameters
#' @inheritParams connectivity
#' @inheritParams hci_mean
#' @inherit hci_mean return
#' @param rast SpatRaster. Template for variance output
#' @export
hci_variance <- function(indices,
                         rast,
                         global,
                         east = NULL,
                         west = NULL,
                         geoscale,
                         res = reso(),
                         outdir = tempdir()) {

  .cal_var <- function(ext_indices, scale) {
    var_rastvect <-
      apply(do.call(cbind, lapply(ext_indices, terra::values)), 1, stats::var, na.rm = TRUE)

    scaled_rast <- terra::crop(rast, .to_ext(scale))
    scaled_rast[] <- var_rastvect

    var_disag_rast <-
      terra::disagg(scaled_rast, fact = c(res, res))

    var_disag_rast[var_disag_rast[] == 0] <- NA

    var_disag_rast + .cal_zerorast(var_disag_rast, res)
  }

  geoext <- geoscale

  var_out <- if (global == TRUE) {

    .global_paramsok(global, east, west)
    exts <- global_scales()

    east <- .cal_var(east, exts[[STR_EAST]])
    west <- .cal_var(west, exts[[STR_WEST]])

    geoext <- .global_ext()
    terra::merge(east, west)

  } else {

    .scaleok(geoscale)

    .cal_var(indices, geoscale)
  }

  z_var_w <- range(var_out[which(var_out[] > 0)])
  plt_ret <- .plot(var_out,
                   "Variance in cropland connectivity",
                   global,
                   geoext,
                   zlim = z_var_w,
                   typ = "variance",
                   outdir = outdir)

  return(new("RiskMap", map = "variance", riid = var_out, spr = plt_ret[[1]], fp = plt_ret[[2]]))
}

#' Calculate difference map
#'
#' This function produces a map of difference b/w mean and sum indexes in rank of cropland harvested area fraction.
#' @param x SpatRaster.
#' @param y SpatRaster.
#' @param geoscale Numeric vector. `x` will be cropped to this extent.
#' @inheritParams connectivity
#' @inheritParams hci_mean
#' @inherit hci_mean return
#' @details
#' Ideally, the function is tested to yield desired results when
#' `length(which(y[] > 0)) > length(which(x[] > 0))`.
#'
#' @export
hci_diff <- function(x,
                     y,
                     global,
                     geoscale,
                     res = reso(),
                     outdir = tempdir()) {
  # difference map
  # Function to check for missing or null values
  # Check if all arguments are SpatRaster objects
  lapply(list(x, y), .stopifnot_sprast)

  .scaleok(geoscale)
  geoext <- if (global) {
    .global_ext()
  } else {
    geoscale
  }

  zr2 <- c(0, 0)

  .cal_diff <- function(r1, r2, scale) {

    scaled_rast <- terra::crop(x, .to_ext(scale))
    ccri_id <- which(scaled_rast[] > 0)
    #meantotalland_w <- sum(r1, r2, na.rm = TRUE) / 2

    meanindexcell_w <- scaled_rast[][ccri_id]
    meantotallandcell_w <- y[][ccri_id]

    # mean cropland minus mean index,
    # negative value means importance of cropland reduce,
    # positive value means importance increase,
    # zero means the importance of cropland doesn't change.
    rankdifferent_w <-
      rank(meanindexcell_w * (-1)) - rank(meantotallandcell_w * (-1))
    scaled_rast[] <- NA
    scaled_rast[][ccri_id] <- rankdifferent_w

    maxrank_w <- max(abs(rankdifferent_w))
    zr2 <- max(zr2, range(-maxrank_w, maxrank_w))

    diagg_rast <- terra::disagg(scaled_rast,
                                fact = c(res, res))
    diagg_rast + .cal_zerorast(diagg_rast, res)
  }

  diff_out <- .cal_diff(x, y, geoext)

  plt_ret <- .plot(diff_out,
                   "Difference in rank of host connectivity and host density",
                   global,
                   geoext,
                   .get_palette_for_diffmap(),
                   zr2,
                   typ = "difference",
                   outdir)

  return(new("RiskMap", map = "difference", riid = diff_out, spr = plt_ret[[1]], fp = plt_ret[[2]]))
}


# private functions -------------------------------------------------------

.merge_mapobs <- function(m, v, d) {
  return(setmaps(new("Gmap"), m, v, d))
}

.global_paramsok <- function(global, east, west) {
  stopifnot("Require global parameter" = !is.null(global))
  stopifnot("Require east and west indices for global calculation" = !is.null(east), !is.null(west))
  stopifnot("Require list of east and west indices" = is.list(east) && is.list(west))
  return(TRUE)
}

.scaleok <- function(geoscale) {
  stopifnot("Require geoscale parameter for non-global analysis" = !is.null(geoscale))
}
