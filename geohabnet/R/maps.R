#' Calculate and plot maps
#'
#' Calculate mean, variance and difference. The result is produced in form of maps plotted with predefined settings.
#' Currently, the settings for plot cannot be customized.
#' Default value is `TRUE` for all logical arguments
#' @param indexes list of rasters. See details.
#' @param global logical. `TRUE` if global analysis is required, `FALSE` otherwise.
#' When `TRUE`, `geoscale` is ignored. Default is `TRUE`.
#' @param geoscale vector. geographical scale
#' @param res numeric. map resolution
#' @param pmean `TRUE` if map of mean should be plotted, `FALSE` otherwise.
#' @param pvar `TRUE` if variance map should be plotted, `FALSE` otherwise
#' @param pdiff `TRUE` if difference map should be plotted, `FALSE` otherwise
#' @details
#' `indexes` are actually risk resulting from operations on crop's raster and
#' parameters provided in either `parameters.yaml` or [sean()].
#'
#' It will save all the opted plots using - `pmean`, `pvar` and `pdiff`.
#' File will be saved in [getwd()].If [interactive()] is `TRUE`,
#' then plots can be seen in active plot window. E.g. Rstudio
#'
#' @inherit sensitivity_analysis references
#' @export
connectivity <- function(indexes,
                         global = TRUE,
                         geoscale,
                         res = reso(),
                         pmean = TRUE,
                         pvar = TRUE,
                         pdiff = TRUE) {

  mean_rast <- ccri_mean(indexes, global, geoscale, pmean)

  if (pvar == TRUE) {
    ccri_variance(
      indexes,
      mean_rast,
      global,
      geoscale,
      res)
  }

  if (pdiff == TRUE) {
    if (global) {
      geoscale <- .global_ext()
    }

    ccri_diff(
      mean_rast,
      the$cropharvest_aggtm_crop,
      the$cropharvest_agglm_crop,
      global,
      geoscale,
      res)
  }
  invisible()
}

# maps --------------------------------------------------------------------

#' Calculate mean of raster objects
#'
#'   Wrapper for [terra::mean()]. Calculates mean of list of rasters.
#' @inheritParams connectivity
#' @param plt `TRUE` if need to plot mean map, `FALSE` otherwise and `geoscale`.
#' @export
ccri_mean <- function(indexes,
                      global = TRUE,
                      geoscale = NULL,
                      plt = TRUE) {

  .cal_mean <- function(ext_indices) {
    mean_idx <- terra::app(terra::rast(ext_indices), sum, na.rm = TRUE) / length(ext_indices)
    mean_index_vals <- terra::values(mean_idx)
    zeroid <- which(mean_index_vals == 0)
    mean_idx[zeroid] <- NaN
    mean_idx
  }

  mean_index <- if (global == TRUE) {

    .gan_paramok(indexes)
    east <- .cal_mean(indexes[[STR_EAST]])
    west <- .cal_mean(indexes[[STR_WEST]])
    geoscale <- .global_ext()
    terra::merge(east, west)

  } else {
    .cal_mean(indexes)
  }

  if (plt == TRUE) {
    .plot(mean_index,
          paste("Mean cropland connectivity risk index\n"),
          global,
          geoscale,
          zlim = c(0, 1),
          typ = "mean")
  }

  return(mean_index)
}

#' Calculate variance of CCRI
#'
#'    This function produces a map of variance of CCRI based on input parameters
#' @inheritParams connectivity
#' @param rast A raster object. It will be used in calculating variance.
#' @export
ccri_variance <- function(indexes,
                          rast,
                          global,
                          geoscale,
                          res = reso()) {

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

  var_out <- if (global == TRUE) {

    .gan_paramok(indexes)
    exts <- global_scales()
    east <-
      .cal_var(indexes[[STR_EAST]], exts[[STR_EAST]])
    west <-
      .cal_var(indexes[[STR_WEST]], exts[[STR_WEST]])

    geoscale <- .global_ext()
    terra::merge(east, west)

  } else {
    .cal_var(indexes, geoscale)
  }

  z_var_w <- range(var_out[which(var_out[] > 0)])
  .plot(var_out,
        "Variance in cropland connectivity",
        global,
        geoscale,
        zlim = z_var_w,
        typ = "variance")

  invisible(1)
}

#' Calculate difference map
#'
#' This function produces a map of difference b/w mean and sum indexes in rank of cropland harvested area fraction.
#' @param rast A raster object for mean index raster difference
#' @param x A raster object for cropland harvest
#' @param y A raster object for cropland harvest
#' @inheritParams connectivity
#' @export
ccri_diff <- function(rast,
                      x,
                      y,
                      global,
                      geoscale,
                      res = reso()) {
  # difference map
  # Function to check for missing or null values
  .params_ok <- function(...) {
    !any(sapply(list(...), function(r) missing(r) || is.null(r)))
  }

  zr2 <- c(0, 0)

  .cal_diff <- function(r1, r2, scale) {

    scaled_rast <- terra::crop(rast, .to_ext(scale))
    ccri_id <- which(scaled_rast[] > 0)
    meantotalland_w <- sum(r1, r2, na.rm = TRUE) / 2

    meanindexcell_w <- scaled_rast[][ccri_id]
    meantotallandcell_w <- meantotalland_w[][ccri_id]

    # mean cropland minus mean index,
    # negative value means importance of cropland reduce,
    # positive value means importance increase,
    # zero means the importance of cropland doesn't change.
    rankdifferent_w <-
      rank(meantotallandcell_w * (-1)) - rank(meanindexcell_w * (-1))
    scaled_rast[] <- NaN
    scaled_rast[][ccri_id] <- rankdifferent_w

    maxrank_w <- max(abs(rankdifferent_w))
    zr2 <- max(zr2, range(-maxrank_w, maxrank_w))

    diagg_rast <- terra::disagg(scaled_rast,
                              fact = c(res, res))
    diagg_rast + .cal_zerorast(diagg_rast, res)
  }

  diff_out <- if (global == TRUE) {

    if (!.params_ok(the$gan[["sum"]][[STR_EAST]],
                    the$gan[["mean"]][[STR_EAST]],
                    the$gan[["sum"]][[STR_WEST]],
                    the$gan[["mean"]][[STR_WEST]])) {
      message("Either sum or mean aggregate is missing. Aborting difference calculation")
      return(NULL)
    }

    exts <- global_scales()

    east_var <-
      .cal_diff(the$gan[["sum"]][[STR_EAST]], the$gan[["mean"]][[STR_EAST]], exts[[STR_EAST]])
    west_var <-
      .cal_diff(the$gan[["sum"]][[STR_WEST]], the$gan[["mean"]][[STR_WEST]], exts[[STR_WEST]])

    geoscale <- .global_ext(exts)
    terra::merge(east_var, west_var)
  } else {
    if (!.params_ok(x, y)) {
      message("Either sum or mean aggregate is missing. Aborting difference calculation")
      return(NULL)
    }
    .cal_diff(x, y, geoscale)
  }

  .plot(diff_out,
        "Difference in rank of host connectivity and host density",
        global,
        geoscale,
        .get_palette_for_diffmap(),
        zr2,
        typ = "difference")

  invisible()
}

# private functions -------------------------------------------------------

.plot <- function(rast,
                  label,
                  isglobal,
                  geoscale,
                  colorss = .get_palette(),
                  zlim,
                  typ = "plot",
                  plotf = .plotmap) {

  .saverast(typ, rast)

  if (is.null(plotf)) {
    .plotmap(rast, geoscale, isglobal, label, colorss, zlim)
  } else {
    plotf(rast = rast,
      geoscale = geoscale,
      isglobal = isglobal,
      label = label,
      col_pal = colorss,
      zlim = zlim)
  }

  invisible()
}

.saverast <- function(typ, rast) {
  # Create the "plots" directory if it doesn't exist
  if (!dir.exists("plots")) {
    dir.create("plots")
  }

  # Save the plot as a raster file
  fname <- paste("plots", "/", typ,
                 "_",
                 stringr::str_replace_all(Sys.time(), "[^a-zA-Z0-9]", ""),
                 ".tif", sep = "")
  terra::writeRaster(rast, overwrite = TRUE,
                     filename = fname,
                     gdal = c("COMPRESS=NONE"))
  message(paste("raster created", fname, sep = ": "), "\n")
}

.plotmap <- function(rast, geoscale, isglobal, label, col_pal, zlim) {
  if (interactive()) {

    # Set the plot parameters
    graphics::par(bg = "aliceblue")
    # Plot the base map
    terra::plot(.cal_mgb(geoscale, isglobal),
                col = "grey85",
                xaxt = "n",
                yaxt = "n",
                legend = FALSE,
                main = label,
                cex.main = 0.9)
    # Plot the raster
    if (isglobal == TRUE) {
      gs <- terra::ext(rast)
      terra::plot(rast,
                  col = col_pal,
                  xaxt = "n",
                  yaxt = "n",
                  zlim = zlim,
                  add = TRUE,
                  lwd = 0.7,
                  legend = TRUE,
                  plg = list(loc = "bottom",
                             ext = c(gs[1] + 30, gs[2] - 30, gs[3] - 30, gs[3] - 20),
                             horizontal = TRUE)
      )
    } else {
      terra::plot(rast,
                  col = col_pal,
                  xaxt = "n",
                  yaxt = "n",
                  zlim = zlim,
                  add = TRUE,
                  lwd = 0.7,
                  legend = TRUE
      )
    }

    # plg = list(loc = "bottom", horizontal = TRUE)
    # Plot the country boundaries
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    world <- world[which(world$continent != "Antarctica"), ]["geometry"]
    world <- terra::vect(world)

    if (isglobal == FALSE) {
      world <- terra::crop(world, terra::ext(rast))
    }

    terra::plot(world, col = NA, border = "black", add = TRUE)
  }
  invisible()
}

.gan_paramok <- function(indices) {
  stopifnot("Require list of east and west indices" = all(c(STR_EAST, STR_WEST) %in% names(indices)))
  return(TRUE)
}
