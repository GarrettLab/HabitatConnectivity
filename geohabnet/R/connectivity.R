#' Calculate and plot maps
#'
#' Calculate mean, variance and difference. The result is produced in form of maps plotted with predefined settings.
#' Currently, the settings for plot cannot be customized.
#' Default value is `TRUE` for all logical arguments
#' @param host SpatRaster. Host density map aka `SpatRaster` object
#' @param indices SpatRaster. Collection of risk indices.
#' @param global Logical. `TRUE` if global analysis is desired, `FALSE` otherwise.
#' `east` and `west` are required when `TRUE`.
#' @param east SpatRaster. Collection of risk indices on eastern extent.
#' @param west SpatRaster. Collection of risk indices on western extent.
#' When `TRUE`, `geoscale` is ignored. Default is `TRUE`.
#' @param geoscale Vector. geographical scale. Default is `NULL`.
#' @param res Numeric. Map resolution. This value is used in aggregation and dis-aggregation operation.
#' Default is [reso()].
#' @param pmean Logical. `TRUE` if map of mean should be plotted, `FALSE` otherwise.
#' @param pvar Logical. `TRUE` if variance map should be plotted, `FALSE` otherwise.
#' @param pdiff Logical. `TRUE` if difference map should be plotted, `FALSE` otherwise.
#' @param outdir Character. Output directory for saving raster in TIFF format.
#' Default is [tempdir()].
#' @return Gmap. See details.
#' @details
#' `indexes` are actually risk indices representing in the form of `spatRaster`
#' resulting from operations on crop's raster and
#' parameters provided in either `parameters.yaml` or [sean()].
#'
#' It will save all the opted plots using - `pmean`, `pvar` and `pdiff`.
#' File will be saved in provided value of `outdir` or  [tempdir()].If [interactive()] is `TRUE`,
#' then plots can be seen in active plot window. E.g. Rstudio. The maps are plotted using `SpatRaster` object.
#' These objects are available as a return value of this function.
#'
#' @inherit sensitivity_analysis references
#' @export
connectivity <- function(host,
                         indices,
                         global = FALSE,
                         east = NULL,
                         west = NULL,
                         geoscale = NULL,
                         res = reso(),
                         pmean = TRUE,
                         pvar = TRUE,
                         pdiff = TRUE,
                         outdir = tempdir()) {

  stopifnot("Require host parameter" = !is.null(host))
  .stopifnot_sprast(host)

  if (global) {
    stopifnot("Need east and west indices for global analysis" = !is.null(east), !is.null(west))
    stopifnot("East and west indices should be of same length" = length(east) == length(west))
  }

  actualscale <- if(is.null(geoscale)) {
    as.vector(terra::ext(host))
  } else {
    geoscale
  }

  mobj <- ccri_mean(indices, global, east, west, actualscale, res, pmean, outdir)

  vobj <- if (pvar == TRUE) {
    ccri_variance(indices,
                  mobj@riid,
                  global,
                  east,
                  west,
                  actualscale,
                  res,
                  outdir)
  }

  dobj <- if (pdiff == TRUE) {
    if (global) {
      geoscale <- .global_ext()
    }

    ccri_diff(mobj@riid,
              host,
              global,
              actualscale,
              res,
              outdir)
  }

  return(.merge_mapobs(mobj, vobj, dobj))
}

# private functions -------------------------------------------------------

.connectivity <- function(grast,
                          global = TRUE,
                          geoscale = NULL,
                          res = reso(),
                          pmean = TRUE,
                          pvar = TRUE,
                          pdiff = TRUE,
                          outdir = tempdir()) {

  ri_ind <- risk_indices(grast)

  return(connectivity(.unpack_rast_ifnot(grast$host_density),
                      ri_ind,
                      global,
                      east = ri_ind[[STR_EAST]],
                      west = ri_ind[[STR_WEST]],
                      geoscale,
                      res,
                      pmean,
                      pvar,
                      pdiff,
                      outdir))
}
