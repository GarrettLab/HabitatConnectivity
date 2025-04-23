#' Calculate and plot maps of habitat connectivity
#'
#' Calculate the mean habitat connectivity across selected parameter, variance in habitat connectivity, and the difference in ranks between mean habitat connectivity and habitat availability. The result is produced in form of maps plotted with predefined graphics settings.
#' Currently, the settings for plot cannot be customized.
#' Default value is `TRUE` for all logical arguments
#' @param host SpatRaster. A `SpatRaster` object for the spatial distribution of habitat availability (such as host availability or cropland density). Note that a valid input data for geohabnet is a raster layer of habitat availability (such as host availability), in which each grid cell has any values between zero and one. If you are using your own dataset, please also make sure that your raster layer is in the standard coordinate reference system (i.e., EPSG:4326), the only CRS supported by geohabnet for now.
#' @param indices SpatRaster. Collection of risk indices.
#' @param global Logical. Select `TRUE` if a global analysis is desired, `FALSE` otherwise.
#' `east` and `west` are required when `TRUE`.
#' @param east SpatRaster. Collection of risk indices on eastern extent.
#' @param west SpatRaster. Collection of risk indices on western extent.
#' When `TRUE`, `geoscale` is ignored. Default is `TRUE`.
#' @param geoscale Vector. This refers to the geographical extent for the habitat connectivity analysis when `global` is set to `FALSE`. Default is `NULL`.
#' @param res Numeric. This parameter refers to the spatial aggregation factor. This value is the number of cells that are grouped when aggregating a raster layer from fine to coarse spatial resolution to reduce computational costs. Setting this parameter to 1 would not aggregate the raster layers.
#' Default is [reso()].
#' @param pmean Logical. `TRUE` if a map of mean habitat connectivity should be plotted, `FALSE` otherwise.
#' @param pvar Logical. `TRUE` if a map of the variance in habitat connectivity should be plotted, `FALSE` otherwise.
#' @param pdiff Logical. `TRUE` if a map of the difference in the ranks between the mean habitat connectivity and habitat availability should be plotted, `FALSE` otherwise.
#' @param outdir Character. Output directory for saving raster in TIFF format.
#' Default is [tempdir()].
#' @return Gmap. See details.
#' @details
#' `indexes` has a list of `spatRaster` objects resulting from the unique combinations of all parameters specified in either `parameters.yaml` or [sean()].
#' For each unique combination of parameters, an index of habitat connectivity is calculated for each location in a landscape.
#' Then these indices are used to calculate the mean and variance of habitat connectivity of a location across all specified parameters.
#'
#' This function will save all the opted plots using - `pmean`, `pvar` and `pdiff`.
#' File will be saved in provided value of `outdir` or  [tempdir()]. If [interactive()] is `TRUE`,
#' then plots can be seen in active plot window (e.g., plot panel in Rstudio). The maps are plotted using `SpatRaster` objects.
#' These `SpatRaster` objects are also available as a return value of this function.
#'
#' @inherit sensitivity_analysis references
#' @seealso [hci_mean()], [hci_variance()], [hci_diff()]
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
  stopifnot("Require indices" = !is.null(indices))

  .stopifnot_sprast(host)

  if (global) {
    stopifnot("Need east and west indices for global analysis" = !is.null(east), !is.null(west))
    stopifnot("East and west indices should be of same length" = length(east) == length(west))
  }

  actualscale <- if (is.null(geoscale)) {
    as.vector(terra::ext(host))
  } else {
    geoscale
  }

  mobj <- hci_mean(indices, global, east, west, actualscale, res, pmean, outdir)

  vobj <- if (pvar == TRUE) {
    hci_variance(indices,
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

    hci_diff(mobj@riid,
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
