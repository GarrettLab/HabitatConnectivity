#' @title Maps
#' @name Gmap
#' @docType class
#' @description
#' A class to represent various maps.
#' @field me_rast SpatRaster A raster representing mean risk index.
#' @field me_out Character. A file path to the mean risk index raster.
#' @field diff_rast SpatRaster A raster representing difference.
#' @field diff_out Character. A file path to the difference raster.
#' @field var_rast Numeric. A raster representing variance.
#' @field var_out SpatRaster A file path to the variance raster.
#' @export
setClass("Gmap",
         slots = list(me_rast = "ANY",
                      me_out = "character",
                      diff_rast = "ANY",
                      diff_out = "character",
                      var_rast = "ANY",
                      var_out = "character"))

setGeneric("setmaps", function(x, me, vari, dif) {
  standardGeneric("setmaps")
})


#' @title setmaps
#' @export
#' @method setmaps Gmap
#' @param x A Gmap object.
#' @param me A GeoRaster object representing mean risk index.
#' @param vari A GeoRaster object representing variance.
#' @param dif A GeoRaster object representing difference.
#' @return A Gmap object.
setMethod("setmaps", "Gmap", function(x, me, vari, dif) {

  if (!is.null(me)) {
    x@me_rast <- me@spr
    x@me_out <- me@fp
  }

  if (!is.null(vari)) {
    x@var_rast <- vari@spr
    x@var_out <- vari@fp
  }

  if (!is.null(dif)) {
    x@diff_rast <- dif@spr
    x@diff_out <- dif@fp
  }

  validObject(x)
  return(x)
})

#' @name GeoNetwork
#' @title Class GeoNetwork.
#' @docType class
#' @description
#' A class representing a network of geographical data.
#' This will wrap all the results from the risk analysis using [sean()] or [sensitivity_analysis()].
#' This class contains the field from `Gmap` class which has results in the form of `SpatRaster` and TIFF file.
#' @field rasters A list of \code{\link{GeoRasters}} objects.
#' @export
setClass("GeoNetwork", contains = "Gmap",
         slots = list(rasters = "ANY"),
         prototype = list(me_rast = NA,
                          me_out = NA_character_,
                          diff_rast = NA,
                          diff_out = NA_character_,
                          var_rast = NA,
                          var_out = NA_character_,
                          rasters = NA))

.indices <- function(crop_rasters) {
  risk_indices <- sapply(crop_rasters, function(x) x@indices)
  return(risk_indices)
}
