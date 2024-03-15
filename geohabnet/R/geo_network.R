#' Gmap class
#'
#' @description
#' An S4 class to represent various maps in the form of `SpatRaster`.
#'
#' @slot me_rast
#' A raster representing mean risk index.
#'
#' @slot me_out
#' Character. A file path to the mean risk index raster.
#'
#' @slot diff_rast
#' A raster representing difference.
#'
#' @slot diff_out
#' Character. A file path to the difference raster.
#'
#' @slot var_rast
#' A raster representing variance.
#'
#' @slot var_out
#' Character. A file path to the variance raster.
#'
#' @export
setClass("Gmap",
         slots = list(
           me_rast = "ANY",
           me_out = "character",
           diff_rast = "ANY",
           diff_out = "character",
           var_rast = "ANY",
           var_out = "character"
         ))

#' Sets the slots in the Gmap object.
#'
#' @rdname Gmap-class
#'
#' @param x
#' Gmap object.
#'
#' @param me
#' SpatRaster. A raster representing mean risk index.
#'
#' @param vari
#' SpatRaster. A raster representing variance.
#'
#' @param dif
#' SpatRaster. A raster representing difference.
#'
#' @return
#' Gmap object.
#'
#' @export
setGeneric("setmaps", function(x, me, vari, dif) {
  standardGeneric("setmaps")
})

#' Sets the map slots in the Gmap object.
#'
#'
#' This wraps the results(SpatRasters) from the risk analysis.
#'
#' @param x
#' A Gmap object.
#'
#' @param me
#' A GeoRaster object representing mean risk index.
#'
#' @param vari
#' A GeoRaster object representing variance.
#'
#' @param dif
#' A GeoRaster object representing difference.
#'
#' @return
#' A Gmap object.
#'
#' @export
#'
#' @method setmaps Gmap
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

#' GeoNetwork
#'
#' @description
#' An S4 class representing a network of geographical data.
#' This will wrap all the results from the risk analysis using [sean()] or [sensitivity_analysis()].
#' This class contains the field from `Gmap` class which has results in the form of `SpatRaster` and TIFF file.
#'
#' @slot rasters A list of `GeoRasters` objects.
#' @slot host_density A `SpatRaster` representing the host density.
#' @slot me_rast
#' A raster representing mean risk index.
#'
#' @slot me_out
#' Character. A file path to the mean risk index raster.
#'
#' @slot diff_rast
#' A raster representing difference.
#'
#' @slot diff_out
#' Character. A file path to the difference raster.
#'
#' @slot var_rast
#' A raster representing variance.
#'
#' @slot var_out
#' Character. A file path to the variance raster.
#'
#' @export
setClass("GeoNetwork", contains = "Gmap",
         slots = list(
           host_density = "ANY",
           rasters = "ANY"
         ),
         prototype = list(
           host_density = NA,
           me_rast = NA,
           me_out = NA_character_,
           diff_rast = NA,
           diff_out = NA_character_,
           var_rast = NA,
           var_out = NA_character_,
           rasters = NA
         ))


#' Set the host density.
#'
#'
#' Sets the host density slot in the GeoNetwork object
#' @param x the GeoNetwork object.
#' @param value SpatRaster.
setGeneric("host_density<-", function(x, value) {
  standardGeneric("host_density<-")
})


#' @rdname GeoNetwork-class
#'
#' @param x
#' GeoNetwork.
#'
#' @param value
#' SpatRaster.
#'
#' @return
#' GeoNetwork.
#'
#' @export
setMethod("host_density<-", "GeoNetwork", function(x, value) {
  stopifnot(class(value) == "SpatRaster", "value must be of type SpatRaster")
  x@host_density <- value
  x
})

#' Internal function to extract risk indices from a list of crop rasters.
#'
#' @param crop_rasters
#' List of raster objects.
#'
#' @return
#' A list of risk indices.
#'
#' @export
.indices <- function(crop_rasters) {
  risk_indices <- sapply(crop_rasters, function(x) x@indices)
  return(risk_indices)
}
