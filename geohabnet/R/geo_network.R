#' Gmap class
#'
#' @description
#' An S4 class to organize various maps in the form of `SpatRaster` in a single object.
#'
#' @slot me_rast
#' A raster object representing habitat connectivity of a region averaged across all selected parameters.
#'
#' @slot me_out
#' Character. A file path to where the mean habitat connectivity is saved.
#'
#' @slot diff_rast
#' A raster object representing the difference in ranks between mean habitat connectivity and habitat availability in a region.
#'
#' @slot diff_out
#' Character. A file path to where the difference raster is saved.
#'
#' @slot var_rast
#' A raster object representing the variance in habitat connectivity of a region calculated across all specified parameters.
#'
#' @slot var_out
#' Character. A file path to where the variance raster is saved.
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
#' SpatRaster. A raster used as background when plotting the map of mean habitat connectivity.
#'
#' @param vari
#' SpatRaster. A raster used as background when plotting the map of variance in habitat connectivity.
#'
#' @param dif
#' SpatRaster. A raster used as background when plotting the map of difference in habitat connectivity and habitat availability.
#'
#' @return
#' Gmap object.
#'
#' @export
setGeneric("setmaps", function(x, me, vari, dif) {
  standardGeneric("setmaps")
})

#' Network density
#'
#' This function first calculates the network density for each dispersal parameter specified by the user.
#' Network density compares the number of available links in a network versus the total number of possible links in the same network.
#' Network density is a measure of how well an entire network is, ranging from 0 (not connected at all) to 1 (fully connected).
#' This function then compares visually how network density changes with changes in dispersal parameter values.
#' In other words, it calculates and plots the network density of a GeoNetwork object.
#'
#' @param x A GeoNetwork object
#' @return Vector. Up to two ggplot2 objects
#' @export
setGeneric("ndplot", function(x) {
  standardGeneric("ndplot")
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
#' A GeoRaster object representing mean habitat connectivity.
#'
#' @param vari
#' A GeoRaster object representing variance in habitat connectivity.
#'
#' @param dif
#' A GeoRaster object representing the difference in ranks between habitat connectivity and habitat availability.
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
#' This will wrap all the results from the habitat connectivity analysis using [sean()] or [sensitivity_analysis()].
#' This class contains the field from `Gmap` class which has results in the form of `SpatRaster` and TIFF file.
#'
#' @slot rasters A list of `GeoRasters` objects.
#' @slot host_density A `SpatRaster` representing the habitat availability (or simply host density).
#' @slot me_rast
#' A raster representing mean habitat connectivity in a region.
#'
#' @slot me_out
#' Character. A file path to where the mean habitat connectivity raster is saved.
#'
#' @slot diff_rast
#' A raster representing the difference in ranks between habitat connectivity and habitat availability.
#'
#' @slot diff_out
#' Character. A file path to where the difference raster is located.
#'
#' @slot var_rast
#' A raster representing the variance in habitat connectivity in a region.
#'
#' @slot var_out
#' Character. A file path to where the variance raster is located.
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
