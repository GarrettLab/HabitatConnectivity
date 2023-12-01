#' @title Maps
#' @name Gmap
#' @docType class
#' @description
#' A class to represent various maps.
#' @field mean Numeric. A raster representing mean.
#' @field diff Numeric. A raster representing difference.
#' @field var Numeric. A raster representing variance.
#' @export
setClass("Gmap",
         slots = list(me_rast = "ANY",
                      me_out = "character",
                      diff_rast = "ANY",
                      diff_out = "character",
                      var_rast = "ANY",
                      var_out = "character"))

#' @name Rimap
#' @doctype class
#' @description
#' A class representing resulting maps from the specific operation type.
#' @export
setClass("RiskMap",
         slots = list(
           map = "character",
           riid = "ANY",
           spr = "ANY",
           fp = "character"),
         prototype = list(
           map = NA_character_,
           riid = NA,
           spr = NA,
           fp = NA_character_
         ))

setGeneric("setmaps", function(x, me, vari, dif) {
  standardGeneric("setmaps")
  })

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
#' @doctype class
#' @description
#' A class representing a network of geographical data.
#' @export
setClass("GeoNetwork", contains = "Gmap",
         slots = list(rasters = "ANY"),
         prototype = list(me_rast = NA, me_out = NA_character_, diff_rast = NA, diff_out = NA_character_, var_rast = NA, var_out = NA_character_, rasters = NA))

.indices <- function(crop_rasters) {
  risk_indices <- sapply(crop_rasters, function(x) x@indices)
  return(risk_indices)
}
