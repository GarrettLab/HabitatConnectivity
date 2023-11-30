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
setClass("Rimap",
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

setMethod("setmaps", "Person", function(x, me, vari, dif) {
  
  if (!is.null(me) & !is.na(me)) {
    x@me_rast <- me@spr
    x@me_out <- me@fp 
  }

  if (!is.null(vari) & !is.na(vari)) {
    x@var_rast <- vari@spr
    x@var_out <- vari@sfp 
  }
  
  if(!is.null(dif) & !is.null(dif)) {
    x@diff_rast <- dif@spr
    x@dif_out <- dif@fp 
  }

  validObject(x)
  return(x)
})

setClass("Network", contains = "Gmap",
         slots = list(indices = "ANY"),
         prototype = structure(Gmap(), indices = NA))

.indices <- function(crop_rasters) {
  risk_indices <- sapply(crop_rasters, function(x) x@indices)
  return(risk_indices)
}