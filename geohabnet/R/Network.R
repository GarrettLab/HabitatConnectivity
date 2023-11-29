#' @title Maps
#' @docType class
#' @description
#' A class to represent various maps.
#' @field mean Numeric. A raster representing mean.
#' @field diff Numeric. A raster representing difference.
#' @field var Numeric. A raster representing variance.
#' @export
setRefClass("Cmap",
            fields = list(mean = "numeric",
                          diff = "numeric",
                          var = "numeric"
            ))

setRefClass("Network", contains = "Cmap",
            fields = list(indices = "ANY"))

.indices <- function(crop_rasters) {
  risk_indices <- sapply(crop_rasters, function(x) x@indices)
  return(risk_indices)
}