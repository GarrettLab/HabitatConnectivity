
#' @name GlobalRast
#' @docType class
#' @description
#' A class to represent raster for global scales.
#' @field east A list of raster for eastern hemisphere.
#' @field west A list of raster for western hemisphere.
#' @export
.grast_ro <- setRefClass("GlobalRast",
            fields = list(east = "ANY",
                          west = "ANY"))

#' @name GhabRasters
#' @docType class
#' @description
#' A class to represent raster vis-a-vis risk indices.
#' @field rasters List. List of raster representing risk indices.
#' @field global Boolean. True if contains `GlobalRast` object, False otherwise.
#' @export
.rast_ro <- setRefClass("GeoRasters",
            fields = list(rasters = "list",
                          global = "logical",
                          global_rast = "list"
                          ),
            methods = list(
              com = function(x) {
                if (x$global) {
                  global_rast <<- c(global_rast, x$global_rast)
                } else {
                  global <<- FALSE
                  rasters <<- c(rasters, x$rasters)
                }
                return(.self)
              },
              initialize = function(...) {
                global <<- TRUE
              },
              add_gr = function(x) {
                stopifnot("Object is not of type GlobaRast" = class(x) == "GlobalRast")
                global_rast <<- c(global_rast, x)
              }
            ))

