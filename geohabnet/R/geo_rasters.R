
#' @name GlobalRast
#' @title GlobalRast class
#' @docType class
#' @description
#' A class to represent raster for global scales. Global scales are accessible using [global_scales()].
#' However, this class encapsulates the results of apply dispersal models and metrics.
#' @field east A list of raster for eastern hemisphere.
#' @field west A list of raster for western hemisphere.
#' @export
.grast_ro <- setRefClass("GlobalRast",
                         fields = list(east = "ANY",
                                       west = "ANY"))

#' @name GeoRasters
#' @title GeoRasters. Class to represent rasters from geohabnet
#' @docType class
#' @description
#' A class to represent raster vis-a-vis risk indices.
#' This class encapsulates the results of apply dispersal models and metrics.
#' @field rasters List. List of raster representing risk indices. These are of type `GeoModels`.
#' @field global Boolean. True if contains `GlobalRast` object, False otherwise.
#' @export
.rast_ro <- setRefClass("GeoRasters",
                        fields = list(rasters = "list",
                                      global = "logical",
                                      global_rast = "list"),
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
