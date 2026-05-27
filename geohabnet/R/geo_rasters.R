#' GlobalRast class
#'
#' @description
#' A class to represent raster objects for global scales. Global scales are accessible using [global_scales()].
#' However, this class encapsulates the results of apply dispersal models and metrics.
#' @field east A list of raster objects for eastern hemisphere.
#' @field west A list of raster objects for western hemisphere.
#' @export
.grast_ro <- setRefClass("GlobalRast",
                         fields = list(east = "ANY",
                                       west = "ANY"))

#' GeoRaster class
#'
#' @description
#' A class to represent raster vis-a-vis risk indices.
#' This class encapsulates the results of apply dispersal models and metrics.
#' @field habitat_density SpatRaster. A spatial raster representing habitat density.
#' @field rasters List. List of raster representing risk indices. These are of type `GeoModels`.
#' @field global Boolean. True if contains `GlobalRast` object, False otherwise.
#' @export
.rast_ro <- setRefClass("GeoRasters",
                        fields = list(habitat_density = "ANY",
                                      rasters = "list",
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
                            ## commenting this because it will take lot of memory to store the same
                            # habitat density again and again. Handled externally wherever required.
                            # if (!is.null(habitat_density) &&
                            #     terra::compareGeom(x$habitat_density, habitat_density) != TRUE) {
                            #   warning("The habitat_density density is different")
                            # }
                            # habitat_density <<- x$habitat_density
                            return(.self)
                          },
                          initialize = function(...) {
                            global <<- TRUE
                            habitat_density <<- NULL
                          },
                          add_gr = function(x) {
                            stopifnot("Object is not of type GlobaRast" = class(x) == "GlobalRast")
                            global_rast <<- c(global_rast, x)
                          },
                          set_hd = function(x) {
                            stopifnot("Habitat density must be of type SpatRaster" = class(x) == "SpatRaster" |
                                        .is_packed_rast(x))
                            habitat_density <<- x
                          }
                        ))
