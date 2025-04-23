#' RiskMap class
#'
#' @description
#' An S4 class representing resulting maps from the specific operation type.
#' @field map Character. A file path to the map.
#' @field riid SpatRaster. This is one of the maps of habitat connectivity.
#' @field spr SpatRaster. A spatial raster representing the habitat connectivity index.
#' @field fp Character. A file path to the habitat connectivity raster.
#' @export
setClass(
  "RiskMap",
  slots = list(
    map = "character",
    riid = "ANY",
    spr = "ANY",
    fp = "character"
  ),
  prototype = list(
    map = NA_character_,
    riid = NA,
    spr = NA,
    fp = NA_character_
  )
)
