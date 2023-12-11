#' @name RiskMap
#' @title RiskMap. Class to represent risk maps from geohabnet
#' @docType class
#' @description
#' A class representing resulting maps from the specific operation type.
#' @field map Character. A file path to the map.
#' @field riid SpatRaster. This is one of the risk maps.
#' @field spr SpatRaster. A spatial raster representing the risk index.
#' @field fp Character. A file path to the risk index raster.
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
