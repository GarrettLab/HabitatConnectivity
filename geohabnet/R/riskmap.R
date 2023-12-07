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
