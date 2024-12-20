#' Only meant for global variables
#' @keywords internal
scales <- new.env(parent = emptyenv())
scales$geast <- c(-24, 180, -58, 80)
scales$gwest <- c(-140, -34, -58, 80)

#' Global geographical extent
#'
#'   This function provides the geographical extent used in a global analysis.
#'   This function returns the geographic extents of the eastern and western hemispheres used in the analysis.
#'   Each geographic extent is in the form of c(Xmin, Xmax, Ymin, Ymax). The geohabnet functions are designed to work with the coordinate reference system: lon/lat WGS 84 (EPSG:4326).
#' @return List. Named list with scales for eastern and western hemispheres
#' @export
#' @details
#' When a habitat connectivity analysis is global, the functions in geohabnet will conduct two separate analyses, one on the geographical scale of the eastern hemisphere and another for the western hemisphere.
#' The final outcomes (such as maps or adjacency matrices) are then combined for the global analysis.
#'
#' @seealso [set_global_scales()]
global_scales <- function() {
  sc <- list(scales$geast, scales$gwest)
  names(sc) <- c(STR_EAST, STR_WEST)
  return(sc)
}

.global_ext <- function(scales = global_scales()) {
  # xmin, xmax, ymin, ymax
  xmin <- min(scales[[STR_EAST]][1], scales[[STR_WEST]][1])
  xmax <- max(scales[[STR_EAST]][2], scales[[STR_WEST]][2])
  ymin <- min(scales[[STR_EAST]][3], scales[[STR_WEST]][3])
  ymax <- max(scales[[STR_EAST]][4], scales[[STR_WEST]][4])

  return(c(xmin, xmax, ymin, ymax))
}

#' Set global geographical extent
#'
#'   This function sets the geographical extents used in global analysis. See also [geoscale_param()] to set the geographic extent of an analysis that is not global.
#'   Each geographic extent should be in the form of c(Xmin, Xmax, Ymin, Ymax). Geographic extent must be specified by four values in degrees that represent the geographic limits of the area for analysis, following the order: minimum longitude, maximum longitude, minimum latitude, and maximum latitude. Degrees are in decimal notation and have a negative sign for the southern and western hemispheres.
#' @param value list. Named list of eastern and western hemisphere extents. See usage.
#' @inherit global_scales return
#' @export
#' @examples
#' set_global_scales(list(east = c(-24, 180, -58, 60), west = c(-140, -34, -58, 60)))
#' @seealso
#' [global_scales()]
#' [terra::ext()]
set_global_scales <- function(value) {

  east <- as.vector(value[[STR_EAST]])
  west <- as.vector(value[[STR_WEST]])
  stopifnot("Not a valid east coordinate" = is.numeric(east), is.vector(east), length(east) == 4)
  stopifnot("Not a valid west coordinate" = is.numeric(west), is.vector(west), length(west) == 4)

  scales$geast <- east
  scales$gwest <- west
  value <- c(east, west)
  return(global_scales())
}

#' Get geographical scales from the parameters
#'
#'   This function returns a list of geographical scales set in global and custom extent in `parameters.yaml`.
#'   If `global` is `TRUE`, the `CustomExt` is ignored.
#' @param gparams Optional. [load_parameters()] or null
#' @return Vector. A set of geographical scales
#' @export
geoscale_param <- function(gparams = load_parameters()) {

  cparams <- if (is.null(gparams)) {
    load_parameters()
  } else {
    gparams
  }
  xf <- cparams$`CCRI parameters`$GeoExtent$global
  if (as.logical(xf) == FALSE) {
    if (length(cparams$`CCRI parameters`$GeoExtent$customExt) != 4) {
      .showmsg("Geographical scale is missing in parameters. Will use extent of host density(SpatRaster)")
    }
  }
  return(as.vector(cparams$`CCRI parameters`$GeoExtent$customExt))
}
