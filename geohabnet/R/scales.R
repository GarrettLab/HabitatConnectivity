#' Only meant to global variables
#' @keywords internal
scales <- new.env(parent = emptyenv())
scales$geast <- c(-24, 180, -58, 60)
scales$gwest <- c(-140, -34, -58, 60)

#' Global geographical extent
#'
#'   See geographical extents used in global analysis.
#'   Returns eastern and western hemisphere extents.
#'   Each extent is in the form of c(Xmin, Xmax, Ymin, Ymax).
#' @return List. Named list with scales for eastern and western hemisphere
#' @export
#' @details
#' Seperate analysis on geographical scales of eastern and western hemisphere
#' are combined to run global analysis.
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
#'   Set the geographical extents used in global analysis.
#'   Each extent should be in the form of c(Xmin, Xmax, Ymin, Ymax)
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
  } else{
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
