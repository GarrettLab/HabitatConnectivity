scales <- new.env(parent = emptyenv())
scales$geast <- c(-24, 180, -58, 60)
scales$gwest <- c(-140, -34, -58, 60)

#' Global geographical extent
#'
#'   See geographical extents used in global analysis.
#'   Returns eastern and western hemisphere extents.
#'   Each extent is in the form of c(Xmin, Xmax, Ymin, Ymax).
#'   @export
global_scales <- function() {
  sc <- list(scales$geast, scales$gwest)
  names(sc) <- c(STR_EAST, STR_WEST)
  return(sc)
}

.global_ext <- function(scales = global_scales()) {
  return(scales[[STR_EAST]] + scales[[STR_WEST]])
}

#' Set global geographical extent
#'
#'   Set the geographical extents used in global analysis.
#'   Each extent should be in the form of c(Xmin, Xmax, Ymin, Ymax)
#' @param east vector. The eastern hemisphere extent.
#' @param west vector. The western hemisphere extent.
#' @export
#' @seealso
#' [global_scales()]
#' [terra::ext()]
`global_scales<-` <- function(east, west) {

  if (missing(east)) {
    stop("Please provide the eastern hemisphere extent.")
  }
  if (missing(west)) {
    stop("Please provide the western hemisphere extent.")
  }
  stopifnot("Not a valid east coordinate" = is.vector(east), length(east) == 4)
  stopifnot("Not a valid west coordinate" = is.vector(west), length(west) == 4)

  scales$geast <- east
  scales$gwest <- west
  return(invisible(global_scales()))
}

#' Get geographical scales from the parameters
#'
#'   This function returns a list of geographical scales set in global and custom extent in `parameters.yaml`.
#'   If `global` is `TRUE`, the `CustomExt` is ignored.
#' @return A list of geographical scales
#' @export
geoscale_param <- function() {
  .loadparam_ifnotnull()
  xf <- the$parameters_config$`CCRI parameters`$GeoExtent$global
  if (as.logical(xf) == FALSE) {
    stopifnot("Geographical missing in parameters " =
                length(the$parameters_config$`CCRI parameters`$GeoExtent$customExt) == 4)
  }
  return(as.vector(the$parameters_config$`CCRI parameters`$GeoExtent$customExt))
}
