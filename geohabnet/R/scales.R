
.coords <- function() {
  return(c("Xmin", "Xmax", "Ymin", "Ymax"))
}

#' Only meant to global variables
#' @keywords internal
scales <- new.env(parent = emptyenv())
scales$geast <- c(-24, 180, -58, 60)
names(scales$geast) <- .coords()

scales$gwest <- c(-140, 34, -58, 60)
names(scales$gwest) <- .coords()

#' Global geographical extent
#'
#'   See geographical extents used in global analysis.
#'   Returns eastern and western hemisphere extents.
#'   Each extent is in the form of c(Xmin, Xmax, Ymin, Ymax).
#' @export
#' @seealso [set_global_scales()]
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
#' @param value list. Named list of extents for eastern and western hemisphere. See usage.
#' @export
#' @examples
#' \dontrun{
#' set_global_scales(list(east = c(Xmin = -24, Xmax = 180, Ymin = -58, Ymax = 60),
#' west = c(Xmin = -140, Xmax = -34, Ymin = -58, Ymax = 60)))
#' }
#' @seealso
#' [global_scales()]
#' [terra::ext()]
set_global_scales <- function(value) {

  east <- as.vector(value[[STR_EAST]])
  west <- as.vector(value[[STR_WEST]])
  stopifnot("Not a valid east coordinate" = is.numeric(east), is.vector(east), length(east) == 4)
  stopifnot("Not a valid west coordinate" = is.numeric(west), is.vector(west), length(west) == 4)

  coords <- .coords()

  scales$geast <- east
  names(east) <- coords

  scales$gwest <- west
  names(east) <- coords

  value <- c(east, west)

  return(invisible(value))
}

#' Get geographical scales from the parameters
#'
#'   This function returns a list of geographical scales set in global and custom extent in `parameters.yaml`.
#'   If `global` is `TRUE`, the `CustomExt` is ignored.
#' @return A list of geographical scales
#' @export
#' @details
#' The geographical scales provided are converted using [terra::ext()].
#' Although, no particular CRS is enforced,
#' we recommend using WGS84(Google maps) or EPSG:32611.
#' @references Hijmans R (2023). _terra: Spatial Data Analysis_. R package version 1.7-46,
#' <https://CRAN.R-project.org/package=terra>.
#'
geoscale_param <- function() {
  .loadparam_ifnull()
  xf <- the$parameters_config$`CCRI parameters`$GeoExtent$global
  par_ext <- c(the$parameters_config$`CCRI parameters`$GeoExtent$customExt$Xmin,
               the$parameters_config$`CCRI parameters`$GeoExtent$customExt$Xmax,
               the$parameters_config$`CCRI parameters`$GeoExtent$customExt$Ymin,
               the$parameters_config$`CCRI parameters`$GeoExtent$customExt$Ymax)
  names(par_ext) <- .coords()

  if (as.logical(xf) == FALSE) {
    stopifnot("Geographical scale missing in parameters " =
                length(par_ext) == 4)
  }

  return(par_ext)
}

.validate_scales <- function(geoscale = geoscale_param()) {
  msg <- "Set all the points(Xmin, Xmax, Ymin, Ymax"
  stopifnot(msg = length(geoscale) == 4)

  if (length(names(geoscale)) > 0) {
    stopifnot(msg = length(names(geoscale)) == 4)
    stopifnot(msg = isTRUE(all(names(geoscale) %in% .coords())))
  }
  names(geoscale) <- .coords()
  invisible()
}
