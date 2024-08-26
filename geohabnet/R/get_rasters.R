#' Get rasters object from parameters
#'
#' Takes named list of hosts as an input. See host object in [get_parameters()] or [load_parameters()].
#' This is also a wrapper of [crops_rast()].
#' Function creates 2 raster object - one is a sum of all the crops specified under sources
#' and other using the provided raster file. See [tiff_torast()]
#' @param hosts List of hosts and values. It is synonym to Hosts object in parameters
#' @return List of SpatRaster.
#' @examples
#' # Get default rasters
#' \dontrun{
#' get_rasters("some_raster.tif"))
#' }
#' @seealso [load_parameters()], [get_parameters()], [tiff_torast()], [cropharvest_rast()]
get_rasters <- function(host) {
  return(torast(host))
}

#' Get raster object from tif file
#'
#' This is a wrapper of [terra::rast()] and generates a raster object if provided with a TIF file.
#'
#' @param path_to_tif TIFF file. This is an encoding of map in raster format.
#' @return SpatRaster.
#' @examples
#' \donttest{
#' # Generate raster for usage
#' fp <- paste(tempfile(), ".tif", sep = "")
#' ret <- utils::download.file(
#' "https://geohabnet.s3.us-east-2.amazonaws.com/util-rasters/avocado_HarvestedAreaFraction.tif",
#' destfile = fp, method = "auto", mode = "wb")
#' tiff_torast(fp)
#' }
torast <- function(path_to_tif) {
  return(terra::rast(path_to_tif))
}

