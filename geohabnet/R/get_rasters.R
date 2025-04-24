#' Get rasters object from parameters
#' See host object in [get_parameters()] or [load_parameters()].
#' @param host SpatRaster. It is synonym to Hosts object in parameters.
#' This is a wrapper to [terra::rast()] and generates a raster object if provided with a TIF file.
#' @return List of SpatRaster.
#' @examples
#' # Get default rasters
#' \dontrun{
#' get_rasters("some_raster.tif")
#' }
#' @seealso [load_parameters()], [get_parameters()]
get_rasters <- function(host) {
  return(terra::rast(host))
}


