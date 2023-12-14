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
#' get_rasters(list(mapspam = c("wheat"), monfreda = c("avocado"), file = "some_raster.tif"))
#' }
#' @seealso [load_parameters()], [get_parameters()], [tiff_torast()], [cropharvest_rast()]
get_rasters <- function(hosts) {
  t_file <- hosts[["file"]]
  rasters <- list()
  if (!is.null(t_file) && !is.na(t_file)) {
    rasters <- c(rasters, tiff_torast(t_file))
  }
  rasters <- c(rasters, crops_rast(hosts))
  return(rasters)
}


#' @title Get sum of rasters for individual crops
#'
#' @description
#' Takes crop names and returns raster object which is sum of raster of individual crops.
#' Currently, only supports crops listed in
#' [geodata::monfredaCrops()], [geodata::spamCrops()]
#' If crop is present in multiple sources, then their mean is calculated.
#' @param crop_names A named list of source along with crop names
#' @return SpatRaster. Raster object which is sum of all the individual crop raster
#' @export
#' @examples
#' \donttest{
#' crops_rast(list(monfreda = c("wheat", "barley"), mapspam = c("wheat", "potato")))
#' }
crops_rast <- function(crop_names) {
  if (!is.list(crop_names) || length(crop_names) == 0) {
    stop("Input 'crop_names' must be a non-empty list of crop names.")
  }

  # output: list("wheat" = c("monfreda", "mapspam"), "barley" = c("monfreda"), "potato" = c("mapspam"))
  crops <- list()

  for (src in get_supported_sources()) {
    for (crop_name in crop_names[[src]]) {
      crops[[crop_name]] <- c(crops[[crop_name]], src)
    }
  }

  # calculate sum of rasters

  # iterate named lists
  # crop names
  nams <- names(crops)
  cropharvests <- list()
  for (i in seq_along(crops)) {
    single_crop_rasters <- list()
    for (j in crops[[i]]) {
      cr <- cropharvest_rast(nams[i], j)
      single_crop_rasters <- c(single_crop_rasters, cr)
    }
    len_scr <- length(single_crop_rasters)
    if (len_scr > 1) {
      cropharvests <- c(cropharvests, terra::app(terra::rast(single_crop_rasters), fun = sum, na.rm = TRUE) / len_scr)
    } else {
      cropharvests <- c(cropharvests, single_crop_rasters)
    }
  }

  return(Reduce("+", cropharvests))
}

#' @title Get raster object for crop
#' @description Get cropland information in a form of raster object from data source for crop
#' @param crop_name Name of the crop
#' @param data_source Data source for cropland information
#' @return Raster.
#' @export
#' @examples
#' \donttest{
#' cropharvest_rast("avocado", "monfreda")
#' }
#'
cropharvest_rast <- function(crop_name, data_source) {
  # supported sources
  sources <- get_supported_sources()
  if (!(data_source %in% sources)) {
    stop(paste("data source: ", data_source, " is not supported"))
  }
  cropharvest_r <- .get_cropharvest_raster_helper(crop_name = crop_name, data_source = data_source)

  return(cropharvest_r)
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
tiff_torast <- function(path_to_tif) {
  .validate_tif(path_to_tif)
  return(terra::rast(path_to_tif))
}

.validate_tif <- function(path_to_tif) {
  file_extension <- stringr::str_sub(path_to_tif, start = -4)
  stopifnot(
    file.exists(path_to_tif),
    file_extension %in% c(".tif", ".TIF")
  )
}
