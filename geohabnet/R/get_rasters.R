#' Get rasters object from parameters
#'
#' Takes named list of hosts as an input. See host object in [get_parameters()] or [load_parameters()].
#' Function creates 2 raster object - one is a sum of all the crops specified under sources
#' and other using the provided raster file. See [get_crop_raster_fromtif()]
#' @param hosts List of hosts and values. It is synonym to Hosts object in parameters
#' @return List of rasters
#' @examples
#' # Get default rasters
#' \dontrun{
#' get_rasters(list(mapspam = c("wheat"), monfreda = c("avocado"), file = "some_raster.tif"))
#' }
#' @seealso [load_parameters()], [get_parameters()], [get_crop_raster_fromtif()], [get_cropharvest_raster()]
get_rasters <- function(hosts) {
  t_file <- hosts[["file"]]
  rasters <- list()
  if (!is.null(t_file) &&
    !is.na(t_file)) {
    rasters <- c(rasters, get_crop_raster_fromtif(t_file))
  }
  rasters <- c(rasters, get_cropharvest_raster_sum(hosts))
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
#' @return Raster object which is sum of all the individual crop rasters
#' @export
#' @examples
#' \dontrun{
#' get_cropharvest_raster_sum(list(monfreda = c("wheat", "barley"), mapspam = c("wheat", "potato")))
#' }
get_cropharvest_raster_sum <- function(crop_names) {
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
      cr <- get_cropharvest_raster(nams[i], j)
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
#' @return Raster object
#' @export
#' @examples
#' get_cropharvest_raster("avocado", "monfreda")
get_cropharvest_raster <- function(crop_name, data_source) {
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
#' @param path_to_tif TIF file
#' @return Raster object
#' @examples
#' \dontrun{
#' # Generate raster for usage
#' fp <- .get_helper_filepath("avocado")
#' get_crop_raster_fromtif(fp)
#' get_crop_raster_fromtif("avocado_HarvestedAreaFraction.tif")
#'
#' }
get_crop_raster_fromtif <- function(path_to_tif) {
  .validate_tif(path_to_tif)
  return(terra::rast(path_to_tif))
}

.validate_tif <- function(path_to_tif) {
  stopifnot(
    file.exists(path_to_tif),
    stringr::str_sub(path_to_tif, start = -4) == ".tif"
  )
}
