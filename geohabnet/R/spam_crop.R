#' raster for mapspam crop.
#'
#' get raster for crop in mapspam dataset
#' @param crp character. name of a crop. Case-insensitive.
#' @return spatRaster
#' @details
#' See [geodata::spamCrops()] for supported crops.
#' @export
#' @examples
#' \dontrun{
#' sp_rast("rice")
#' }
#' @seealso
#' [geodata::spamCrops()]
#' [search_crop()]
#' @references
#' International Food Policy Research Institute, 2020.
#' Spatially-Disaggregated Crop Production Statistics Data in Africa South of the Sahara for 2017.
#' <doi: 10.7910/DVN/FSSKBW>, Harvard Dataverse, V2
#'
sp_rast <- function(crp) {
  return(terra::rast(.gen_url(crp)))
}

.gen_url <- function(crop) {

  crp <- tolower(trimws(crop))
  crops <- geodata::spamCrops()
  if (!(crp %in% crops)) {
    stop("crop not in mapspam; see spamCrops()")
  }
  i <- which(crp == crops)[1]
  if (i > nrow(crops))
  i <- i - nrow(crops)
  crp <- toupper(crops[i, 2])
  uri <- paste("https://geohabnet.s3.us-east-2.amazonaws.com/spamcrops/spam2010V2r0_global_H",
               crp,
               "A.tif",
               sep = "_")

  return(.download(uri))
}
