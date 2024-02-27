#' Raster for mapspam crop.
#'
#' get raster for crop in mapspam dataset
#' @param crp character. name of a crop. Case-insensitive.
#' @param africa Fetch the 2017 Mpaspam crop data of Africa. Default is FALSE.
#' @return SpatRaster
#' @details
#' See [geodata::spamCrops()] for supported crops.
#' @export
#' @examples
#' \donttest{
#' sp_rast("potato")
#' sp_rast("potato", TRUE)
#' }
#' @seealso
#' [geodata::spamCrops()]
#' [search_crop()]
#' @references
#' International Food Policy Research Institute, 2020.
#' Spatially-Disaggregated Crop Production Statistics Data in Africa South of the Sahara for 2017.
#' <doi: 10.7910/DVN/FSSKBW>, Harvard Dataverse, V2
#'
sp_rast <- function(crp, africa = FALSE) {

  f <- paste(tempfile(), ".tif", sep = "")

  sf_crp <- geodata::crop_spam(crop = crp,
                               var = "harv_area",
                               africa = africa,
                               path = f,
                               mode = "wb",
                               quiet = !getOption("verbose") == 0)

  harv_area_sp <- NULL

  if (africa) {

    for (i in names(sf_crp)) {
      if (stringr::str_detect(i, "harv_area_all")) {
        harv_area_sp <- sf_crp[[i]]
        break
      }
    }
  } else {

    for (s in terra::sources(sf_crp)) {
      if (endsWith(s, "_H.tif")) {
        harv_area_sp <- terra::rast(s)
        break
      }
    }
  }

  return(harv_area_sp)
}

.gen_url <- function(crop) {

  crp <- tolower(trimws(crop))
  crops <- geodata::spamCrops()
  if (!(crp %in% crops)) {
    stop("crop not in mapspam; see spamCrops()")
  }
  i <- which(crp == crops)[1]
  if (i > nrow(crops)) {
    i <- i - nrow(crops)
  }

  crp <- toupper(crops[i, 2])
  uri <- paste("https://geohabnet.s3.us-east-2.amazonaws.com/spamcrops/spam2010V2r0_global_H",
               crp,
               "A.tif",
               sep = "_")

  return(.download(uri))
}
