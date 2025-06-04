library(yaml)

# Global constants --------------------------------------------------------

.kparameters_file_type <- "parameters"
.kzeroraster_fname <- "ZeroRaster.tif"
.kmapgreybackground_fname <- "map_grey_background.tif"

# Utility functions for calculating the habitat connectivity index -----------

.is_packed_rast <- function(x) {
  if (tolower(class(x)) == "packedspatraster") {
    TRUE
  } else {
    FALSE
  }
}
.unpack_rast_ifnot <- function(x) {
  if (.is_packed_rast(x)) {
    terra::unwrap(x)
  } else {
    x
  }
}

.stopifnot_sprast <- function(x) {
  stopifnot("Require argument of type SpatRaster" = methods::isClass(x, "SpatRaster"))
}

.host_map <- function(...) {
  if (length(...) == 1) {
    return(...[[1]])
  }
  # assumes only 2 elements, since only 2 spatial aggregation methods are supported,
  # i.e. sum and mean. Take mean if both are present, else take whatever.
  sum(...[[1]], ...[[2]], na.rm = TRUE) / 2
}

# Meta-programming approach with eval_tidy
.cal_dist <- function(latilongimatr, method) {

  #---- use Geosphere package, fun distVincentyEllipsoid() is used to calculate the distance, default distance is meter
  # reference of standard distance in meter for one degree

  method <- tolower(method)
  supported <- dist_methods()
  stopifnot("Distance strategy not supported. See dist_methods()\n" = method %in% supported)

  n <- nrow(latilongimatr)
  temp_matrix <- matrix(-999, n, n)

  f <- switch(method,
              "geodesic" = geosphere::distGeo,
              "vincentyellipsoid" = geosphere::distVincentyEllipsoid)

  dvse <- f(c(0, 0), cbind(1, 0))

  # Calculate the distances
  for (i in seq_len(n)) {
    temp_matrix[i, ] <- f(round(latilongimatr[i, ], 5), latilongimatr) / dvse
  }

  return(temp_matrix)
}

#' Get risk indices
#'
#' @description Get a habitat connectivity index for each unique combination of parameters from GeoRasters object.
#' @param ri GeoRasters object
#' @return List of habitat connectivity indices. If the `ri` is global, the list will contain two elements,
#' one for each hemisphere. e.g. `list(east = list(), west = list())`. If the `ri` is not global,
#' the list will contain a single element, e.g. `list()`.
#' @details
#' This function will unpack SpatRasters from GeoModel and thus is [future::future()] safe.
#'
#' @export
risk_indices <- function(ri) {
  stopifnot("Object is not of type GeoRasters" = class(ri) == "GeoRasters")
  .ew_split <- function() {
    ew_indices <- list(list(), list())
    names(ew_indices) <- c(STR_EAST, STR_WEST)
    
    cnt <- 0
    for (grast in ri$global_rast) {
      for (mod in grast$east) {
        ew_indices[[STR_EAST]] <- c(ew_indices[[STR_EAST]], .unpack_rast_ifnot(mod@index))
      }
      for (mod in grast$west) {
        ew_indices[[STR_WEST]] <- c(ew_indices[[STR_WEST]], .unpack_rast_ifnot(mod@index))
      }
    }
    return(ew_indices)
  }

  if (ri$global) {
    #east-west split
    .ew_split()
  } else {
    unlist(lapply(
      ri$rasters,
      FUN = function(x) {
        terra::rast(x@index)
      }
    ), recursive = FALSE)
  }
}

.to_ext <- function(geoscale) {
  return(terra::ext(geoscale))
}

.showmsg <- function(...) {
  if (getOption("verbose")) {
    message(...)
  }
}

.valid_vector_input <- function(vector_to_check) {
  if (!is.vector(vector_to_check) || length(vector_to_check) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

.download <- function(uri) {
  f <- paste(tempfile(), ".tif", sep = "")
  stopifnot("download failed " = utils::download.file(uri,
                                                      destfile = f,
                                                      method = "auto",
                                                      mode = "wb",
                                                      quiet = getOption("verbose")) == 0)
  return(f)
}

.utilrast <- function(fname) {

  stopifnot("Internal error. TIFF file not available." =
            fname %in% c(.kzeroraster_fname, .kmapgreybackground_fname))
  return(system.file(fname,
                     package = utils::packageName(),
                     mustWork = TRUE))
}

.apply_agg <- function(rast,
                       reso,
                       method) {

  return(terra::aggregate(rast,
                          fact = reso,
                          fun = method,
                          na.rm = TRUE,
                          na.action = stats::na.omit))
}

.onLoad <- function(libname, pkgname) {

  # this will cleanup previously created config and replace it with new one
  # in case, if it's a first installation, it will simply copy the config.
  reset_params()

  .utilrast <<- memoise::memoise(.utilrast)
  .cal_mgb <<- memoise::memoise(.cal_mgb)
}

.get_helper_filepath <- function(file_type) {
  file_path <- if (file_type == "parameters") {
    system.file("parameters.yaml",
                package = "geohabnet",
                mustWork = TRUE)
  } else {
    .utilrast(file_type)
  }
  return(file_path)
}

# Recursively check the structure of nested sections
.check_nested_structure <- function(existing_section, provided_section,
                                    section_name) {
  if (!identical(names(existing_section), names(provided_section))) {
    stop(paste("The", section_name, "section in the provided YAML file does not
               match the structure of the existing YAML file."))
  }

  # Check the structure of nested sections
  for (key in names(existing_section)) {
    if (is.list(existing_section[[key]]) && is.list(provided_section[[key]])) {
      .check_nested_structure(
        existing_section[[key]], provided_section[[key]],
        paste(section_name, key, sep = " > ")
      )
    }
  }
}

.check_yaml_structure <- function(existing_yaml_file, provided_yaml_file) {
  # Read the existing YAML file
  existing_yaml <- yaml::yaml.load_file(existing_yaml_file)

  # Read the provided YAML file
  provided_yaml <- yaml::yaml.load_file(provided_yaml_file)

  # Compare the structure of the YAML files
  if (!identical(names(existing_yaml), names(provided_yaml))) {
    stop("The provided YAML file does not match the structure of the existing
         YAML file.")
  }
  # Check the structure of nested sections
  for (key in names(existing_yaml)) {
    if (is.list(existing_yaml[[key]]) && is.list(provided_yaml[[key]])) {
      .check_nested_structure(existing_yaml[[key]], provided_yaml[[key]], key)
    }
  }
  # If the function reaches this point, the YAML structures match
  return(TRUE)
}

.param_hosts <- function(param_config = load_parameters()) {
  return(paste(param_config$`CCRI parameters`$Hosts, collapse = ", "))
}

.cal_mgb <- function(geoscale, isglobal) {
  # calculate map grey background
  map_grey_background <- terra::rast(.get_helper_filepath(.kmapgreybackground_fname))
  map_grey_background_ext <- if (isglobal == FALSE) {
    terra::crop(map_grey_background, .to_ext(geoscale))
  } else {
    map_grey_background
  }
  return(map_grey_background_ext)
}

.cal_zerorast <- function(in_rast) {

  # Create zero_rast with the same dimensions as in_rast
  zero_rast <- terra::rast(.get_helper_filepath(.kzeroraster_fname))
  # Set extent of zero_rast to match in_rast
  zero_rast <- terra::resample(zero_rast, in_rast, threads = TRUE)

  return(zero_rast)
}

.get_palette <- function() {
  palette1 <- viridisLite::viridis(n=100, option = "inferno", direction = -1, begin = 0.05, end = 0.95)
  return(palette1)
}

.get_palette_for_diffmap <- function() {

  paldif <- viridisLite::viridis(80, option = "cividis", direction = -1, alpha = 0.95)
  return(paldif)
}

.write_yaml <- function(yaml_obj, file_path) {
  # Validate YAML object
  if (is.null(yaml_obj) || !is.list(yaml_obj)) {
    stop("Invalid YAML object. Please provide a non-null list as the YAML object.")
  }

  # Validate file path and type
  if (!is.character(file_path) || !grepl("\\.yaml$|\\.yml$", file_path, ignore.case = TRUE)) {
    stop("Invalid file path. Please provide a valid YAML file path with '.yaml' or '.yml' extension.")
  }

  # Write YAML to file
  tryCatch(
    yaml::write_yaml(yaml_obj, file_path),
    error = function(e) {
      stop("Error writing YAML to file:", conditionMessage(e))
    }
  )

  .showmsg("YAML object successfully written to file: ", file_path)
}

#' Distance methods supported
#'
#' Contains supported strategies to calculate distance between two points.
#' Use of one of two methods in [sean()] or [sensitivity_analysis()].
#' @return vector
#' @export
#'
#' @examples
#' dist_methods()
#'
dist_methods <- function() {
  return(c("geodesic", "vincentyellipsoid"))
}
