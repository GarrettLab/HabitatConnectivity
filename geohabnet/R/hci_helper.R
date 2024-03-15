library(yaml)

# Global constants --------------------------------------------------------

.kparameters_file_type <- "parameters"
.kzeroraster_file_type <- "zero"
.kmapgreybackground_file_type <- "map_grey"

# utility functions for CCRI ----------------------------------------------

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
  # assumes only 2 elements, since only 2 agg methods are supported,
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
#' @description Get risk indices from GeoRasters object.
#' @param ri GeoRasters object
#' @return List of risk indices. If the `ri` is global, the list will contain two elements,
#' one for each hemisphere. e.g. `list(east = list(), west = list())`. If the `ri` is not global,
#' the list will contain a single element, e.g. `list()`.
#' @details
#' This function will unpack SpatRasters from GeoMadel and thus is [future::future()] safe.
#'
#' @export
risk_indices <- function(ri) {
  stopifnot("Object is not of type GeoRasters" = class(ri) == "GeoRasters")
  .ew_split <- function() {
    ew_indices <- list(list(), list())
    names(ew_indices) <- c(STR_EAST, STR_WEST)

    for (grast in ri$global_rast) {
      for (mod in grast$east) {
        ew_indices[[STR_EAST]] <- c(ew_indices[[STR_EAST]], terra::rast(mod$index))
      }
      for (mod in grast$west) {
        ew_indices[[STR_WEST]] <- c(ew_indices[[STR_WEST]], terra::rast(mod$index))
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
        terra::rast(x$index)
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

.utilrast_locs <- function() {

  base_uri <- "https://geohabnet.s3.us-east-2.amazonaws.com/util-rasters/"
  full_uri <- function(name) {
    paste(base_uri, name, sep = "")
  }

  locs <- matrix(c(.kzeroraster_file_type, full_uri("ZeroRaster.tif"),
                   .kmapgreybackground_file_type, full_uri("map_grey_background.tif"),
                   "avocado", full_uri("avocado_HarvestedAreaFraction.tif")), # only for testing purpose
                 ncol = 2, byrow = TRUE)
  colnames(locs) <- c("raster", "uri")
  return(locs)
}

.utilrast_uri <- function(typ) {
  locs <- .utilrast_locs()
  stopifnot("internal error - wrong type" = typ %in% locs[, 1])
  idx <- which(locs[, 1] == typ)
  return(locs[idx, ][[2]])
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

.utilrast <- function(typ) {
  return(.download(.utilrast_uri(typ)))
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
  #.apply_agg <<- memoise::memoise(.apply_agg)
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
  map_grey_background <- terra::rast(.get_helper_filepath(.kmapgreybackground_file_type))
  map_grey_background_ext <- if (isglobal == FALSE) {
    terra::crop(map_grey_background, .to_ext(geoscale))
  } else {
    map_grey_background
  }
  return(map_grey_background_ext)
}

.cal_zerorast <- function(in_rast, reso) {

  # Create zero_rast with the same dimensions as in_rast
  zero_rast <- terra::rast(.get_helper_filepath(.kzeroraster_file_type))
  # Set extent of zero_rast to match in_rast
  zero_rast <- terra::resample(zero_rast, in_rast, threads = TRUE)

  return(zero_rast)
}


.get_palette <- function() {
  palette1 <- c(
    "#F4E156FF", "#F6D746FF", "#F8CD37FF", "#FAC329FF", "#FBB91EFF",
    "#FCAF13FF", "#FCA50BFF", "#FB9C06FF", "#FA9207FF", "#F8890CFF",
    "#F68013FF", "#F37819FF", "#F06F20FF", "#EC6727FF", "#E85F2EFF",
    "#E25834FF", "#DD5139FF", "#D74B3FFF", "#D04545FF", "#CA404AFF",
    "#C33B4FFF", "#BC3754FF", "#B43359FF", "#AC305EFF", "#A42C60FF",
    "#9B2964FF", "#932667FF", "#922568FF", "#902568FF", "#8F2469FF",
    "#8D2369FF", "#8C2369FF", "#8A226AFF", "#88226AFF", "#87216BFF",
    "#85216BFF", "#84206BFF", "#82206CFF", "#801F6CFF", "#7F1E6CFF",
    "#7D1E6DFF", "#7C1D6DFF", "#7A1D6DFF", "#781C6DFF", "#771C6DFF",
    "#751B6EFF", "#741A6EFF", "#721A6EFF", "#71196EFF", "#6E196EFF",
    "#6D186EFF", "#6B186EFF", "#6A176EFF", "#68166EFF", "#66166EFF",
    "#65156EFF", "#63156EFF", "#61136EFF", "#60136EFF", "#5E126EFF",
    "#5C126EFF", "#5B126EFF", "#59106EFF", "#58106EFF", "#560F6DFF",
    "#540F6DFF", "#530E6DFF", "#510E6CFF", "#500D6CFF", "#4D0D6CFF",
    "#4C0C6BFF", "#4A0C6BFF", "#490B6AFF", "#470B6AFF", "#450A69FF",
    "#440A68FF", "#420A68FF", "#400A67FF", "#3E0966FF", "#3D0965FF",
    "#3B0964FF", "#390963FF", "#380962FF", "#360961FF", "#340A5FFF",
    "#320A5EFF", "#310A5CFF", "#2F0A5BFF", "#2D0B59FF", "#2B0B57FF",
    "#290B55FF", "#280B53FF", "#250C51FF", "#240C4EFF", "#230C4BFF",
    "#200C49FF", "#1F0C47FF", "#1D0C44FF", "#1C0C42FF", "#1A0C40FF",
    "#190C3DFF", "#170C3BFF", "#150B38FF", "#150B36FF", "#130A33FF",
    "#110A31FF", "#11092EFF", "#0F092CFF", "#0D082AFF", "#0C0827FF",
    "#0B0725FF", "#0A0723FF", "#090620FF", "#08051EFF", "#07051CFF",
    "#060419FF", "#050418FF", "#040315FF", "#040312FF", "#030210FF",
    "#02020EFF", "#02020CFF", "#02010AFF", "#010108FF", "#010106FF",
    "#010005FF", "#000004FF", "#000004FF", "#000004FF"
  )
  return(palette1)
}

.get_palette_for_diffmap <- function() {

  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  paldif <- viridisLite::viridis(80, direction = 1, alpha = 0.9)
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

.get_cropharvest_raster_helper <- function(crop_name, data_source) {
  if (data_source == "monfreda") {
    geodata::crop_monfreda(crop = crop_name, path = tempdir(), var = "area_f")
  } else if (data_source %in% c("mapspam2010", "mapspam2017Africa")) {
    x <- if (data_source == "mapspam2010") {
      sp_rast(crp = crop_name)
    } else {
      sp_rast(crp = crop_name, africa = TRUE)
    }
    x * 0.0001
  } else {
    stop(paste("unsupported source: ", data_source))
  }
}

#' Get supported sources of crops
#'
#' When provided, [cropharvest_rast()] will
#' look for cropland data in this specific source.
#' @returns Vector of supported sources.
#' Also used as a lookup to find get raster object.
#' @export
#' @examples
#' # Get currently supported sources
#' supported_sources()
supported_sources <- function() {
  return(c(monfreda(), mapspam()))
}

#' Supported sources for monfreda
#' 
#' @export
monfreda <- function() {
  return(c("monfreda"))
}

#' Supported sources for Mapspam
#' 
#' @export
mapspam <- function() {
  return(c("mapspam2010", "mapspam2017Africa"))
}

#' Search for crop
#'
#' It returns the dataset sources in which crop data is available.
#' It's a wrapper around [geodata::spamCrops()] and [geodata::monfredaCrops()]
#' @param name name of crop
#' @return Logical. Sources iin crop data is available.
#' @export
#' @examples
#' search_crop("coffee")
#' search_crop("wheat")
#' \donttest{
#' search_crop("jackfruit")
#' }
#'
#' @seealso [supported_sources()]
search_crop <- function(name) {
  crp <- tolower(trimws(name))

  funs <- c("monfreda", "spam")
  srcs <- character(0)

  for (src in funs) {
    f <- paste0("geodata::", src, "Crops()")
    res <- rlang::eval_tidy(rlang::parse_expr(f))
    if (src == "monfreda") {
      res <- res$name
    }
    if (crp %in% res) {
      srcs <- c(srcs, src)
    }
  }

  srcs <- if (is.null(srcs) || length(srcs) < 1) {
    "Crop not present in supported sources."
  } else {
    srcs
  }

  return(srcs)
}

#' Distance methods supported
#'
#' Contains supported strategies to calculate distance between two points.
#' Use of one the methods in [sean()] or [sensitivity_analysis()].
#' @return vector
#' @export
#'
#' @examples
#' dist_methods()
#'
dist_methods <- function() {
  return(c("geodesic", "vincentyellipsoid"))
}
