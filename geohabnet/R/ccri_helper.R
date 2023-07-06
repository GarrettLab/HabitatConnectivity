library(yaml)

# Global constants --------------------------------------------------------

.kparameters_file_type <- "parameters"
.kzeroraster_file_type <- "zero"
.kmapgreybackground_file_type <- "map_grey"

# utility functions for CCRI ----------------------------------------------

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
  stopifnot("dowload failed " = utils::download.file(uri, destfile = f, method = "auto") == 0)
  return(f)
}

.utilrast <- function(typ) {
  return(.download(.utilrast_uri(typ)))
}

.onLoad <- function(libname, pkgname) {
  .utilrast <<- memoise::memoise(.utilrast)
}

.get_helper_filepath <- function(file_type) {
  file_path <- if (file_type == "parameters") {
     system.file("parameters.yaml",
      package = "geohabnet",
      mustWork = TRUE
    )
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

.get_cs_host_names <- function(param_config = load_parameters()) {
  return(paste(param_config$`CCRI parameters`$Crops, collapse = ", "))
}

.get_map_grey_background_extent <- function(geoscale) {
  map_grey_background <- raster::raster(.get_helper_filepath(.kmapgreybackground_file_type))
  map_grey_background_ext <- raster::crop(map_grey_background, geoscale)
  return(map_grey_background_ext)
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
  paldif <- colorspace::diverge_hcl(51, c = 100, l = c(20, 90), power = 1.3)
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

  message("YAML object successfully written to file:", file_path)
}

.get_cropharvest_raster_helper <- function(crop_name, data_source) {
  if (data_source == "monfreda") {
    geodata::crop_monfreda(crop = crop_name, path = tempdir(), var = "area_f")
  } else if (data_source == "spam") {
    sp_rast(crp = crop_name) / 10000
  } else {
    stop(paste("Encountered unsupported source: ", data_source))
  }
}

#' Get supported sources of crops
#' When provided, [get_cropharvest_raster()] will
#' look for cropland data in this specific source.
#' @returns return vector of supported sources.
#' Also used as a lookup to find get raster object.
#' @export
#' @examples
#' # Get currently supported sources
#' get_supported_sources()
get_supported_sources <- function() {
  return(c("monfreda", "spam"))
}
