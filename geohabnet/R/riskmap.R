#' RiskMap class
#' 
#' @description 
#' An S4 class representing resulting maps from specific operation types.
#' Handles automated defaults for missing or NULL spatial data.
#' 
#' @export
RiskMap <- setClass(
  "RiskMap",
  slots = list(
    map  = "character",
    riid = "ANY", # SpatRaster
    spr  = "ANY", # SpatRaster (result of plotting/disaggregation)
    fp   = "character"
  ),
  prototype = list(
    map  = NA_character_,
    riid = NULL,
    spr  = NULL,
    fp   = NA_character_
  )
)

# --- Initialize Method ---
setMethod("initialize", "RiskMap", function(.Object, ...) {
  args <- list(...)
  
  # 1. Handle 'map' (Operation type name)
  .Object@map <- if (!is.null(args$map)) as.character(args$map) else "Undefined"
  
  # 2. Handle 'riid' (Original Raster)
  .Object@riid <- if (!is.null(args$riid)) args$riid else NULL
  
  # 3. Handle 'spr' (Processed/Plotting Raster)
  .Object@spr <- if (!is.null(args$spr)) args$spr else NULL
  
  # 4. Handle 'fp' (File path)
  if (!is.null(args$fp) && !is.na(args$fp)) {
    .Object@fp <- as.character(args$fp)
  } else {
    .Object@fp <- "No file generated"
  }
  
  return(.Object)
})

# --- Validity Check (Optional but Recommended) ---
setValidity("RiskMap", function(object) {
  errors <- character()
  
  if (length(object@map) > 1) {
    errors <- c(errors, "The 'map' slot must be a single character string.")
  }
  
  if (length(object@fp) > 1) {
    errors <- c(errors, "The 'fp' slot must be a single file path string.")
  }
  
  if (length(errors) == 0) TRUE else errors
})