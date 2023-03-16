# dependencies and libraries ----------------------------------------------
library(rlist)
library(raster)
library(rgdal)
library(dismo)
library(expm)
library(igraph)
library(maptools)
library(rrcov)
library(splines)
library(rworldmap)
library(mapdata)
library(sp)
library(maps)
library(rgeos)
library(geosphere)
library(RColorBrewer)
library("colorspace") 
data("countriesLow")
library(viridis)
# external source ----------------------------------------------
source("CCRI.R", echo = FALSE)

# load config ----------------------------------------------
LoadConfig <- function()
{
  config <<- config::get(file = "configurations/parameters.yaml") 
}

# Setting color palettes----------------------------------------------------------

palette1 <- c( "#F4E156FF", "#F6D746FF", "#F8CD37FF", "#FAC329FF", "#FBB91EFF", "#FCAF13FF", 
                          "#FCA50BFF", "#FB9C06FF", "#FA9207FF", "#F8890CFF", "#F68013FF", "#F37819FF",
                          "#F06F20FF", "#EC6727FF", "#E85F2EFF", "#E25834FF", "#DD5139FF", "#D74B3FFF",
                          "#D04545FF", "#CA404AFF", "#C33B4FFF", "#BC3754FF", "#B43359FF", "#AC305EFF",
                          "#A42C60FF", "#9B2964FF", "#932667FF", "#922568FF", "#902568FF", "#8F2469FF",
                          "#8D2369FF", "#8C2369FF", "#8A226AFF", "#88226AFF", "#87216BFF", "#85216BFF",
                          "#84206BFF", "#82206CFF", "#801F6CFF", "#7F1E6CFF", "#7D1E6DFF", "#7C1D6DFF",
                          "#7A1D6DFF", "#781C6DFF", "#771C6DFF", "#751B6EFF", "#741A6EFF", "#721A6EFF",
                          "#71196EFF", "#6E196EFF", "#6D186EFF", "#6B186EFF", "#6A176EFF", "#68166EFF",
                          "#66166EFF", "#65156EFF", "#63156EFF", "#61136EFF", "#60136EFF", "#5E126EFF",
                          "#5C126EFF", "#5B126EFF", "#59106EFF", "#58106EFF", "#560F6DFF", "#540F6DFF",
                          "#530E6DFF", "#510E6CFF", "#500D6CFF", "#4D0D6CFF", "#4C0C6BFF", "#4A0C6BFF",
                          "#490B6AFF", "#470B6AFF", "#450A69FF", "#440A68FF", "#420A68FF", "#400A67FF",
                          "#3E0966FF", "#3D0965FF", "#3B0964FF", "#390963FF", "#380962FF", "#360961FF",
                          "#340A5FFF", "#320A5EFF", "#310A5CFF", "#2F0A5BFF", "#2D0B59FF", "#2B0B57FF",
                          "#290B55FF", "#280B53FF", "#250C51FF", "#240C4EFF", "#230C4BFF", "#200C49FF",
                          "#1F0C47FF", "#1D0C44FF", "#1C0C42FF", "#1A0C40FF", "#190C3DFF", "#170C3BFF",
                          "#150B38FF", "#150B36FF", "#130A33FF", "#110A31FF", "#11092EFF", "#0F092CFF",
                          "#0D082AFF", "#0C0827FF", "#0B0725FF", "#0A0723FF", "#090620FF", "#08051EFF",
                          "#07051CFF", "#060419FF", "#050418FF", "#040315FF", "#040312FF", "#030210FF",
                          "#02020EFF", "#02020CFF", "#02010AFF", "#010108FF", "#010106FF", "#010005FF",
                          "#000004FF", "#000004FF", "#000004FF")
                          
paldif <- diverge_hcl(12,h=c(128,330),c=98,l=c(65,90)) # palette for different map

# source level variables --------------------------------------------------
# Result of CCRI requests
result_index_list <- list()


# Utility functions -------------------------------------------------------
# Calculate crop harvest raster -------------------------------------------

getCropHarvestRaster <- function(crop_name)
{
  cropharvest <- geodata::crop_monfreda(crop = crop_name, path = tempdir())
  cropharvest <- raster(terra::sources(cropharvest))
  return(cropharvest)
}
getCropHarvestRasterSum <- function(crop_names)
{
  #crop_harvest1 + crop_harvest2 +.....n
  cropharvests <- lapply(crop_names, getCropHarvestRaster)
  Reduce('+', cropharvests)
}

# Initialize --------------------------------------------------
is_initialized <- FALSE
InitializeCroplandData <- function(cropharvestRaster, resolution, geo_scale, cutoff, aggregateMethod)
{
  ## Read cropland data in a .tif file and get data.frame lon/ lat /cropland density
  # TODO:do the above outside this function
  # cropharvest <- getCropHarvestRasterSum(crop_names)
  
  # aggregated resolution
  Resolution <- resolution # Set aggregated resolution, for example, assign 12 for 1 degree.
  
  #----------- aggregration -----------------------------
  cropharvestAGG <- aggregate(cropharvestRaster, fact = Resolution, fun=quote(aggregateMethod), na.action = na.omit)
  cropharvestAGGTM <- cropharvestAGG / Resolution / Resolution #TOTAL MEAN
  plot(cropharvestAGGTM, col = palette1)
  #----------- crop cropland area for the given extent ----------
  cropharvestAGGTM_crop <<- crop(cropharvestAGGTM, geo_scale)	
  plot(cropharvestAGGTM_crop, col = palette1)
  #----------- Extract cropland density data -----------------------
  CropValues <- getValues(cropharvestAGGTM_crop)
  CropValuesAzero <<- which(CropValues > cutoff) # find the cells with value > 0.0001
  cropValue <<- CropValues[CropValuesAzero]
  #----------- Extract xy corrdination for "povalue" cells ---------
  lon <<- NULL # xmin
  lat <<- NULL # ymax
  
  for(i in 1:length(CropValuesAzero)){
    temp <- extentFromCells(cropharvestAGGTM_crop, CropValuesAzero[i])
    AVxminO <- temp[1]
    lon <<- c(lon, AVxminO)
    AVymaxO <- temp[4]
    lat <<- c(lat, AVymaxO)
  }
  
  #---------------------------------------------------------------
  # Prepare arguments elements values for the CCRI funtions
  cropdata1 <- data.frame(lon, lat, cropValue)
  #adjustConstant <- 2 # to adjust the distance and make sure the distance >1
  latilongimatr <- cropdata1[ ,c(1:2)]# save the latitude and longitude as new matrix  
  #---- use Geosphere package, function distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
  dvse <- distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree
  latilongimatr <- as.matrix(latilongimatr)
  TemMat <- matrix(-999, nrow( latilongimatr),nrow(latilongimatr))
  
  for (i in 1:nrow(latilongimatr)) {
    TemMat[i, ] <- distVincentyEllipsoid(latilongimatr[i,], latilongimatr)/dvse
  }
  distance_matrix <<- TemMat
  # ```
  
  #Global cropland density map
  
  # ```{r, fig.width=20, fig.height=10, dpi=400}
  
  cropharvestAGGTM_crop1 <- crop(cropharvestAGGTM, extent(-180, 180, -60, 80))	
  zrWorldMean <- range(0.1, max(getValues(cropharvestAGGTM_crop1)))
  
  #Removing pixels outside boundary
  mean_index_raster_val <- getValues(cropharvestAGGTM_crop1)
  #structure(mean_index_raster_val)
  
  zeroID <- which(mean_index_raster_val == 0)
  cropharvestAGGTM[zeroID] <- NaN
  
  ZeroRaster <- raster("ZeroRaster.tif")
  CAM_Zero <- crop(ZeroRaster, extent(-180, 180, -60, 80))
  mean_index_raster <- disaggregate(cropharvestAGGTM_crop1, fact = c(Resolution, Resolution), method = '')
  mean_index_raster_CAM <- mean_index_raster + CAM_Zero
  
  #Plotting cropland density
  map_grey_background <- raster("map_grey_background.tif")
  
  map_grey_background_CAM <- crop(map_grey_background, extent(-180, 180, -60, 80))
  
  plot(map_grey_background_CAM, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Mean in crop area fraction:', crop), cex.main=1.6)
  
  plot(mean_index_raster_CAM, main = paste('Crop area density: ', crop), col = palette1, zlim = zrWorldMean, xaxt = 'n', yaxt = 'n', axes = F, box = F, add = TRUE)
  
  is_initialized = TRUE
}

validate_index_cal <- function(params)
{
  ready <- TRUE
  if(!is_initialized)
  {
    stop("Not initialized. Call initializeCroplandData()")
  }
  if(!is.list(beta_vals))
  {
    warning("argument is not a list")
    ready <- FALSE;
  }
  return(ready)
}

# inverse power law -------------------------------------------------------
CCRI_powerlaw <- function(beta_vals)
{
  if(!validate_index_cal(beta_vals))
    return(0)
  
  index_list <- lapply(beta_vals, CCRI_powerlaw_function, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
  result_index_list <- list.append(result_index_list, index_list)
  return(1)
}

# negative exponential function -------------------------------------------
CCRI_negative_exponential <- function(gamma_vals)
{
  if(!validate_index_cal(gamma_vals))
    return(0)
  
  index_list <- CCRI_negExponential_function(gamma00, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
  result_index_list <- list.append(result_index_list, index_list)
  return(1)
}


# Utility functions -------------------------------------------------------

GetGeographicScales <- function()
{
  performGlobalAnalysis <- config$`CCRI parameters`$Longitude_Latitude$Global
  geoScales = list()
  if(performGlobalAnalysis)
  {
    geoScales <- list(config$`CCRI parameters`$Longitude_Latitude$EastExt, config$`CCRI parameters`$Longitude_Latitude$WestExt)
  }
  customScales <- config$`CCRI parameters`$Longitude_Latitude$CustomExt
  if(!(is.null(customScales) || is.na(customScales) || length(customScales) == 0))
    geoScales <- list.append(geoScales, customScales)
  
  return(geoScales)
  #extent(as.numeric(unlist(geoScales)))
}
# Sensitivity analysis ----------------------------------------------------

SenstivityAnalysis <- function()
{
  geoScales <- GetGeographicScales()
  geoAreaExt <- extent(as.numeric(unlist(GetGeographicScales()[1]))) #list
  
  cropharvest <- getCropHarvestRasterSum(as.list(config$`CCRI parameters`$crop)) #list
  
  aggregateMethod <- config$`CCRI parameters`$aggregationStrategy #list
  
  # TODO: per cutoff or cropland threshold
  InitializeCroplandData(cropharvest, config$`CCRI parameters`$Resolution, 
                         geoAreaExt, config$`CCRI parameters`$cuttoff, aggregateMethod[1])
  
  InitializeCroplandData(cropharvest, config$`CCRI parameters`$Resolution, 
                         geoAreaExt, config$`CCRI parameters`$cuttoff, aggregateMethod[2])
  
  mean_index_raster <- sum (unlist(result_index_list)) / length(result_index_list)
  
  mean_index_raster_diff <- mean_index_raster
  Variance_mean_index_raster_west <- mean_index_raster
  
  mean_index_raster_val <- getValues(mean_index_raster)
  zeroId <- which(mean_index_raster_val == 0)
  mean_index_raster[zeroId] <- NaN
  
  plot(mean_index_raster, col = palette1, zlim= c(0, 1), main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado, resolution = 1'), cex.main=0.7)
  plot(countriesLow, add=TRUE)
}
#TODO execute each of the functions and add interactive mode