# dependencies and libraries ----------------------------------------------
library(rlist)
library(raster)
library(dismo) #TODO: check if it's required
library(igraph)
library(rworldmap)
library(geosphere)
library("colorspace") 
library(viridis) #TODO: check if it's required
library(terra) # TODO: check raster vs terra
library(this.path)
library(geodata)
library(config)
library(here)

#' @exportPattern ^[^\\.].*

data("countriesLow")

# Global constants --------------------------------------------------------

.kConfigFileFullPath <-  system.file("parameters.yaml", package = "geohabnet", mustWork = TRUE)
.kZeroRasterFilePath <- system.file("tifs", "ZeroRaster.tif", package = "geohabnet", mustWork = TRUE)
.kMapGreyBackGroundTifFilePath <- system.file("tifs", "map_grey_background.tif", package = "geohabnet", mustWork = TRUE)
.kHelperFilePath <- "R/Utilities/ccri_helper.R"

# Load helper functions ---------------------------------------------------

source(paste(here::here(), "R/Utilities/strings.R", sep = "/"))
source(paste(here::here(), .kHelperFilePath, sep = "/"))

# load parameters config ----------------------------------------------

LoadParameters <- function(filePath = .kConfigFileFullPath)
{
  cat(paste("hh ", filePath))
  config <<- config::get(file = filePath) 
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


# Utility functions -------------------------------------------------------
# Calculate crop harvest raster -------------------------------------------

getCropHarvestRaster <- function(crop_name) {
  cropharvest <- geodata::crop_monfreda(crop = crop_name, path = tempdir(), var = "area_f")
  cropharvest <- raster::raster(terra::sources(cropharvest))
  return(cropharvest)
}

getCropHarvestRasterSum <- function(crop_names)
{
  if(!is.vector(crop_names) || length(crop_names) == 0) {
    stop("Input 'crop_names' must be a non-empty vector of crop names.")
  }
  
  #crop_harvest1 + crop_harvest2 +.....n
  cropharvests <- lapply(crop_names, getCropHarvestRaster)
  Reduce('+', cropharvests)
}

.extract_lon_lat <- function(CropValuesAzero, cropharvestAGG_crop) {
  #----------- Extract xy corrdination for "povalue" cells ---------
  lon <<- NULL # xmin
  lat <<- NULL # ymax
  
  for(i in 1:length(CropValuesAzero)){
    temp <- raster::extentFromCells(cropharvestAGG_crop, CropValuesAzero[i])
    AVxminO <- temp[1]
    lon <<- c(lon, AVxminO)
    AVymaxO <- temp[4]
    lat <<- c(lat, AVymaxO)
  }
  return(list(longitude = lon, lattitude = lat))
}

#----------- Extract cropland density data -----------------------
.extract_cropland_density <- function(cropharvestAGG_crop, hostDensityThreshold) {
  CropValues <- raster::getValues(cropharvestAGG_crop)
  CropValuesAzero <<- which(CropValues > hostDensityThreshold) # find the cells with value > 0.0001
  cropValue <<- CropValues[CropValuesAzero]
  return(.extract_lon_lat(CropValuesAzero, cropharvestAGG_crop))
}

# Initialize --------------------------------------------------
is_initialized <<- FALSE

#Global cropland density map---------------------------------------------------------------
# Only when user has enabled global analysis
GlobalAnalysis <- function()
{
  
  # ```{r, fig.width=20, fig.height=10, dpi=400}
  
  cropharvestAGGTM_crop1 <- crop(cropharvestAGGTM, raster::extent(-180, 180, -60, 80))	
  zrWorldMean <- range(0.1, max(raster::getValues(cropharvestAGGTM_crop1)))
  
  #Removing pixels outside boundary
  mean_index_raster_val <- raster::getValues(cropharvestAGGTM_crop1)
  #structure(mean_index_raster_val)
  
  zeroID <- which(mean_index_raster_val == 0)
  cropharvestAGGTM[zeroID] <- NaN
  
  ZeroRaster <- raster(.kZeroRasterFilePath)
  CAM_Zero <- raster::crop(ZeroRaster, raster::extent(-180, 180, -60, 80))
  mean_index_raster <- disaggregate(cropharvestAGGTM_crop1, fact = c(Resolution, Resolution), method = '')
  mean_index_raster_CAM <- mean_index_raster + CAM_Zero
  
  #Plotting cropland density
  map_grey_background <- raster(.kMapGreyBackGroundTifFilePath)
  
  map_grey_background_CAM <- raster::crop(map_grey_background, raster::extent(-180, 180, -60, 80))
  
  plot(map_grey_background_CAM, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Mean in crop area fraction:', crop), cex.main=1.6)
  
  plot(mean_index_raster_CAM, main = paste('Crop area density: ', crop), col = palette1, zlim = zrWorldMean, xaxt = 'n', yaxt = 'n', axes = F, box = F, add = TRUE)
}

InitializeCroplandData <- function(cropharvestRaster, resolution, geo_scale, hostDensityThreshold, aggMethod) {
  ## Read cropland data in a .tif file and get data.frame lon/ lat /cropland density
  # cropharvest <- getCropHarvestRasterSum(crop_names)
  # aggregated resolution
  Resolution <<- resolution # Set aggregated resolution, for example, assign 12 for 1 degree.
  #----------- aggregration -----------------------------
  cropharvestAGG <- raster::aggregate(cropharvestRaster, fact = Resolution, fun = aggMethod, na.action = na.omit)
  if(aggMethod == "sum") {
    #cropharvestAGG <- aggregate(cropharvestRaster, fact = Resolution, fun=quote(aggregateMethod), na.action = na.omit)
    cropharvestAGGTM <- cropharvestAGG / Resolution / Resolution #TOTAL MEAN
    raster::plot(cropharvestAGGTM, col = palette1) #map of cropland density
    #----------- crop cropland area for the given extent ----------
    cropharvestAGGTM_crop <<- raster::crop(cropharvestAGGTM, geo_scale)	
    raster::plot(cropharvestAGGTM_crop, col = palette1) #TODO: don't show this
    .extract_cropland_density(cropharvestAGGTM_crop, hostDensityThreshold)
  } else if(aggMethod == "mean"){
    #cropharvestAGGLM <- aggregate(cropharvestRaster, fact = Resolution, fun=quote(aggregateMethod), na.action = na.omit) # land mean
    cropharvestAGGLM <- cropharvestAGG
    raster::plot(cropharvestAGGLM, col = palette1)
    #----------- crop cropland area for the given extent ----------
    cropharvestAGGLM_crop <<- crop(cropharvestAGGLM, geo_scale)	
    raster::plot(cropharvestAGGLM_crop, col = palette1)
    .extract_cropland_density(cropharvestAGGLM_crop, hostDensityThreshold)
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
  
  #TODO: set round limit programitcally
  for (i in 1:nrow(latilongimatr)) {
    TemMat[i, ] <- distVincentyEllipsoid(round(latilongimatr[i,], 5), latilongimatr)/dvse
  }
  
  distance_matrix <<- TemMat
  
  is_initialized <<- TRUE
}


validate_index_cal <- function(vals_list)
{
  ready <- TRUE
  if(!is_initialized)
  {
    stop("Not initialized. Call initializeCroplandData()")
  }
  if(!is.list(vals_list))
  {
    warning("argument is not a list")
    ready <- FALSE;
  }
  return(ready)
}


# Aggregate -----------------------------------------------------

# inverse power law -------------------------------------------------------
CCRI_powerlaw <- function(dispersal_parameter_beta_vals, linkThreshold = 0, betweenness_metric = FALSE, node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE)
{
  if(!validate_index_cal(dispersal_parameter_beta_vals))
    return(0)
  
  index_list <- lapply(dispersal_parameter_beta_vals, CCRI_powerlaw_function, linkThreshold = linkThreshold, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero,
                       betweenness_metric = betweenness_metric, node_strength = node_strength, sum_of_nearest_neighbors = sum_of_nearest_neighbors, eigenvector_centrality = eigenvector_centrality)

  result_index_list <<- c(result_index_list, index_list)
  return(1)
}

# negative exponential function -------------------------------------------
CCRI_negative_exponential <- function(dispersal_parameter_gamma_vals, linkThreshold = 0, betweenness_metric = FALSE, node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE)
{
  if(!validate_index_cal(dispersal_parameter_gamma_vals))
    return(0)
  
  index_list <- lapply(dispersal_parameter_gamma_vals,
                       CCRI_negExponential_function,
                       linkThreshold = linkThreshold, distance_matrix, lon, lat,
                       cropValue, cropharvestAGGTM_crop, CropValuesAzero,
                       betweenness_metric, node_strength,
                       sum_of_nearest_neighbors, eigenvector_centrality)
  
  result_index_list <<- c(result_index_list, index_list)
  return(1)
}


# Utility functions -------------------------------------------------------

getWeightVector <- function(cropdistancematrix) {
  weight_vec <- E(cropdistancematrix)$weight
  weight_vec[is.na(weight_vec)] = 0
  weight_vec <- weight_vec + 1e-10
}

GetGeographicScales <- function()
{
  performGlobalAnalysis <- config$`CCRI parameters`$Longitude_Latitude$Global
  geoScales <- list()
  if(performGlobalAnalysis)
  {
    geoScales <- list(config$`CCRI parameters`$Longitude_Latitude$EastExt,
                      config$`CCRI parameters`$Longitude_Latitude$WestExt)
  }
  customScales <- config$`CCRI parameters`$Longitude_Latitude$CustomExt
  if(!(is.null(customScales) || is.na(customScales) || length(customScales) == 0))
    geoScales <- c(geoScales, lapply(customScales,as.numeric) )
  
  return(geoScales)
}


# method to calculate zero raster -----------------------------------------

CalculateZeroRaster <- function(geoScale, mean_index_raster)
{
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  #TODO: is there any other way to get 0 raster?
  ZeroRaster <- raster(.kZeroRasterFilePath)
  extZero <- raster::crop(ZeroRaster, geoScale)
  mean_index_raster <- raster::disaggregate(mean_index_raster, fact = c(Resolution, Resolution), method ='' )
  mean_index_raster_ext <- mean_index_raster + extZero
  #TODO: remove this plot..use the one below with col = grey75
  raster::plot(mean_index_raster_ext, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
       yaxt='n', axes=F, box=F, main=paste('Mean cropland connectivity risk index from sensitivity analysis:',
                                           paste(config$`CCRI parameters`$Hosts, collapse = ",")), 
       cex.main=0.7)
  raster::plot(countriesLow, add=TRUE)
  
  #------------------------------------------------------------
  map_grey_background <- raster(.kMapGreyBackGroundTifFilePath)
  
  #Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
  map_grey_background_ext <- crop(map_grey_background, geoScale)
  raster::plot(map_grey_background_ext, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Mean cropland connectivity risk index from sensitivity analysis:',
                  paste(config$`CCRI parameters`$Hosts, collapse = ",")), cex.main=0.7)
  raster::plot(mean_index_raster_ext, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
       yaxt='n', axes=F, box=F, add = TRUE)
  
  raster::plot(countriesLow, add=TRUE, border = "white")
  
  return (c(zeroRasterExtent = extZero, mapGreyBackGroundExtent = map_grey_background_ext, use.names = TRUE))

}

# Complete sensitivity analysis of Variance of CCRI -------------
CCRIVariance <- function(indexes, variance_mean_index_raster, zeroExtentRaster, map_grey_background_ext)
{
  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  Variance_mean_index_ext <- apply(do.call(cbind, indexes), 1, var)

  
  variance_mean_index_raster[] <- Variance_mean_index_ext
  z_var_w <- range(Variance_mean_index_ext[which(Variance_mean_index_ext > 0)])
  raster::plot(variance_mean_index_raster, col = palette1, zlim= z_var_w, xaxt='n',  
       yaxt='n', axes=F, box=F, main = paste('Variance in Cropland Connectivity for range: ', paste(z_var_w, collapse = ' to ')))
  raster::plot(countriesLow, add=TRUE)
  
  #----------------------------------------------------
  
  Variance_mean_index_raster_ext_disagg <- disaggregate(variance_mean_index_raster, fact = c(Resolution, Resolution), method ='' )
  Variance_mean_index_raster_ext_disagg <- Variance_mean_index_raster_ext_disagg + zeroExtentRaster
  
  
  #TODO: explore colors
  raster::plot(map_grey_background_ext, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Variance in cropland connectivity risk index from sensitivity analysis:',
                  paste(config$`CCRI parameters`$Hosts, collapse = ",")), cex.main=0.7)
  raster::plot(Variance_mean_index_raster_ext_disagg, col = palette1, zlim= z_var_w, xaxt='n',  
       yaxt='n', axes=F, box=F, add = TRUE)
  raster::plot(countriesLow, add=TRUE)
  
}

# difference map ----------------------------------------------------------

CalculateDifferenceMap <- function(mean_index_raster_diff, cropharvestAGGTM_crop, cropharvestAGGLM_crop, zeroExtentRaster, map_grey_background_ext)
{
  # difference map
  
  if(missing(cropharvestAGGTM_crop) || missing(cropharvestAGGLM_crop))
  {
    message("Either sum or mean aggregate is missing. Aborting diffrence calculation")
    return(NULL)
  }
  
  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  paldif4 <- diverge_hcl(51, c = 100, l = c(20, 90), power = 1.3  )
  
  #-----------------------------------------------------
  CCRI_ID <- which(mean_index_raster_diff[]>0)
  meantotallandW <- sum(cropharvestAGGTM_crop, cropharvestAGGLM_crop, na.rm = T)/2
  
  meanindexcellW <- mean_index_raster_diff[][CCRI_ID]
  meantotallandcellW <- meantotallandW[][CCRI_ID]
  
  rankdifferentW <- rank(meantotallandcellW*(-1))-rank(meanindexcellW*(-1)) #mean cropland minus mean index, negative value means importance of cropland reduce, positive value means importance increase, zero means the importance of cropland doesn't change.
  mean_index_raster_diff[] <- NaN 
  mean_index_raster_diff[][CCRI_ID] <- rankdifferentW
  
  maxrankW <- max(abs(rankdifferentW))
  zr2 <- range(-maxrankW, maxrankW)
  
  #TODO: not required
  raster::plot(mean_index_raster_diff, main=paste('Difference in rank of cropland harvested area fraction and CCRI:', 
                                          paste(config$`CCRI parameters`$Hosts, collapse = ",")), col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
  raster::plot(countriesLow, add=TRUE)
  
  #mean_index_raster_diff[]
  #--------------------------------------------------
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  #ZeroRaster <- raster("ZeroRaster.tif")
  #West_Zero <- crop(ZeroRaster, west_ext)
  #rasters <- adjust_rasterpair_extent(mean_index_raster_diff, zeroExtentRaster)
  mean_index_raster_diff_disagg <- disaggregate(mean_index_raster_diff, fact = c(Resolution, Resolution), method ='' )
  mean_index_raster_diff_disagg <- mean_index_raster_diff_disagg + zeroExtentRaster
  
  cropNames <- paste(config$`CCRI parameters`$Crops, collapse = ", ")
  #TODO: not required
  raster::plot(mean_index_raster_diff_disagg,
       main=paste('Difference in rank of cropland harvested area fraction and CCRI:', cropNames),
       col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
  raster::plot(countriesLow, add=TRUE)
  
  #------------------------------------------------------------
  #map_grey_background <- raster("map_grey_background.tif")
  #Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
  #map_grey_background_west <- crop(map_grey_background, west_ext)
  raster::plot(map_grey_background_ext, col = "grey65",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Difference in rank of cropland harvested area fraction and CCRI:', cropNames), cex.main=0.7)
  raster::plot(mean_index_raster_diff_disagg, col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, add = TRUE)
  #plot(countriesLow, add=TRUE, border = "white")
}


# CCRI functions ----------------------------------------------------------

CalculateCCRI <- function(linkThreshold = 0, power_law_metrics = config$`CCRI parameters`$NetworkMetrics$InversePowerLaw, 
                          negative_exponential_metrics = config$`CCRI parameters`$NetworkMetrics$NegativeExponential) {
  #TODO: parallelize them
  
  if(!valid_vector_input(power_law_metrics)) {
    stop("Input 'power_law_metrics' must be a non-empty vector of metric names for inverse power law.")
  }
  if(!valid_vector_input(negative_exponential_metrics)) {
    stop("Input 'neative_exponential_metrics' must be a non-empty vector of metric names for negative power law.")
  }
  opted_powerlaw_metrics <- check_metrics(power_law_metrics)
  CCRI_powerlaw(config$`CCRI parameters`$DispersalParameterBeta, linkThreshold, betweenness_metric = opted_powerlaw_metrics$betweeness, 
                node_strength = opted_powerlaw_metrics$node_strength, 
                sum_of_nearest_neighbors = opted_powerlaw_metrics$sum_of_nearest_neighbors, 
                eigenvector_centrality = opted_powerlaw_metrics$eigenvector_centrality)
  
  opted_negative_exp_metrics <- check_metrics(negative_exponential_metrics)
  CCRI_negative_exponential(config$`CCRI parameters`$DispersalParameterGamma, linkThreshold, betweenness_metric = opted_negative_exp_metrics$betweeness, 
                            node_strength = opted_negative_exp_metrics$node_strength, 
                            sum_of_nearest_neighbors = opted_negative_exp_metrics$sum_of_nearest_neighbors, 
                            eigenvector_centrality = opted_negative_exp_metrics$eigenvector_centrality)
}

CCRI_powerlaw_function <- function(dispersal_parameter_beta, linkThreshold, distance_matrix, lon, lat, cropValue, cropRaster, CellNumber, 
                                   betweenness_metric = FALSE, node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE) {
  ##############################################
  #### create adjacency matrix
  
  distancematr <- distance_matrix # pairwise distance matrix
  #---- end of code
  distancematrexp <- distancematr^(-dispersal_parameter_beta) #use function C=AX^(-beta), here A=1, X=distancematr
  cropmatr <- cropValue # complete gravity model with crop data
  cropmatr1 <- matrix(cropmatr, , 1 )
  cropmatr2 <- matrix(cropmatr, 1, )
  
  cropmatrix <- cropmatr1 %*% cropmatr2
  cropmatrix <- as.matrix(cropmatrix)
  cropdistancematr <- distancematrexp * cropmatrix # adjacecy matrix
  logicalmatr <- cropdistancematr > linkThreshold # adjacency matrix after threshold
  stan <- cropdistancematr * logicalmatr
  stan <- round(stan, 6) # use round() because betweenness() may have problem when do the calculation
  cropdistancematrix <- graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
  
  ##############################################
  #### CCRI is a weighted mean of 4 network metric
  ####  
  metric_weights <- calculate_metrics_weight(betweenness_metric, node_strength, sum_of_nearest_neighbors, eigenvector_centrality)
  index <- NULL
  
  ##############################################
  ## sum of nearest neighbors degree
  
  if(sum_of_nearest_neighbors) {
    
    knnpref0<-graph.knn(cropdistancematrix,weights=NA)$knn
    knnpref0[is.na(knnpref0)]<-0
    degreematr<-degree(cropdistancematrix)
    knnpref<-knnpref0*degreematr
    if(max(knnpref)==0){knnprefp=0}else
      if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/metric_weights[[STR_NEAREST_NEIGHBORS_SUM]]}
    
    index <- ifelse(is.null(index), knnprefp, index + knnprefp)
  }
  
  ##############################################
  #### node degree, node strengh 
  ####
  if(node_strength) {
    nodestrength<-graph.strength(cropdistancematrix) 
    nodestrength[is.na(nodestrength)]<-0
    if(max(nodestrength)==0){nodestr=0}else
      if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/metric_weights[[STR_NODE_STRENGTH]]}
    
    index <- ifelse(is.null(index), nodestr, index + nodestr)
  }
  ##############################################
  #### betweenness centrality
  #### 
  #weight method 0:
  #between<-betweenness(cropdistancematrix, weights = 1/E(cropdistancematrix)$weight)
  #weight method 1: 
  #   between<-betweenness(cropdistancematrix, weights = -log(E(cropdistancematrix)$weight))
  #weight method 2:
  if(betweenness_metric) {
    between<-betweenness(cropdistancematrix, weights = (1-1/exp(getWeightVector(cropdistancematrix))))
    
    between[is.na(between)]<-0
    if(max(between)==0){betweenp=0}else
      if(max(between)>0){betweenp=between/max(between)/metric_weights[[STR_BETWEENNESS]]} 
    
    index <- ifelse(is.null(index), betweenp, index + betweenp)
  }
  
  ##############################################
  #### eigenvector and eigenvalues
  #### 
  if(eigenvector_centrality) {
    eigenvectorvalues<-evcent(cropdistancematrix)
    ev<-eigenvectorvalues$vector
    ev[is.na(ev)]<-0
    if(max(ev)==0){evp=0}else
      if(max(ev)!=0){evp=ev/max(ev)/metric_weights[[STR_EIGEN_VECTOR_CENTRALITY]]}
    
    index <- ifelse(is.null(index), evp, index + evp)
  }
  
  
  #index<-knnprefp+evp+betweenp+nodestr
  
  indexpre<-cropRaster
  indexpre[]<-0
  indexpre[CellNumber]<- index
  indexv<-indexpre
  return(indexv)
}

# ```

# CCRI calculated by negative exponential function

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

CCRI_negExponential_function <-function(dispersal_parameter_gamma_val, linkThreshold, distance_matrix, lon, lat, cropValue, cropRaster, CellNumber,
                                        betweenness_metric = FALSE, node_strength = FALSE, sum_of_nearest_neighbors = FALSE, eigenvector_centrality = FALSE)   {
  ##############################################
  #### create adjacency matrix
  ####
  distancematr <- distance_matrix
  #---- end of code
  
  eulernumber<-exp(1)
  distancematrexponential <- eulernumber ^ (-dispersal_parameter_gamma_val * distancematr)# exponential model
  cropmatr <- cropValue # complete gravity model with crop data
  cropmatr1 <- matrix(cropmatr,,1) # complete gravity model with crop data
  cropmatr2 <- matrix(cropmatr,1,)
  cropmatrix <- cropmatr1 %*% cropmatr2
  cropmatrix <- as.matrix(cropmatrix)
  cropdistancematr <- distancematrexponential * cropmatrix
  logicalmatr <- cropdistancematr > linkThreshold
  stan <- cropdistancematr * logicalmatr
  stan <- round(stan, 6) # use round() because betweenness() may have problem when do the calculation
  cropdistancematrix<-graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
  ##############################################
  #### create network for all the selected nodes
  ####
  #V(cropdistancematrix)$color=colororder
  V(cropdistancematrix)$label.cex=0.7
  edgeweight<-E(cropdistancematrix)$weight*4000
  E(cropdistancematrix)$color="red"
  #plot(cropdistancematrix,vertex.size=povalue*300,edge.arrow.size=0.2,edge.width=edgeweight,vertex.label=NA,main=paste(crop, sphere1, 'adjacency matrix threshold>',cutoffadja, ', beta=',beta)) # network with weighted node sizes
  # plot(cropdistancematrix,vertex.size=5,edge.arrow.size=0.2,edge.width=edgeweight,vertex.label=NA,main=paste(crop, sphere1, 'adjacency matrix threshold>',cutoffadja, ', beta=',beta)) # network with identical node size
  
  metric_weights <- calculate_metrics_weight(betweenness_metric, node_strength, sum_of_nearest_neighbors, eigenvector_centrality)
  index <- NULL
  
  if(sum_of_nearest_neighbors) {
    knnpref0<-graph.knn(cropdistancematrix,weights=NA)$knn
    knnpref0[is.na(knnpref0)]<-0
    degreematr<-degree(cropdistancematrix)
    knnpref<-knnpref0*degreematr
    if(max(knnpref)==0){knnprefp=0}else
      if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/metric_weights[[STR_NEAREST_NEIGHBORS_SUM]]}
    
    index <- ifelse(is.null(index), knnprefp, index + knnprefp)
  }
  
  ##############################################
  #### node degree, node strength 
  ####
  if(node_strength) {
  
    nodestrength<-graph.strength(cropdistancematrix) 
    nodestrength[is.na(nodestrength)]<-0
    if(max(nodestrength)==0){nodestr=0}else
      if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/metric_weights[[STR_NODE_STRENGTH]]}
    
    index <- ifelse(is.null(index), nodestr, index + nodestr)
  }
  
  ##############################################
  #### betweenness centrality
  #### 
  #weight method 0
  #between<-betweenness(cropdistancematrix, weights = 1/E(cropdistancematrix)$weight)
  #weight method 1: 
  #   between<-betweenness(cropdistancematrix, weights = -log(E(cropdistancematrix)$weight))
  #weight method 2:
  if(betweenness_metric) {
    
    between<-betweenness(cropdistancematrix, weights = (1-1/exp(getWeightVector(cropdistancematrix))))
    between[is.na(between)]<-0
    if(max(between)==0){betweenp=0}else
      if(max(between)>0){betweenp=between/max(between)/metric_weights[[STR_BETWEENNESS]]}
    
    index <- ifelse(is.null(index), betweenp, index + betweenp)
  }
  
  ##############################################
  #### eigenvector and eigenvalues
  #### 
  if(eigenvector_centrality) {
    eigenvectorvalues<-evcent(cropdistancematrix)
    ev<-eigenvectorvalues$vector
    ev[is.na(ev)]<-0
    if(max(ev)==0){evp=0}else
      if(max(ev)!=0){evp=ev/max(ev)/metric_weights[[STR_EIGEN_VECTOR_CENTRALITY]]}
    
    index <- ifelse(is.null(index), evp, index + evp)
  }
  
  ##############################################
  #### plot index layer
  ####    
  #index<-knnprefp+evp+betweenp+nodestr
  
  indexpre<-cropRaster
  indexpre[]<-0
  indexpre[CellNumber] <- index
  indexv<-indexpre
  return(indexv)
  
}

# Sensitivity analysis ----------------------------------------------------

SensitivityAnalysisOnGeoExtentScale <- function(linkThreshold = 0, geoScale, aggregateMethods, cropHarvestRaster, hostDensityThreshold, resolution) {
  
  cat("\nRunning senstivity analysis for the extent: [", geoScale, "], Link threshold: ", linkThreshold, "Host density threshold: ", hostDensityThreshold)
  
  geoAreaExt <- raster::extent(as.numeric(unlist(geoScale))) #list
  result_index_list <<- list()
  # TODO: per cutoff or cropland threshold
  for (aggMethod in aggregateMethods) 
  {
    InitializeCroplandData(cropHarvestRaster, resolution, geoAreaExt, hostDensityThreshold = hostDensityThreshold, aggMethod)
    
    CalculateCCRI(linkThreshold, power_law_metrics = config$`CCRI parameters`$NetworkMetrics$InversePowerLaw, 
                  negative_exponential_metrics = config$`CCRI parameters`$NetworkMetrics$NegativeExponential)
  }
  
  stackedRasters <- stack(result_index_list)
  mean_index_raster <-  calc(stackedRasters, sum) / length(result_index_list)
  
  mean_index_raster_diff <- mean_index_raster
  variance_mean_index_raster <- mean_index_raster
  
  mean_index_raster_val <- raster::getValues(mean_index_raster)
  zeroId <- which(mean_index_raster_val == 0)
  mean_index_raster[zeroId] <- NaN
  
  terra::plot(mean_index_raster, col = palette1, zlim= c(0, 1),
       main=paste("Mean cropland connectivity risk index from sensitivity analysis: ",
                  config$`CCRI parameters`$Crops , "resolution = ", config$`CCRI parameters`$Resolution), cex.main=0.7)
  raster::plot(countriesLow, add=TRUE)
  
  zeroRasterResults <- CalculateZeroRaster(geoAreaExt, mean_index_raster)
  CCRIVariance(lapply(result_index_list, raster::getValues), variance_mean_index_raster, zeroRasterResults$zeroRasterExtent, zeroRasterResults$mapGreyBackGroundExtent)
  
  CalculateDifferenceMap(mean_index_raster_diff, cropharvestAGGTM_crop, cropharvestAGGLM_crop, zeroRasterResults$zeroRasterExtent, zeroRasterResults$mapGreyBackGroundExtent)
}

SensitivityAnalysisOnCroplandThreshold <- function(linkThresholds, hostDensityThresholds, geoScale, aggregateMethods, cropHarvestRaster, resolution) {
  lapply(linkThresholds, SensitivityAnalysisOnLinkWeight, hostDensityThresholds = hostDensityThresholds, geoScale = geoScale, aggregateMethods = aggregateMethods, cropHarvestRaster = cropHarvestRaster, resolution = resolution)
}

SensitivityAnalysisOnLinkWeight <- function(linkThreshold = 0, hostDensityThresholds, geoScale, aggregateMethods, cropHarvestRaster, resolution) {
  lapply(hostDensityThresholds, SensitivityAnalysisOnGeoExtentScale, linkThreshold = linkThreshold, geoScale = geoScale, aggregateMethods = aggregateMethods, cropHarvestRaster = cropHarvestRaster, resolution = resolution)
}

SenstivityAnalysis <- function()
{
  LoadParameters()
  
  #cuttoff adjacencey matrix
  croplandThresholds <<- config$`CCRI parameters`$HostDensityThreshold
  #cutoffadja <<- config$`CCRI parameters`$LinkWeight
  
  # crop data
  cropharvest <- getCropHarvestRasterSum(as.list(config$`CCRI parameters`$Hosts)) #list
  aggregateMethods <- config$`CCRI parameters`$AggregationStrategy #list
  
  # maps
  geoScales <- GetGeographicScales()
  
  #resolution
  resolution <- config$`CCRI parameters`$Resolution
  
  lapply(geoScales, SensitivityAnalysisOnCroplandThreshold, linkThresholds = config$`CCRI parameters`$LinkThreshold, hostDensityThresholds = croplandThresholds, aggregateMethods = aggregateMethods,
         cropHarvestRaster = cropharvest, resolution = resolution)
}
