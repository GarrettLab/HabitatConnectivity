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

# Global constants --------------------------------------------------------

kConfigFileFullPath <-  "configurations/parameters.yaml"
kZeroRasterFilePath <- "ZeroRaster.tif"
kMapGreyBackGroundTifFilePath <- "map_grey_background.tif"

# load config ----------------------------------------------

LoadConfig <- function(filePath = kConfigFileFullPath)
{
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

#Global cropland density map---------------------------------------------------------------
# Only when user has enabled global analysis
GlobalAnalysis <- function()
{
  
  # ```{r, fig.width=20, fig.height=10, dpi=400}
  
  cropharvestAGGTM_crop1 <- crop(cropharvestAGGTM, extent(-180, 180, -60, 80))	
  zrWorldMean <- range(0.1, max(getValues(cropharvestAGGTM_crop1)))
  
  #Removing pixels outside boundary
  mean_index_raster_val <- getValues(cropharvestAGGTM_crop1)
  #structure(mean_index_raster_val)
  
  zeroID <- which(mean_index_raster_val == 0)
  cropharvestAGGTM[zeroID] <- NaN
  
  ZeroRaster <- raster(kZeroRasterFilePath)
  CAM_Zero <- crop(ZeroRaster, extent(-180, 180, -60, 80))
  mean_index_raster <- disaggregate(cropharvestAGGTM_crop1, fact = c(Resolution, Resolution), method = '')
  mean_index_raster_CAM <- mean_index_raster + CAM_Zero
  
  #Plotting cropland density
  map_grey_background <- raster(kMapGreyBackGroundTifFilePath)
  
  map_grey_background_CAM <- crop(map_grey_background, extent(-180, 180, -60, 80))
  
  plot(map_grey_background_CAM, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Mean in crop area fraction:', crop), cex.main=1.6)
  
  plot(mean_index_raster_CAM, main = paste('Crop area density: ', crop), col = palette1, zlim = zrWorldMean, xaxt = 'n', yaxt = 'n', axes = F, box = F, add = TRUE)
}

InitializeCroplandData <- function(cropharvestRaster, resolution, geo_scale, cutoff, aggMethod) {
  ## Read cropland data in a .tif file and get data.frame lon/ lat /cropland density
  # cropharvest <- getCropHarvestRasterSum(crop_names)
  # aggregated resolution
  Resolution <<- resolution # Set aggregated resolution, for example, assign 12 for 1 degree.
  #----------- aggregration -----------------------------
  cropharvestAGG <- aggregate(cropharvestRaster, fact = Resolution, fun = aggMethod, na.action = na.omit)
  if(aggMethod == "sum") {
    #cropharvestAGG <- aggregate(cropharvestRaster, fact = Resolution, fun=quote(aggregateMethod), na.action = na.omit)
    cropharvestAGGTM <- cropharvestAGG / Resolution / Resolution #TOTAL MEAN
    plot(cropharvestAGGTM, col = palette1) #map of cropland density
    #----------- crop cropland area for the given extent ----------
    cropharvestAGGTM_crop <<- crop(cropharvestAGGTM, geo_scale)	
    plot(cropharvestAGGTM_crop, col = palette1) #TODO: don't show this
  } else if(aggMethod == "mean"){
    #cropharvestAGGLM <- aggregate(cropharvestRaster, fact = Resolution, fun=quote(aggregateMethod), na.action = na.omit) # land mean
    cropharvestAGGLM <- cropharvestAGG
    plot(cropharvestAGGLM, col = palette1)
    #----------- crop cropland area for the given extent ----------
    cropharvestAGGLM_crop <<- crop(cropharvestAGGLM, geo_scale)	
    plot(cropharvestAGGLM_crop, col = palette1)
  }
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
CCRI_powerlaw <- function(beta_vals)
{
  if(!validate_index_cal(beta_vals))
    return(0)
  
  index_list <- lapply(beta_vals, CCRI_powerlaw_function, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
  result_index_list <<- c(result_index_list, index_list)
  return(1)
}

# negative exponential function -------------------------------------------
CCRI_negative_exponential <- function(gamma_vals)
{
  if(!validate_index_cal(gamma_vals))
    return(0)
  
  index_list <- lapply(gamma_vals, CCRI_negExponential_function, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
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


# method to calculate zero raster -----------------------------------------

CalculateZeroRaster <- function(geoScale, mean_index_raster)
{
  #cropharvest_grey <- cropharvest
  #greyVal <- getValues(cropharvest_grey)
  #ValCell <- which(greyVal > 0)
  #cropharvest_grey[ValCell] <- 0
  #writeRaster(cropharvest_grey, "ZeroRaster.tif")
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  #TODO: is there any other way to get 0 raster?
  ZeroRaster <- raster(kZeroRasterFilePath)
  extZero <- crop(ZeroRaster, geoScale)
  mean_index_raster <- disaggregate(mean_index_raster, fact = c(Resolution, Resolution), method ='' )
  mean_index_raster_ext <- mean_index_raster + extZero
  #TODO: remove this plot..use the one below with col = grey75
  plot(mean_index_raster_ext, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
       yaxt='n', axes=F, box=F, main=paste('Mean cropland connectivity risk index from sensitivity analysis:', config$`CCRI parameters`$Crops), 
       cex.main=0.7)
  plot(countriesLow, add=TRUE)
  
  #------------------------------------------------------------
  map_grey_background <- raster(kMapGreyBackGroundTifFilePath)
  
  #Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
  map_grey_background_ext <- crop(map_grey_background, geoScale)
  plot(map_grey_background_ext, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=0.7)
  plot(mean_index_raster_ext, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
       yaxt='n', axes=F, box=F, add = TRUE)
  
  plot(countriesLow, add=TRUE, border = "white")
  
  return (c(zeroRasterExtent = extZero, mapGreyBackGroundExtent = map_grey_background_ext, use.names = TRUE))

}

# Complete sensitivity analysis of Variance of CCRI -------------
CCRIVariance <- function(indexes, variance_mean_index_raster, zeroExtentRaster, map_grey_background_ext)
{
  # ```{r ,fig.width=6, fig.height=7, dpi=150}
  Variance_mean_index_ext <- apply(do.call(cbind, indexes), 1, var)

  
  variance_mean_index_raster[] <- Variance_mean_index_ext
  z_var_w <- range(Variance_mean_index_ext[which(Variance_mean_index_ext > 0)])
  plot(variance_mean_index_raster, col = palette1, zlim= z_var_w, xaxt='n',  
       yaxt='n', axes=F, box=F, main = paste('Variance in Cropland Connectivity for range: ', paste(z_var_w, collapse = ' to ')))
  plot(countriesLow, add=TRUE)
  
  #----------------------------------------------------
  
  Variance_mean_index_raster_ext_disagg <- disaggregate(variance_mean_index_raster, fact = c(Resolution, Resolution), method ='' )
  Variance_mean_index_raster_ext_disagg <- Variance_mean_index_raster_ext_disagg + zeroExtentRaster
  
  
  #TODO: explore colors
  plot(map_grey_background_ext, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', config$`CCRI parameters`$Crops), cex.main=0.7)
  plot(Variance_mean_index_raster_ext_disagg, col = palette1, zlim= z_var_w, xaxt='n',  
       yaxt='n', axes=F, box=F, add = TRUE)
  plot(countriesLow, add=TRUE)
  
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
  plot(mean_index_raster_diff, main=paste('Difference in rank of cropland harvested area fraction and CCRI:', 
                                          paste(config$`CCRI parameters`$Crops, collapse = ",")), col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
  plot(countriesLow, add=TRUE)
  
  #mean_index_raster_diff[]
  #--------------------------------------------------
  #------------------------------------------------------------
  #--- remove pixels outside of boundary
  #ZeroRaster <- raster("ZeroRaster.tif")
  #West_Zero <- crop(ZeroRaster, west_ext)
  mean_index_raster_diff_disagg <- disaggregate(mean_index_raster_diff, fact = c(Resolution, Resolution), method ='' )
  mean_index_raster_diff_disagg <- mean_index_raster_diff_disagg + zeroExtentRaster
  
  cropNames <- paste(config$`CCRI parameters`$Crops, collapse = ", ")
  #TODO: not required
  plot(mean_index_raster_diff_disagg,
       main=paste('Difference in rank of cropland harvested area fraction and CCRI:', cropNames),
       col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
  plot(countriesLow, add=TRUE)
  
  #------------------------------------------------------------
  #map_grey_background <- raster("map_grey_background.tif")
  #Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
  #map_grey_background_west <- crop(map_grey_background, west_ext)
  plot(map_grey_background_ext, col = "grey65",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
       main=paste('Difference in rank of cropland harvested area fraction and CCRI:', cropNames), cex.main=0.7)
  plot(mean_index_raster_diff_disagg, col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, add = TRUE)
  #plot(countriesLow, add=TRUE, border = "white")
}


# CCRI functions ----------------------------------------------------------

CalculateCCRI <- function() {
  #TODO: parallelize them
  CCRI_powerlaw(config$`CCRI parameters`$Beta)
  CCRI_negative_exponential(config$`CCRI parameters`$Gamma)
}

CCRI_powerlaw_function <- function(beta, cutoffadja, distance_matrix, lon, lat, cropValue, cropRaster, CellNumber)   {
  ##############################################
  #### create adjacency matrix
  
  distancematr <- distance_matrix # pairwise distance matrix
  #---- end of code
  distancematrexp <- distancematr^(-beta) #use function C=AX^(-beta), here A=1, X=distancematr
  cropmatr <- cropValue # complete gravity model with crop data
  cropmatr1 <- matrix(cropmatr, , 1 )
  cropmatr2 <- matrix(cropmatr, 1, )
  
  cropmatrix <- cropmatr1 %*% cropmatr2
  cropmatrix <- as.matrix(cropmatrix)
  cropdistancematr <- distancematrexp * cropmatrix # adjacecy matrix
  logicalmatr <- cropdistancematr > cutoffadja # adjacency matrix after threshold
  stan <- cropdistancematr * logicalmatr
  stan <- round(stan, 6) # use round() because betweenness() may have problem when do the calculation
  cropdistancematrix <- graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
  ##############################################
  ## sum of nearest neighbors degree
  knnpref0<-graph.knn(cropdistancematrix,weights=NA)$knn
  knnpref0[is.na(knnpref0)]<-0
  degreematr<-degree(cropdistancematrix)
  knnpref<-knnpref0*degreematr
  if(max(knnpref)==0){knnprefp=0}else
    if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}
  
  ##############################################
  #### node degree, node strengh 
  ####
  nodestrength<-graph.strength(cropdistancematrix) 
  nodestrength[is.na(nodestrength)]<-0
  if(max(nodestrength)==0){nodestr=0}else
    if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/6}
  ##############################################
  #### betweenness centrality
  #### 
  #weight method 0:
  #between<-betweenness(cropdistancematrix, weights = 1/E(cropdistancematrix)$weight)
  #weight method 1: 
  #   between<-betweenness(cropdistancematrix, weights = -log(E(cropdistancematrix)$weight))
  #weight method 2:
  between<-betweenness(cropdistancematrix, weights = (1-1/exp(getWeightVector(cropdistancematrix))))
  
  between[is.na(between)]<-0
  if(max(between)==0){betweenp=0}else
    if(max(between)>0){betweenp=between/max(between)/2}
  ##############################################
  #### eigenvector and eigenvalues
  #### 
  eigenvectorvalues<-evcent(cropdistancematrix)
  ev<-eigenvectorvalues$vector
  ev[is.na(ev)]<-0
  if(max(ev)==0){evp=0}else
    if(max(ev)!=0){evp=ev/max(ev)/6}
  ##############################################
  #### CCRI is a weighted mean of 4 network metric
  ####    
  index<-knnprefp+evp+betweenp+nodestr
  
  indexpre<-cropRaster
  indexpre[]<-0
  indexpre[CellNumber]<- index
  indexv<-indexpre
  return(indexv)
}

# ```

# CCRI calculated by negative exponential function

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

CCRI_negExponential_function <-function(gamma,cutoffadja, distance_matrix, lon, lat, cropValue, cropRaster, CellNumber)   {
  ##############################################
  #### create adjacency matrix
  ####
  distancematr <- distance_matrix
  #---- end of code
  
  eulernumber<-exp(1)
  distancematrexponential <- eulernumber ^ (-gamma * distancematr)# exponential model
  cropmatr <- cropValue # complete gravity model with crop data
  cropmatr1 <- matrix(cropmatr,,1) # complete gravity model with crop data
  cropmatr2 <- matrix(cropmatr,1,)
  cropmatrix <- cropmatr1 %*% cropmatr2
  cropmatrix <- as.matrix(cropmatrix)
  cropdistancematr <- distancematrexponential * cropmatrix
  logicalmatr <- cropdistancematr > cutoffadja
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
  knnpref0<-graph.knn(cropdistancematrix,weights=NA)$knn
  knnpref0[is.na(knnpref0)]<-0
  degreematr<-degree(cropdistancematrix)
  knnpref<-knnpref0*degreematr
  if(max(knnpref)==0){knnprefp=0}else
    if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}
  
  ##############################################
  #### node degree, node strengh 
  ####
  nodestrength<-graph.strength(cropdistancematrix) 
  nodestrength[is.na(nodestrength)]<-0
  if(max(nodestrength)==0){nodestr=0}else
    if(max(nodestrength)>0){nodestr=nodestrength/max(nodestrength)/6}
  ##############################################
  #### betweenness centrality
  #### 
  #weight method 0
  #between<-betweenness(cropdistancematrix, weights = 1/E(cropdistancematrix)$weight)
  #weight method 1: 
  #   between<-betweenness(cropdistancematrix, weights = -log(E(cropdistancematrix)$weight))
  #weight method 2:
  between<-betweenness(cropdistancematrix, weights = (1-1/exp(getWeightVector(cropdistancematrix))))
  between[is.na(between)]<-0
  if(max(between)==0){betweenp=0}else
    if(max(between)>0){betweenp=between/max(between)/2}
  ##############################################
  #### eigenvector and eigenvalues
  #### 
  eigenvectorvalues<-evcent(cropdistancematrix)
  ev<-eigenvectorvalues$vector
  ev[is.na(ev)]<-0
  if(max(ev)==0){evp=0}else
    if(max(ev)!=0){evp=ev/max(ev)/6}
  ##############################################
  #### plot index layer
  ####    
  index<-knnprefp+evp+betweenp+nodestr
  
  indexpre<-cropRaster
  indexpre[]<-0
  indexpre[CellNumber] <- index
  indexv<-indexpre
  return(indexv)
  
}

# Sensitivity analysis ----------------------------------------------------

SenstivityAnalysis <- function()
{
  LoadConfig()
  
  #cuttoff adjacencey matrix
  cutoff <<- config$`CCRI parameters`$CropLandThreshold[1]
  cutoffadja <<- config$`CCRI parameters`$LinkWeight[1]
  
  # crop data
  cropharvest <- getCropHarvestRasterSum(as.list(config$`CCRI parameters`$Crops)) #list
  aggregateMethod <- config$`CCRI parameters`$AggregationStrategy #list
  
  # maps
  geoScales <- GetGeographicScales()
  
  #resolution
  resolution <- config$`CCRI parameters`$Resolution
  for (geoScale in geoScales)
  {
    cat("\nRunning senstivity analysis for the extent - [", geoScale, "]")
    geoAreaExt <- extent(as.numeric(unlist(geoScale))) #list
    result_index_list <<- list()
    # TODO: per cutoff or cropland threshold
    for (aggMethod in aggregateMethod) 
    {
      InitializeCroplandData(cropharvest, resolution, geoAreaExt, config$`CCRI parameters`$CropLandThreshold[1], aggMethod)
      CalculateCCRI()
    }
    
    stackedRasters <- stack(result_index_list)
    mean_index_raster <-  calc(stackedRasters, sum) / length(result_index_list)
    
    mean_index_raster_diff <- mean_index_raster
    variance_mean_index_raster <- mean_index_raster
    
    mean_index_raster_val <- getValues(mean_index_raster)
    zeroId <- which(mean_index_raster_val == 0)
    mean_index_raster[zeroId] <- NaN
    
    plot(mean_index_raster, col = palette1, zlim= c(0, 1),
         main=paste("Mean cropland connectivity risk index from sensitivity analysis: ",
                    config$`CCRI parameters`$Crops , "resolution = ", config$`CCRI parameters`$Resolution), cex.main=0.7)
    plot(countriesLow, add=TRUE)
    
    zeroRasterResults <- CalculateZeroRaster(geoAreaExt, mean_index_raster)
    CCRIVariance(lapply(result_index_list, getValues), variance_mean_index_raster, zeroRasterResults$zeroRasterExtent, zeroRasterResults$mapGreyBackGroundExtent)

    CalculateDifferenceMap(mean_index_raster_diff, cropharvestAGGTM_crop, cropharvestAGGLM_crop, zeroRasterResults$zeroRasterExtent, zeroRasterResults$mapGreyBackGroundExtent)
  }
}


