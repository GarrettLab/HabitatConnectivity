#'@title: "Code template for global cropland connectivity"
#'@author: "Aaron Plex"
#'@date: "`r Sys.Date()`"
#'@date: "`r format(Sys.time(), '%d %B, %Y')`"

#'@output:
#'  html_document:
#'  toc: true
#' toc_depth: 2
#' toc_float: true
#' number_sections: true

#'@df_print: paged
# This R script analysis the invasion and dispersal risk of pest and disease based on the density of global cropland and the distance among them.
# Uploading libraries needed and geographic scale of analysis

# ```{r, warning=FALSE, message=FALSE}
#----------------------------------------------------------
## Load the following packages

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
#----------------------------------------------------------
#------------ Setting color palettes
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

## Setting geographic scale of analysis
#----------- East hemisphere-------------------
latifrom <- -58 #latitude: 
latito <- 60
longifrom <- -24#longitude: 
longito<- 180
east_ext <- extent(-24, 180, -58, 60)

#----------- West hemisphere-------------------
latifrom2 <- -58 #latitude: from -58 to 60
latito2 <- 60
longifrom2 <- -140 #longitude: from -140 to -34
longito2<- -34
west_ext <- extent(-140, -34, -58, 60)

# ```

# Parameters and data input (Need changes for different crops)
# Here a raster file with information about the distribution of cropland is required. Global maps of cropland are publicly available including [EarthStat](http://www.earthstat.org/harvested-area-yield-175-crops/) and [MapSpam](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DHXBJX). These sources provide data about harvested area of crops around the world as fraction on the total land or hectares. We have also used data from the [Global Biodiversity Information Facility](https://www.gbif.org/), but this requires a transformation from csv to raster files, allocation of georeferenced point locations into pixels and scaling of the number of points falling in each pixels in a range between 0 and 1.
# For simplicity, here we present an example of global cropland connectivity analysis for avocado.

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}
#----------------------------------------------------------
#----------------------------------------------------------
## 1.2 Customize crop and values of parameters
beta0<-0.5                                       ###
beta<-1                                          ###
beta1<-1.5                                       ###
gamma00<-0.05                                    ###
gamma0<-0.1                                      ###
gamma<-0.2                                       ###
gamma1<-0.3                                      ###
gamma2<-1                                        ###
crop<-'avocado'                                  ###
cutoff<-0.0002  #cropland density cutoff        ###
cutoffadja <- 0 # cutoff of adjancecy matrix     ###
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

## Read cropland data in a .tif file and get data.frame lon/ lat /cropland density

cropharvest <- raster("avocado_HarvestedAreaFraction.tif")

## If a crop has more than one raster file, you can sum the cropland density of each file.
#cropharvestB <- raster("otherCrop_HarvestedAreaFraction.tif")
#cropharvest <- sum(cropharvestA, cropharvestB)

## If you are using information from MapSpam make the following transformation:
#cropharvestMapSpam <- raster("Filename.tif") 
#cropharvest<-MapSpam / 10000 # MapSpam data need to convert unit in Hectares into harvested area fraction by deviding 10000
# ```

# Total mean aggregation resolution = 1 (Need changes for different resolution)
# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}
#---------------------------------------------------
# aggregated resolution
Resolution <- 12 # Set aggregated resolution, for example, assign 12 for 1 degree.

#----------- total mean aggregration -----------------------------
cropharvestAGG <- aggregate(cropharvest, fact = Resolution, fun=sum, na.action = na.omit)
cropharvestAGGTM <- cropharvestAGG / Resolution / Resolution #TOTAL MEAN
plot(cropharvestAGGTM, col = palette1)
#----------- crop cropland area for the west hemisphere ----------
cropharvestAGGTM_crop <- crop(cropharvestAGGTM, west_ext)	
plot(cropharvestAGGTM_crop, col = palette1)
#----------- Extract cropland density data -----------------------
CropValues <- getValues(cropharvestAGGTM_crop)
CropValuesAzero <- which(CropValues > cutoff) # find the cells with value > 0.0001
cropValue <- CropValues[CropValuesAzero]
#----------- Extract xy corrdination for "povalue" cells ---------
lon <- NULL # xmin
lat <- NULL # ymax

for(i in 1:length(CropValuesAzero)){
  temp <- extentFromCells(cropharvestAGGTM_crop, CropValuesAzero[i])
  AVxminO <- temp[1]
  lon <- c(lon, AVxminO)
  AVymaxO <- temp[4]
  lat <- c(lat, AVymaxO)
}
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
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
distance_matrix <- TemMat
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

#plot(countriesLow, add=TRUE, border = "white")

#plot(cropharvestAGGTM, col = palette1)
# ```

# CCRI calculated by Inverse power-law function

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}
#CCRI BY Inverse power-law function 
#This version is revised on 07/23/2020

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
  between<-betweenness(cropdistancematrix, weights = (1-1/exp(E(cropdistancematrix)$weight)))
  
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
  between<-betweenness(cropdistancematrix, weights = (1-1/exp(E(cropdistancematrix)$weight)))
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

# ```

## sensitivity analysis CCRI BY Inverse power-law function and negative exponential

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

index1 <- CCRI_powerlaw_function(beta0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index2 <- CCRI_powerlaw_function(beta, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index3 <- CCRI_powerlaw_function(beta1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index4 <- CCRI_negExponential_function(gamma00, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index5 <- CCRI_negExponential_function(gamma0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index6 <- CCRI_negExponential_function(gamma, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index7 <- CCRI_negExponential_function(gamma1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

index8 <- CCRI_negExponential_function(gamma2, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)


# ```

# Land mean aggregation resolution = 1

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

# aggregated resolution
Resolution <- 12 # Set aggregated resolution, for example, assign 12 for 1 degree.
#----------- land mean aggregration -----------------------------
cropharvestAGGLM <- aggregate(cropharvest, fact = Resolution, fun = mean, na.action = na.omit) # land mean
plot(cropharvestAGGLM, col = palette1)
#----------- crop cropland area for the west hemisphere ----------
cropharvestAGGLM_crop <- crop(cropharvestAGGLM, west_ext)	
plot(cropharvestAGGLM_crop, col = palette1)
#----------- Extract cropland density data -----------------------
CropValues <- getValues(cropharvestAGGLM_crop)
CropValuesAzero <- which(CropValues > cutoff) # find the cells with value > 0.0001
cropValue <- CropValues[CropValuesAzero]
#----------- Extract xy corrdination for "povalue" cells ---------
lon <- NULL # xmin
lat <- NULL # ymax

for(i in 1:length(CropValuesAzero)){
  temp <- extentFromCells(cropharvestAGGLM_crop, CropValuesAzero[i])
  AVxminO <- temp[1]
  lon <- c(lon, AVxminO)
  AVymaxO <- temp[4]
  lat <- c(lat, AVymaxO)
}
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# Prepare arguments elements values for the CCRI funtions
cropdata1 <- data.frame(lon, lat, cropValue)
adjustConstant <- 2 # to adjust the distance and make sure the distance >1
latilongimatr <- cropdata1[ ,c(1:2)]# save the latitude and longitude as new matrix  
#---- use Geosphere package, function distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
dvse <- distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree
latilongimatr <- as.matrix(latilongimatr)
TemMat <- matrix(-999, nrow( latilongimatr),nrow(latilongimatr))

for (i in 1:nrow(latilongimatr)) {
  TemMat[i, ] <- distVincentyEllipsoid(latilongimatr[i,], latilongimatr)/dvse * adjustConstant # distance in meter is recaled as degree
}
distance_matrix <- TemMat

# ```

## sensitivity analysis CCRI BY Inverse power-law function and negative exponential

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

index9 <- CCRI_powerlaw_function(beta0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index10 <- CCRI_powerlaw_function(beta, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index11 <- CCRI_powerlaw_function(beta1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index12 <- CCRI_negExponential_function(gamma00, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index13 <- CCRI_negExponential_function(gamma0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index14 <- CCRI_negExponential_function(gamma, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index15 <- CCRI_negExponential_function(gamma1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

index16 <- CCRI_negExponential_function(gamma2, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)

# ```

# Complete sensitivity analysis of CCRI

# ```{r ,fig.width=6, fig.height=7, dpi=150}

mean_index_raster <- sum (index1, index2, index3, index4, index5, index6, index7, index8, 
                          index9, index10, index11, index12, index13, index14, index15, index16) / 16

mean_index_raster_diff <- mean_index_raster
Variance_mean_index_raster_west <- mean_index_raster

mean_index_raster_val <- getValues(mean_index_raster)
zeroId <- which(mean_index_raster_val == 0)
mean_index_raster[zeroId] <- NaN

plot(mean_index_raster, col = palette1, zlim= c(0, 1), main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado, resolution = 1'), cex.main=0.7)
plot(countriesLow, add=TRUE)

#-----------------------------------------------------------
#------------------------  Method to get the zero raster
#cropharvest_grey <- cropharvest
#greyVal <- getValues(cropharvest_grey)
#ValCell <- which(greyVal > 0)
#cropharvest_grey[ValCell] <- 0
#writeRaster(cropharvest_grey, "ZeroRaster.tif")
#------------------------------------------------------------
#--- remove pixels outside of boundary
ZeroRaster <- raster("ZeroRaster.tif")
West_Zero <- crop(ZeroRaster, west_ext)
mean_index_raster <- disaggregate(mean_index_raster, fact = c(Resolution, Resolution), method ='' )
mean_index_raster_west <- mean_index_raster + West_Zero
plot(mean_index_raster_west, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
     yaxt='n', axes=F, box=F, main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), 
     cex.main=0.7)
plot(countriesLow, add=TRUE)

#------------------------------------------------------------
map_grey_background <- raster("map_grey_background.tif")

#Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
map_grey_background_west <- crop(map_grey_background, west_ext)
plot(map_grey_background_west, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=0.7)
plot(mean_index_raster_west, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
     yaxt='n', axes=F, box=F, add = TRUE)
#plot(countriesLow, add=TRUE, border = "white")

# ```

# Western Complete sensitivity analysis of Variance of CCRI

# ```{r ,fig.width=6, fig.height=7, dpi=150}

Variance_mean_index_west <- apply( cbind(index1[], index2[], index3[], index4[], index5[], index6[], 
                                         index7[], index8[], index9[], index10[], index11[], index12[], 
                                         index13[], index14[], index15[], index16[]), 1, var)

Variance_mean_index_raster_west[] <- Variance_mean_index_west
z_var_w <- range(Variance_mean_index_west[which(Variance_mean_index_west > 0)])
plot(Variance_mean_index_raster_west, col = palette1, zlim= z_var_w, xaxt='n',  
     yaxt='n', axes=F, box=F)
plot(countriesLow, add=TRUE)
#----------------------------------------------------

Variance_mean_index_raster_west_disagg <- disaggregate(Variance_mean_index_raster_west, fact = c(Resolution, Resolution), method ='' )
Variance_mean_index_raster_west_disagg <- Variance_mean_index_raster_west_disagg + West_Zero
plot(Variance_mean_index_raster_west_disagg, col = palette1, zlim= z_var_w, xaxt='n',   cex.main=0.7,
     yaxt='n', axes=F, box=F, main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', crop))
plot(countriesLow, add=TRUE)


plot(map_grey_background_west, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', crop), cex.main=0.7)
plot(Variance_mean_index_raster_west_disagg, col = palette1, zlim= z_var_w, xaxt='n',  
     yaxt='n', axes=F, box=F, add = TRUE)


# ```

# Western difference map

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


plot(mean_index_raster_diff, main=paste('Difference in rank of cropland harvested area fraction and CCRI:', crop), col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
plot(countriesLow, add=TRUE)

#mean_index_raster_diff[]
#--------------------------------------------------
#------------------------------------------------------------
#--- remove pixels outside of boundary
#ZeroRaster <- raster("ZeroRaster.tif")
#West_Zero <- crop(ZeroRaster, west_ext)
mean_index_raster_diff_disagg <- disaggregate(mean_index_raster_diff, fact = c(Resolution, Resolution), method ='' )
mean_index_raster_diff_disagg_west <- mean_index_raster_diff_disagg + West_Zero
plot(mean_index_raster_diff_disagg_west, main=paste('Difference in rank of cropland harvested area fraction and CCRI:', crop), col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
plot(countriesLow, add=TRUE)

#------------------------------------------------------------
#map_grey_background <- raster("map_grey_background.tif")
#Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
#map_grey_background_west <- crop(map_grey_background, west_ext)
plot(map_grey_background_west, col = "grey65",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Difference in rank of cropland harvested area fraction and CCRI:', crop), cex.main=0.7)
plot(mean_index_raster_diff_disagg_west, col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, add = TRUE)
#plot(countriesLow, add=TRUE, border = "white")

# ```

# Easten Total mean aggregation resolution = 1

# ```{r ,fig.width=9, fig.height=6.0, dpi=150}
#----------- crop cropland area for the east hemisphere ----------
cropharvestAGGTM_crop <- crop(cropharvestAGGTM, east_ext)	
plot(cropharvestAGGTM_crop, col = palette1, main = "Total mean")
#----------- Extract cropland density data -----------------------
CropValues <- getValues(cropharvestAGGTM_crop)
CropValuesAzero <- which(CropValues > cutoff) # find the cells with value > 0.0001
cropValue <- CropValues[CropValuesAzero]
#----------- Extract xy corrdination for "povalue" cells ---------
lon <- NULL # xmin
lat <- NULL # ymax

for(i in 1:length(CropValuesAzero)){
  temp <- extentFromCells(cropharvestAGGTM_crop, CropValuesAzero[i])
  AVxminO <- temp[1]
  lon <- c(lon, AVxminO)
  AVymaxO <- temp[4]
  lat <- c(lat, AVymaxO)
}
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------

# Prepare arguments elements values for the CCRI funtions
cropdata1 <- data.frame(lon, lat, cropValue)
adjustConstant <- 2 # to adjust the distance and make sure the distance >1
latilongimatr <- cropdata1[ ,c(1:2)]# save the latitude and longitude as new matrix  
#---- use Geosphere package, function distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
dvse <- distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree
latilongimatr <- as.matrix(latilongimatr)
TemMat <- matrix(-999, nrow( latilongimatr),nrow(latilongimatr))

for (i in 1:nrow(latilongimatr)) {
  TemMat[i, ] <- distVincentyEllipsoid(latilongimatr[i,], latilongimatr)/dvse * adjustConstant # distance in meter is recaled as degree
}
distance_matrix <- TemMat

# ```

## sensitivity analysis of CCRI BY Inverse power-law function and negative exponential

# ```{r ,fig.width=9, fig.height=6.0, dpi=150}

index17 <- CCRI_powerlaw_function(beta0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index18 <- CCRI_powerlaw_function(beta, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index19 <- CCRI_powerlaw_function(beta1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index20 <- CCRI_negExponential_function(gamma00, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index21 <- CCRI_negExponential_function(gamma0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index22 <- CCRI_negExponential_function(gamma, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index23 <- CCRI_negExponential_function(gamma1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
index24 <- CCRI_negExponential_function(gamma2, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)

# ```

# Easten land mean aggregation resolution = 1

# ```{r ,fig.width=9, fig.height=6.0, dpi=150}
#---------------------------------------------------
# aggregated resolution
Resolution <- 12 # Set aggregated resolution, for example, assign 12 for 1 degree.
#----------- land mean aggregration -----------------------------
cropharvestAGGLM <- aggregate(cropharvest, fact = Resolution, fun = mean, na.action = na.omit) # land mean
plot(cropharvestAGGLM, col = palette1, main ="Land mean")
#----------- crop cropland area for the west hemisphere ----------
cropharvestAGGLM_crop <- crop(cropharvestAGGLM, east_ext)	
plot(cropharvestAGGLM_crop, col = palette1, , main ="Land mean")
#----------- Extract cropland density data -----------------------
CropValues <- getValues(cropharvestAGGLM_crop)
CropValuesAzero <- which(CropValues > cutoff) # find the cells with value > 0.0001
cropValue <- CropValues[CropValuesAzero]
#----------- Extract xy corrdination for "povalue" cells ---------
lon <- NULL # xmin
lat <- NULL # ymax

for(i in 1:length(CropValuesAzero)){
  temp <- extentFromCells(cropharvestAGGLM_crop, CropValuesAzero[i])
  AVxminO <- temp[1]
  lon <- c(lon, AVxminO)
  AVymaxO <- temp[4]
  lat <- c(lat, AVymaxO)
}
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# Prepare arguments elements values for the CCRI funtions
cropdata1 <- data.frame(lon, lat, cropValue)
adjustConstant <- 2 # to adjust the distance and make sure the distance >1
latilongimatr <- cropdata1[ ,c(1:2)]# save the latitude and longitude as new matrix  
#---- use Geosphere package, function distVincentyEllipsoid() is used to calculate the distance, defult distance is meter
dvse <- distVincentyEllipsoid(c(0,0), cbind(1, 0)) # reference of standard distance in meter for one degree
latilongimatr <- as.matrix(latilongimatr)
TemMat <- matrix(-999, nrow( latilongimatr),nrow(latilongimatr))

for (i in 1:nrow(latilongimatr)) {
  TemMat[i, ] <- distVincentyEllipsoid(latilongimatr[i,], latilongimatr)/dvse * adjustConstant # distance in meter is recaled as degree
}
distance_matrix <- TemMat


# ```

## land mean eastern sensitivity analysis of CCRI BY Inverse power-law function and negative exponential

# ```{r ,fig.width=9, fig.height=6.0, dpi=150}

index25 <- CCRI_powerlaw_function(beta0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index26 <- CCRI_powerlaw_function(beta, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index27 <- CCRI_powerlaw_function(beta1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index28 <- CCRI_negExponential_function(gamma00, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index29 <- CCRI_negExponential_function(gamma0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index30 <- CCRI_negExponential_function(gamma, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index31 <- CCRI_negExponential_function(gamma1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)
index32 <- CCRI_negExponential_function(gamma2, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGLM_crop, CropValuesAzero)


# ```

# Eastern Complete sensitivity analysis of CCRI

# ```{r ,fig.width=9, fig.height=6.0, dpi=150}

mean_index_raster <- sum(index17, index18, index19, index20, index21, index22, index23, index24, 
                         index25, index26, index27, index28, index29, index30, index31, index32) / 16
mean_index_raster_east_diff <- mean_index_raster
Variance_mean_index_raster_east <- mean_index_raster


mean_index_raster_val <- getValues(mean_index_raster)
zeroId <- which(mean_index_raster_val == 0)
mean_index_raster[zeroId] <- NaN

plot(mean_index_raster, col = palette1, zlim= c(0, 1),  main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=1.2)
plot(countriesLow, add=TRUE)

#-----------------------------------------------------------
#--- remove pixels outside of boundary
East_Zero <- crop(ZeroRaster, east_ext)
mean_index_raster <- disaggregate(mean_index_raster, fact = c(Resolution, Resolution), method ='' )
mean_index_raster_east <- mean_index_raster + East_Zero
plot(mean_index_raster_east, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
     yaxt='n', axes=F, box=F, main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=1.2)
plot(countriesLow, add=TRUE)

#-----------------------------------------------------------
map_grey_background_east <- crop(map_grey_background, east_ext)
plot(map_grey_background_east, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=1.2)
plot(mean_index_raster_east, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
     yaxt='n', axes=F, box=F, add = TRUE)
#plot(countriesLow, add=TRUE, border = "white")

# ```

# Eastern Complete sensitivity analysis of Variance of CCRI

# ```{r ,fig.width=9, fig.height=6.0, dpi=150}

Variance_mean_index_east <- apply( cbind(index17[], index18[], index19[], index20[], index21[], index22[], 
                                         index23[], index24[], index25[], index26[], index27[], index28[], 
                                         index29[], index30[], index31[], index32[]), 1, var)

Variance_mean_index_raster_east[] <- Variance_mean_index_east
z_var <- range(Variance_mean_index_east[which(Variance_mean_index_east > 0)])
plot(Variance_mean_index_raster_east, col = palette1, zlim= z_var, xaxt='n',  
     yaxt='n', axes=F, box=F, main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', crop))
plot(countriesLow, add=TRUE)
#----------------------------------------------------

Variance_mean_index_raster_east_disagg <- disaggregate(Variance_mean_index_raster_east, fact = c(Resolution, Resolution), method ='' )
Variance_mean_index_raster_east_disagg <- Variance_mean_index_raster_east_disagg + East_Zero
plot(Variance_mean_index_raster_east_disagg, col = palette1, zlim= z_var, xaxt='n',  
     yaxt='n', axes=F, box=F, main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', crop))
plot(countriesLow, add=TRUE)


plot(map_grey_background_east, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Variance in cropland connectivity risk index from sensitivity analysis:', crop), cex.main=1.2)
plot(Variance_mean_index_raster_east_disagg, col = palette1, zlim= z_var, xaxt='n',  
     yaxt='n', axes=F, box=F, add = TRUE)

# ```

# Eestern difference map

# ```{r ,fig.width=9, fig.height=6, dpi=150}

#-----------------------------------------------------
CCRI_ID_E <- which(mean_index_raster_east_diff[]>0)
meantotallandE <- sum(cropharvestAGGTM_crop, cropharvestAGGLM_crop, na.rm = T)/2

meanindexcellE <- mean_index_raster_east_diff[][CCRI_ID_E]
meantotallandcellE <- meantotallandE[][CCRI_ID_E]

rankdifferentE <- rank(meantotallandcellE*(-1))-rank(meanindexcellE*(-1)) #mean cropland minus mean index, negative value means importance of cropland reduce, positive value means importance increase, zero means the importance of cropland doesn't change.
mean_index_raster_east_diff[] <- NaN 
mean_index_raster_east_diff[][CCRI_ID_E] <- rankdifferentE

maxrankE<-max(abs(rankdifferentE))
zr2 <- range(-maxrankE,maxrankE)


plot(mean_index_raster_east_diff, main=paste('Difference in rank of cropland harvested area fraction and CCRI:', crop), col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
plot(countriesLow, add=TRUE)

#mean_index_raster_diff[]
#--------------------------------------------------
#------------------------------------------------------------
#--- remove pixels outside of boundary
#ZeroRaster <- raster("ZeroRaster.tif")
East_Zero <- crop(ZeroRaster, east_ext)
mean_index_raster_diff_disagg_E <- disaggregate(mean_index_raster_east_diff, fact = c(Resolution, Resolution), method ='' )
mean_index_raster_diff_disagg_east <- mean_index_raster_diff_disagg_E + East_Zero
plot(mean_index_raster_diff_disagg_east, main=paste('Difference in rank of cropland harvested area fraction and CCRI:', crop), col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, cex.main=0.7)
plot(countriesLow, add=TRUE)

#------------------------------------------------------------
#map_grey_background <- raster("map_grey_background.tif")
#Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
map_grey_background_east <- crop(map_grey_background, east_ext)
plot(map_grey_background_east, col = "grey65",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Difference in rank of cropland harvested area fraction and CCRI:', crop), cex.main=0.7)
plot(mean_index_raster_diff_disagg_east, col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, add = TRUE)
#plot(countriesLow, add=TRUE, border = "white")

# ```

# combind East and west CCRI, they was analysis seperatedly first

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=150}

latifrom3 <- -58
latito3 <- 76
longifrom3 <- -140
longito3 <- 160
EWcropEx <- extent(-140, 160, -50, 70)
EWcropMeanCCRI <- merge(mean_index_raster_west, mean_index_raster_east, overlay=T, ext=EWcropEx)
#writeRaster(EWcropMeanCCRI, "world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
plot(EWcropMeanCCRI, col = palette1,  xaxt='n',  yaxt='n', axes=F, box=F,
     main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=1.6)
plot(countriesLow, add=TRUE)
# ```

#plot grey background

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=400}


#----------------------------------------------
#Avocado <- raster("world Mean cropland connectivity risk index from sensitivity analysis_Avocado v3.tif")
plot(map_grey_background, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=1.6)
plot(EWcropMeanCCRI, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
     yaxt='n', axes=F, box=F, add = TRUE)

#----------------------------------------------
#plot(map_grey_background, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
#     main=paste('Mean cropland connectivity risk index from sensitivity analysis: avocado'), cex.main=1.6)
#plot(EWcropMeanCCRI, col = palette1, zlim= c(0.000000000000, 1), xaxt='n',  
#    yaxt='n', axes=F, box=F, add = TRUE)
#plot(countriesLow, add=TRUE, border = "white")


# ```

# combind East and West Variance map, they were analysis seperatedly first

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=400}
EWcropMeanVar <- merge(Variance_mean_index_raster_west_disagg, Variance_mean_index_raster_east_disagg, overlay=T, ext=EWcropEx)


plot(map_grey_background, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Variance in CCRI from sensitivity analysis:', crop), cex.main=1.6)

plot(EWcropMeanVar, col=palette1, zlim=c(min(z_var_w, z_var), max(z_var_w, z_var)), xaxt='n',  yaxt='n', axes=F, box=F, add = TRUE)
# ```

# combind East and West diff map, they were analysis seperatedly first

# ```{r ,fig.width=11.75, fig.height=6.0, dpi=400}
zr2 <- range(-maxrankE,maxrankE)
EWcropMeanDiff <- merge(mean_index_raster_diff_disagg_west, mean_index_raster_diff_disagg_east, overlay=T, ext=EWcropEx)
#writeRaster(EWcropMeanCCRI, "world Mean cropland connectivity risk index from sensitivity analysis_Avocado.tif")
plot(EWcropMeanDiff, col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, 
     main=paste('Difference in rank of cropland harvested area fraction and CCRI', crop), cex.main=1.6)
plot(countriesLow, add=TRUE)

#------------------------------------
plot(map_grey_background, col = "grey75",  xaxt='n',  yaxt='n', axes=F, box=F, legend = F, 
     main=paste('Difference in rank of cropland harvested area fraction and CCRI', crop), cex.main=1.6)
plot(EWcropMeanDiff, col=paldif4, zlim=zr2, xaxt='n',  yaxt='n', axes=F, box=F, add = TRUE)
plot(countriesLow, add=TRUE, border = "white")   
# ```

Saving mean, variance and difference of CCRI and CD in the format ".tif"

# ```{r}
maintitleMeanCCRI <- paste(crop, "Mean CCRI global avocado 1 Monfreda varian probabilistic.tif")
writeRaster(EWcropMeanCCRI, maintitleMeanCCRI, overwrite = TRUE)

maintitleVarianceCCRI <- paste(crop, "Variance CCRI global avocado 1 Monfreda variant probabilistic.tif")
writeRaster(EWcropMeanVar, maintitleVarianceCCRI, overwrite = TRUE)

maintitleDifferenceCCRI_CD <- paste(crop, "Difference CCRI & CD global avocado 1 Monfreda variant probabilistic.tif")
writeRaster(EWcropMeanDiff, maintitleDifferenceCCRI_CD, overwrite = TRUE)
# ```
