#' Prepare crop data for CCRI functions
#' @author Yanru Xing, \email{yxing@@ufl.edu}
#' @param cropharvest harvested area from MapSpam or Monfreda et al, tif format 
#' @param resolution resolution in degrees, default 12 = 1 degree = 60 minutes 
#' @param hemisphere class extent with coordinates indicating global area: west_ext or east_ext
#' @param cutoff cropland density cutoff, adjust for different crops
#' @return a distance_matrix from distVincentyEllipsoid(), default meters
#' 
#' #' @importFrom raster aggregate
#' #' @importFrom raster getValues
#' #' @importFrom raster extentFromCells
#' #' @importFrom geosphere distVincentyEllipsoid
#' #' @importFrom raster crop
#' #' @importFrom raster extent
#' #' @importFrom raster getValues 
#' #' @importFrom raster raster
#' #' @importFrom raster disaggregate

#' @examples 
#' distance_matrix <- preparecrop(cropharvest, resolution, hemisphere = west_ext, cutoff)
#' @export

preparecrop <- function(cropharvest, resolution, hemisphere = west_ext, cutoff){
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
  #' total mean aggregration 
  cropharvestAGG <- raster::aggregate(cropharvest, fact = Resolution, fun=sum, na.action = na.omit)
  #' calculating the total mean
  cropharvestAGGTM <- cropharvestAGG / Resolution / Resolution 
  plot1 <- plot(cropharvestAGGTM, col = palette1, 
                main = paste("Crop harvested mean:", crop))
  #' hemisphere crop cropland area
  cropharvestAGGTM_crop <- crop(cropharvestAGGTM, hemisphere)	
  plot2 <- plot(cropharvestAGGTM_crop, col = palette1,
                main = paste("Crop harvested mean hemisphere:", crop))
  #' extract cropland density data 
  CropValues <- raster::getValues(cropharvestAGGTM_crop)
  CropValuesAzero <- which(CropValues > cutoff) # find the cells with value > 0.0001
  CropValue <- CropValues[CropValuesAzero]
  #' extract xy corrdination for "povalue" cells
  for(i in 1:length(CropValuesAzero)){
    temp <- raster::extentFromCells(cropharvestAGGTM_crop, CropValuesAzero[i])
    AVxminO <- temp[1]
    lon <- c(lon, AVxminO)
    AVymaxO <- temp[4]
    lat <- c(lat, AVymaxO)
   }
  # Prepare arguments for CCRI functions
  cropdata <- data.frame(lon, lat, CropValue)
  #AdjustConstant <- 2 # to adjust the distance and make sure the distance >1
  #' save the latitude and longitude as new matrix
  latilongimatr <- cropdata[ ,c(1:2)]
  #' Geosphere package
  #' function distVincentyEllipsoid(): to calculate distance in meters
  #' reference of standard distance in meter for one degree
  dvse <- geosphere::distVincentyEllipsoid(c(0,0), cbind(1, 0)) 
  latilongimatr <- as.matrix(latilongimatr)
  TemMat <- matrix(-999, nrow( latilongimatr),nrow(latilongimatr))
  #'
  for (i in 1:nrow(latilongimatr)) {
    TemMat[i, ] <- geosphere::distVincentyEllipsoid(latilongimatr[i,], latilongimatr)/dvse
  }
  distance_matrix <- TemMat
  #' Global cropland density map
  cropharvestAGGTM_crop1 <- raster::crop(cropharvestAGGTM, raster::extent(-180, 180, -60, 80))	
  zrWorldMean <- range(0.1, max(getValues(cropharvestAGGTM_crop1)))
  #' Removing pixels outside boundary
  mean_index_raster_val <- raster::getValues(cropharvestAGGTM_crop1)
  #structure(mean_index_raster_val)
  zeroID <- which(mean_index_raster_val == 0)
  cropharvestAGGTM[zeroID] <- NaN
  #' prepare for plotting
  ZeroRaster <- raster::raster("ZeroRaster.tif")
  CAM_Zero <- raster::crop(ZeroRaster, extent(-180, 180, -60, 80))
  mean_index_raster <- raster::disaggregate(cropharvestAGGTM_crop1, 
                                          fact = c(Resolution, Resolution), method = '')
  mean_index_raster_CAM <- mean_index_raster + CAM_Zero
  #' plotting cropland density
  map_grey_background <- raster::raster("map_grey_background.tif")
  map_grey_background_CAM <- raster::crop(map_grey_background, extent(-180, 180, -60, 80))
  #
  plot(map_grey_background_CAM, col = "grey75",  xaxt='n',  yaxt='n', 
               axes=F, box=F, legend = F, 
               main=paste('Mean in crop area fraction:', crop), cex.main=1.6)
  plot3 <-  plot(mean_index_raster_CAM, main = paste('Crop area density: ', crop), 
               col = palette1, zlim = zrWorldMean, xaxt = 'n', yaxt = 'n', 
               axes = F, box = F, add = TRUE)
  #plot(countriesLow, add=TRUE, border = "white")
  #plot(cropharvestAGGTM, col = palette1)
  plot1; plot2; plot3
  return(distance_matrix)
}
