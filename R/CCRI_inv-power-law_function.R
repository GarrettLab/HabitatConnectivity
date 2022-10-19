
#' CCRI calculated by Inverse power-law function
#' This version is revised on 07/23/2020
#' Function that calculates the power-law of crop harvested area
#' @author Yanru Xing, \email{yxing@@ufl.edu}
#' @param beta beta parameter of the inverse power-law function
#' @param cutoffadja cut off for adjacency matrix
#' @param distance_matrix matrix with geodesic distanes
#' @param lat latitude
#' @param lon longitude
#' @param cropValue a vector of crop values extracted from crop harvested area 
#' @param cropRaster an empty object 
#' @param CellNumber a vector with cells above the cutoffs
#' @return a matrix with the inverse power-law values 
#' 
#' #' @importFrom igraph graph.adjacency
#' #' @importFrom igraph degree
#' #' @importFrom igraph graph.knn
#' #' @importFrom igraph graph.strength
#' #' @importFrom igraph betweenness
#' #' @importFrom igraph evcent

#' @examples 
#' index1 <- CCRI_powerlaw_function(beta0, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
#' index2 <- CCRI_powerlaw_function(beta, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
#' index3 <- CCRI_powerlaw_function(beta1, cutoffadja, distance_matrix, lon, lat, cropValue, cropharvestAGGTM_crop, CropValuesAzero)
#' @export

# 

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
  cropdistancematrix <- igraph::graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
  ##############################################
  ## sum of nearest neighbors degree
  knnpref0<-igraph::graph.knn(cropdistancematrix,weights=NA)$knn
  knnpref0[is.na(knnpref0)]<-0
  degreematr<-igraph::degree(cropdistancematrix)
  knnpref<-knnpref0*degreematr
  if(max(knnpref)==0){knnprefp=0}else
    if(max(knnpref)>0){knnprefp=knnpref/max(knnpref)/6}
  
  ##############################################
  #### node degree, node strengh 
  ####
  nodestrength<-igraph::graph.strength(cropdistancematrix) 
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
  between<-igraph::betweenness(cropdistancematrix, weights = (1-1/exp(E(cropdistancematrix)$weight)))
  
  between[is.na(between)]<-0
  if(max(between)==0){betweenp=0}else
    if(max(between)>0){betweenp=between/max(between)/2}
  ##############################################
  #### eigenvector and eigenvalues
  #### 
  eigenvectorvalues<-igraph::evcent(cropdistancematrix)
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
