#' CCRI calculated by negative exponential function
#' Function that calculates the negative exponential of crop harvested area
#' @author Yanru Xing, \email{yxing@@ufl.edu}
#' @param beta beta parameter of the negative exponential function
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
  cropdistancematrix<-igraph::graph.adjacency(stan,mode=c("undirected"),diag=F,weighted=T)#create adjacency matrix
  ##############################################
  #### create network for all the selected nodes
  ####
  #V(cropdistancematrix)$color=colororder
  V(cropdistancematrix)$label.cex=0.7
  edgeweight<-E(cropdistancematrix)$weight*4000
  E(cropdistancematrix)$color="red"
  #plot(cropdistancematrix,vertex.size=povalue*300,edge.arrow.size=0.2,edge.width=edgeweight,vertex.label=NA,main=paste(crop, sphere1, 'adjacency matrix threshold>',cutoffadja, ', beta=',beta)) # network with weighted node sizes
  # plot(cropdistancematrix,vertex.size=5,edge.arrow.size=0.2,edge.width=edgeweight,vertex.label=NA,main=paste(crop, sphere1, 'adjacency matrix threshold>',cutoffadja, ', beta=',beta)) # network with identical node size
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
  #weight method 0
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
  #### plot index layer
  ####    
  index<-knnprefp+evp+betweenp+nodestr
  
  indexpre<-cropRaster
  indexpre[]<-0
  indexpre[CellNumber] <- index
  indexv<-indexpre
  return(indexv)
  
}
