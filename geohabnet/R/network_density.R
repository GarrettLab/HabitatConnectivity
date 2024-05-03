network_density <- function(x) {
  stopifnot("x must be of type GeoNetwork" == class(x) == "GeoNetwork")
  stopifnot("x is null" = !is.null(x))
  
  graph_objs <- lapply(x@rasters$rasters, function(x) {igraph::graph_from_adjacency_matrix(x@amatrix)})
  
  
  eds <- lapply(graph_objs, igraph::edge_density(x))
}