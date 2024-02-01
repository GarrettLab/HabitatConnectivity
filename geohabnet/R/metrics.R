# exported functions ------------------------------------------------------

#'
#'  Returns metrics currently supported in the analysis.
#'
#'
#' @returns vector of supported metrics.
#' @examples
#' supported_metrics()
#'
#' @family metrics
#' @export
supported_metrics <- function() {
  return(c(STR_BETWEENNESS,
           STR_NODE_STRENGTH,
           STR_NEAREST_NEIGHBORS_SUM,
           STR_EIGEN_VECTOR_CENTRALITY,
           STR_CLOSENESS_CENTRALITY,
           STR_DEGREE,
           STR_PAGE_RANK))
}

#' Calculation on network metrics a.k.a centralities.
#'
#' @description
#' These are functions under the [igraph] package adapted to calculate habitat connectivity.
#' In the context of habitat connectivity, the functions can be interpreted as follows:
#' - `[nn_sum()]`: Calculates the sum of nearest neighbors [igraph::graph.knn()].
#' - `[node_strength()]`: Calculates the sum of edge weights of adjacent nodes [igraph::graph.strength()].
#' - `[betweeness()]`: Calculates the node betweenness based on the number of shortest paths.
#' [igraph::betweenness()]. Because the [igraph::betweenness()] function in [igraph] interprets link weights as distances to calculate the shortest paths, the [geohabnet::betweenness()] function in [geohabnet] transforms the link weights (or the relative likelihood of pathogen or pest movement) in the adjacency matrix so that higher link weight values will be the shortest (or more likely) paths for pathogen or pest movement. 
#' - `[ev()]`: Calculates the eigenvector centrality of positions within the network [igraph::evcent()].
#' - `[closeness()]`: measures how many steps is required to access every other vertex from a given vertex
#' [igraph::closeness()]. Because the [igraph::closeness()] function in [igraph] interprets link weights as distances to calculate the shortest paths, the [geohabnet::closeness()] function in [geohabnet] transforms the link weights (or the relative likelihood of pathogen or pest movement) in the adjacency matrix so that higher link weight values will be the shortest (or more likely) paths for pathogen or pest movement.
#' - `[degree()]`: number of adjacent edges [igraph::degree()].
#' - `[pagerank()]`: page rank score for vertices [igraph::page_rank()].
#' @param crop_dm A square adjacency matrix, in which rows and columns names represent nodes (or locations) and each entry indicate the relative likelihood of pathogen or pest movement between a pair of nodes.
#'  In the internal workflow,
#'  the adjacency matrix comes as a result of operations within [sean()] function.
#' @param we Weight in percentage. This weight represents the importance of the network metric in the habitat connectivity analysis.
#' @return SpatRaster. Representing connectivity of each node or location.
#'
#' @family metrics
#' @export
nn_sum <- function(crop_dm) {

  knnpref0 <- igraph::graph.knn(crop_dm, mode = "all", weights = NA)$knn
  knnpref0[is.na(knnpref0)] <- 0
  degreematr <- igraph::degree(crop_dm)
  knnpref <- knnpref0 * degreematr
  knnprefp <- if (max(knnpref) == 0) {
    0
  } else if (max(knnpref) > 0) {
    (knnpref / max(knnpref))
  }
  return(knnprefp)
}


#' @rdname nn_sum
node_strength <- function(crop_dm) {
  nodestrength <- igraph::graph.strength(crop_dm)
  nodestrength[is.na(nodestrength)] <- 0
  nodestr <- if (max(nodestrength) == 0) {
    0
  } else if (max(nodestrength) > 0) {
    (nodestrength / max(nodestrength))
  }

  return(nodestr)
}

#' @rdname nn_sum
betweeness <- function(crop_dm) {

  between <- igraph::betweenness(crop_dm,
                                 directed = FALSE,
                                 weights =
                                   log(1 / get_weight_vector(crop_dm)))

  between[is.na(between)] <- 0
  betweenp <- if (max(between) == 0) {
    0
  } else if (max(between) > 0) {
    (between / max(between))
  }
  return(betweenp)
}

#' @rdname nn_sum
ev <- function(crop_dm) {
  eigenvectorvalues <- igraph::evcent(crop_dm)
  evv <- eigenvectorvalues$vector
  evv[is.na(evv)] <- 0
  evp <- if (max(evv) == 0) {
    0
  } else {
    (evv / max(evv))
  }

  return(evp)
}

#' @rdname nn_sum
degree <- function(crop_dm) {
  dmat <- igraph::degree(crop_dm)
  dmat[is.na(dmat)] <- 0
  dmatr <- if (max(dmat) == 0) {
    0
  } else {
    (dmat / max(dmat))
  }
  return(dmatr)
}

#' @rdname nn_sum
closeness <- function(crop_dm) {
  cvv <- igraph::closeness(crop_dm,
                           weights = log(1/ (.get_weight_vector(crop_dm))))
  cvv[is.na(cvv)] <- 0
  cns <- if (max(cvv) == 0) {
    0
  } else {
    (cvv / max(cvv))
  }
  return(cns)
}

#' @rdname nn_sum
pagerank <- function(crop_dm) {
  pr_scores <- igraph::page_rank(crop_dm)
  prv <- pr_scores$vector
  prv[is.na(prv)] <- 0
  prv <- if (max(prv) == 0) {
    0
  } else {
    (prv / max(prv))
  }
  return(prv)
}

# ------------------------------------------Private methods------------------------------------------



.metric_funs <- function() {

  # Create an empty R environment
  envmap <- new.env()

  # Define the metric functions

  envmap[[STR_NEAREST_NEIGHBORS_SUM]] <- function(graph) nn_sum(graph)
  envmap[[STR_NODE_STRENGTH]] <- function(graph) node_strength(graph)
  envmap[[STR_BETWEENNESS]] <- function(graph) betweeness(graph)
  envmap[[STR_EIGEN_VECTOR_CENTRALITY]] <- function(graph) ev(graph)
  envmap[[STR_CLOSENESS_CENTRALITY]] <- function(graph) closeness(graph)
  envmap[[STR_PAGE_RANK]] <- function(graph) pagerank(graph)
  envmap[[STR_DEGREE]] <- function(graph) degree(graph)
 
  # Return the environment
  return(envmap)
}

.validate_weights <- function(me, we) {
  stopifnot("Sum of metric weights should be 100" = sum(we) == 100)
  stopifnot("Weights or metrics missing. Each metric should have a weight" = length(me) == length(we))
}

.validate_metrics <- function(me, we) {

  lower_me <- tolower(me)
  # check if weights are valid
  .validate_weights(lower_me, we)

  not_sup <- lower_me[!lower_me %in% supported_metrics()]
  if (length(not_sup) > 0) {
    stop(paste("Following metrics are not supported: ", paste(not_sup, collapse = ", ")))
  }
  return(lower_me)
}

.per_to_real <- function(we) {
  return(as.numeric(we) / 100)
}

.list_to_vec <- function(metrics) {
  return(do.call(cbind, metrics))
}

.get_weight_vector <- function(cropdistancematrix) {
  weight_vec <- igraph::E(cropdistancematrix)$weight
  weight_vec[is.na(weight_vec)] <- 0
  weight_vec <- weight_vec + 1e-10
  return(weight_vec)
}

