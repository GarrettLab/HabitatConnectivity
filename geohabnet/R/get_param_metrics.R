
.validate_weights <- function(me, we) {
  stopifnot("Sum of metric weights should be 100" = sum(we) == 100)
  stopifnot("Weights or metrics missing. Each metric should have a weight" = length(me) == length(we))
}

.validate_metrics <- function(metrics) {
  # check if weights are valid
  .validate_weights(metrics[[1]], metrics[[2]])

  not_sup <- metrics[[1]][!metrics[[1]] %in% supported_metrics()]
  if (length(not_sup) > 0) {
    stop(paste("Following metrics are not supported: ", paste(not_sup, collapse = ", ")))
  } else {
    cat("metrics...OK\n")
  }
  invisible()
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

#'
#'  Returns metrics currently supported in the analysis.
#'
#'
#' @returns vector of supported metrics.
#' @examples
#' supported_metrics()
#'
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

#' Get metrics from parameters
#'
#' Get metrics and parameters stored in `parameters.yaml`.
#'
#' @param params R object of [load_parameters()]. Default is `load_parameters()`.
#'
#' @return List of metrics - parameters and values. See usage.
#'
#' @examples
#' # Get metrics from parameters
#' get_param_metrics()
#' get_param_metrics(load_parameters())
#'
#' @export
get_param_metrics <- function(params = load_parameters()) {
  pl <- params$`CCRI parameters`$NetworkMetrics$InversePowerLaw
  if (!is.null(pl)) {
    .validate_metrics(pl)
  }
  ne <- params$`CCRI parameters`$NetworkMetrics$NegativeExponential
  if (!is.null(ne)) {
    .validate_metrics(ne)
  }
  return(list(pl = pl, ne = ne))
}

#' Calculation on network matrix.
#' These are basically an abstraction of functions under the [igraph] package.
#'
#' @description
#' The functions included in this abstraction are:
#' - `sonn()`: Calculates the sum of nearest neighbors.
#' - `node_strength()`: Calculates the sum of edge weights of adjacent nodes.
#' - `betweeness()`: Calculates the vertex and edge betweenness based on the number of geodesics.
#' - `ev()`: Calculates the eigenvector centralities of positions within the network.
#' - `closeness()`: measures how many steps is required to access every other vertex from a given vertex.
#' - `degree()`: number of adjacent edges
#' - `page_rank()`: page rank score for vertices
#' @param crop_dm Distance matrix.
#'        In the internal workflow, the distance matrix comes from [initialize_cropland_data()] and risk functions.
#' @param we Weight in percentage.
#'
#' @return Matrix with the mean value based on the assigned weight.
#'
#' @export
sonn <- function(crop_dm, we) {

  knnpref0 <- igraph::graph.knn(crop_dm, weights = NA)$knn
  knnpref0[is.na(knnpref0)] <- 0
  degreematr <- igraph::degree(crop_dm)
  knnpref <- knnpref0 * degreematr
  knnprefp <- if (max(knnpref) == 0) {
    0
  } else if (max(knnpref) > 0) {
    (knnpref / max(knnpref)) * .per_to_real(we)
  }
  return(knnprefp)
}


#' @rdname sonn
node_strength <- function(crop_dm, we) {
  nodestrength <- igraph::graph.strength(crop_dm)
  nodestrength[is.na(nodestrength)] <- 0
  nodestr <- if (max(nodestrength) == 0) {
    0
  } else if (max(nodestrength) > 0) {
    (nodestrength / max(nodestrength)) * .per_to_real(we)
  }

  return(nodestr)
}

#' @rdname sonn
betweeness <- function(crop_dm, we) {

  between <- igraph::betweenness(crop_dm,
                                 weights =
                                   (1 - 1 / exp(.get_weight_vector(
                                     crop_dm
                                   ))))

  between[is.na(between)] <- 0
  betweenp <- if (max(between) == 0) {
    0
  } else if (max(between) > 0) {
    (between / max(between)) * .per_to_real(we)
  }
  return(betweenp)
}

#' @rdname sonn
ev <- function(crop_dm, we) {
  eigenvectorvalues <- igraph::evcent(crop_dm)
  evv <- eigenvectorvalues$vector
  evv[is.na(evv)] <- 0
  evp <- if (max(evv) == 0) {
    0
  } else {
    (evv / max(evv)) * .per_to_real(we)
  }

  return(evp)
}

#' @rdname sonn
degree <- function(crop_dm, we) {
  dmat <- igraph::degree(crop_dm)
  dmat[is.na(dmat)] <- 0
  dmatr <- if (max(dmat) == 0) {
    0
  } else {
    (dmat / max(dmat)) * .per_to_real(we)
  }
  return(dmatr)
}

#' @rdname sonn
closeness <- function(crop_dm, we) {
  cvv <- igraph::closeness(crop_dm)
  cvv[is.na(cvv)] <- 0
  cns <- if (max(cvv) == 0) {
    0
  } else {
    (cvv / max(cvv)) * .per_to_real(we)
  }
  return(cns)
}

#' @rdname sonn
page_rank <- function(crop_dm, we) {
  pr_scores <- igraph::page_rank(crop_dm)
  prv <- pr_scores$vector
  prv[is.na(prv)] <- 0
  prv <- if (max(prv) == 0) {
    0
  } else {
    (prv / max(prv)) * .per_to_real(we)
  }
  return(prv)
}
