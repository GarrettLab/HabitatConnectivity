# exported functions ------------------------------------------------------

#' list stores functions to apply metrics to distance metrics.
#' @keywords internal
metric_funs <- list(
  STR_NEAREST_NEIGHBORS_SUM = function(graph, param) nn_sum(graph, param),
  STR_NODE_STRENGTH = function(graph, param) node_strength(graph, param),
  STR_BETWEENNESS = function(graph, param) betweeness(graph, param),
  STR_EIGEN_VECTOR_CENTRALITY = function(graph, param) ev(graph, param),
  STR_CLOSENESS_CENTRALITY = function(graph, param) closeness(graph, param),
  STR_PAGE_RANK = function(graph, param) pagerank(graph, param),
  STR_DEGREE = function(graph, param) degree(graph, param)
)

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

  return(list(pl = .refined_mets(params$`CCRI parameters`$NetworkMetrics$InversePowerLaw),
              ne = .refined_mets(params$`CCRI parameters`$NetworkMetrics$NegativeExponential)))
}

#' Calculation on network matrix.
#'
#' @description
#' These are basically an abstraction of functions under the [igraph] package.
#' The functions included in this abstraction are:
#' - `nn_sum()`: Calculates the sum of nearest neighbors [igraph::graph.knn()].
#' - `node_strength()`: Calculates the sum of edge weights of adjacent nodes [igraph::graph.strength()].
#' - `betweeness()`: Calculates the vertex and edge betweenness based on the number of geodesics
#' [igraph::betweenness()].
#' - `ev()`: Calculates the eigenvector centrality of positions within the network [igraph::evcent()].
#' - `closeness()`: measures how many steps is required to access every other vertex from a given vertex
#' [igraph::closeness()].
#' - `degree()`: number of adjacent edges [igraph::degree()].
#' - `page_rank()`: page rank score for vertices [igraph::page_rank()].
#' @param crop_dm Distance matrix.
#'        In the internal workflow, the distance matrix comes from [initialize_cropland_data()] and risk functions.
#' @param we Weight in percentage.
#'
#' @return Matrix with the mean value based on the assigned weight.
#'
#' @family metrics
#' @export
nn_sum <- function(crop_dm, we) {

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


#' @rdname nn_sum
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

#' @rdname nn_sum
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

#' @rdname nn_sum
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

#' @rdname nn_sum
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

#' @rdname nn_sum
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

#' @rdname nn_sum
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

# ------------------------------------------Private methods------------------------------------------

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

.refined_mets <- function(mets) {
  mets <- lapply(mets, tolower)
  mets[["weights"]] <- as.numeric(mets[["weights"]])
  if (!is.null(mets)) {
    .validate_metrics(mets)
  }
  return(mets)
}
