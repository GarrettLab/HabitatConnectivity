
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

supported_metrics <- function() {
  return(c("betweeness", "node_strength", "sum_of_nearest_neighbors", "eigenvector_centrality"))
}

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

ev <- function(crop_dm, we) {
  eigenvectorvalues <- igraph::evcent(crop_dm)
  evv <- eigenvectorvalues$vector
  evv[is.na(evv)] <- 0
  evp <- if (max(evv) == 0) {
    0
  } else if (max(evv) != 0) {
    (evv / max(evv)) * .per_to_real(we)
  }

  return(evp)
}
