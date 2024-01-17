#' GeoModel class
#'
#' @description
#' A ref class to represent results of dispersal models.
#' @field matrix An adjacency matrix to represent network.
.model_ob <- setRefClass("GeoModel",
                         fields = list(amatrix = "matrix",
                                       index = "ANY"))

# private methods ---------------------------------------------------------

.risk_indices <- function(model_list) {
  risk_indices <- sapply(model_list, function(x) x@index)
  return(risk_indices)
}

.model_powerlaw <- function(beta,
                           link_threshold,
                           distance_matrix = NULL,
                           thresholded_crop_values,
                           adj_mat = NULL,
                           crop_raster,
                           crop_cells_above_threshold,
                           metrics = NULL,
                           me_weights = NULL) {

  mets <- .validate_metrics(metrics, me_weights)

  #### create adjacency matrix
  stan <- if (is.null(adj_mat)) {
    distancematr <- distance_matrix # pairwise distance matrix
    #---- end of code
    # use function C=AX^(-beta), here A=1, X=distancematr
    distancematrexp <- distancematr^(-beta)
    cropmatr <- thresholded_crop_values # complete gravity model with crop data
    cropmatr1 <- matrix(cropmatr, , 1)
    cropmatr2 <- matrix(cropmatr, 1, )

    cropmatrix <- cropmatr1 %*% cropmatr2
    cropmatrix <- as.matrix(cropmatrix)
    # adjacency matrix
    cropdistancematr <- distancematrexp * cropmatrix # make available to users
    # adjacency matrix after threshold
    logicalmatr <- cropdistancematr > link_threshold
    adjmat <- cropdistancematr * logicalmatr
    # use round() because betweeness() may have problem when do the calculation
    round(adjmat, 6)
  } else {
    adj_mat
  }

  # create graph object using adjacency matrix
  cropdistancematrix <- igraph::graph.adjacency(stan,
                                                mode = c("undirected"),
                                                diag = FALSE, weighted = TRUE)

  indexpre <- terra::rast(crop_raster)
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- .apply_met(mets, me_weights, cropdistancematrix)
  indexv <- terra::wrap(indexpre)
  return(.model_ob(index = indexv, amatrix = adjmat))
}

.model_neg_exp <- function(gamma_val,
                          link_threshold,
                          distance_matrix = NULL,
                          thresholded_crop_values,
                          adj_mat = NULL,
                          crop_raster,
                          crop_cells_above_threshold,
                          metrics = NULL,
                          me_weights = NULL) {

  mets <- .validate_metrics(metrics, me_weights)

  #### create adjacency matrix
  ####
  stan <- if (is.null(adj_mat)) {
    distancematr <- distance_matrix

    eulernumber <- exp(1)
    # exponential model
    distancematrexponential <-
      eulernumber^(-gamma_val * distancematr)
    cropmatr <- thresholded_crop_values # complete gravity model with crop data
    cropmatr1 <- matrix(cropmatr, , 1) # complete gravity model with crop data
    cropmatr2 <- matrix(cropmatr, 1, )
    cropmatrix <- cropmatr1 %*% cropmatr2
    cropmatrix <- as.matrix(cropmatrix)
    cropdistancematr <- distancematrexponential * cropmatrix
    logicalmatr <- cropdistancematr > link_threshold
    adjmat <- cropdistancematr * logicalmatr
    # use round() because betweeness() may have problem when do the calculation
    round(adjmat, 6)
  } else {
    adj_mat
  }

  # create graph object from adjacency matrix
  cropdistancematrix <- igraph::graph.adjacency(stan,
                                                mode = c("undirected"),
                                                diag = FALSE, weighted = TRUE)

  indexpre <- terra::rast(crop_raster)
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- .apply_met(mets, me_weights, cropdistancematrix)
  indexv <- terra::wrap(indexpre)
  return(.model_ob(index = indexv, amatrix = adjmat))
}

.apply_met <- function(mets, we, adj_graph) {

  mets <- Map(c, mets, we)
  index <- 0

  mfuns <- .metric_funs()
  # Iterate over the metric names and values in 'mets'
  for (mname in names(mets)) {
    if (mname %in% names(mfuns)) {
      val <- mets[[mname]][[2]]
      mfun <- mfuns[[mname]]
      index <- index + (mfun(adj_graph) * .per_to_real(val))
    } else {
      warning(mname, " is not a valid metric for network connectivity")
    }
  }

  replace(index, index == 0, NA)

  return(index)
}
