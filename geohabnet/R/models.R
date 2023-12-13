#' @title Models
#' @name Models
#' @docType class
#' @description
#' A class to represent results of dispersal models.
#' @field matrix An adjacency matrix to represent network.
.model_ob <- setRefClass("GeoModel",
                         fields = list(
                           amatrix = "matrix",
                           index = "ANY"
                           ))

.risk_indices <- function(model_list) {
  risk_indices <- sapply(model_list, function(x) x@index)
  return(risk_indices)
}

#' Calculate risk index using inbuilt models.
#'
#' @description
#' - [`model_powerlaw()`]: calculates risk index using power law.
#' - [`model_neg_exp()`]: calculates risk index using negative exponential.
#' @param beta A list of beta values. `DispersalParameterBeta` in `parameters.yaml`.
#' @param gamma_val A list of beta values. `DispersalParameterGamma` in `parameters.yaml`.
#' @param link_threshold A threshold value for link.
#' @param distance_matrix distance matrix, generated during [sean()].
#' @param thresholded_crop_values crop values above threshold.
#' @param adj_mat Adjacency matrix(optional) representing un-directed graph network.
#' If this is provided, then gamma_val, distance_matrix, link_threshold and thresholded_crop_values are ignored.
#' These ignored parameters are used to generate adjacency matrix internally.
#' This is the only way to use custom adjacency matrix.
#' @param crop_raster A raster object for cropland harvest.
#' @param crop_cells_above_threshold crop cells above threshold. Only contains cells and not the the values.
#' @param metrics A list 2 vectors - metrics and weights.
#' @return risk index
#' @details
#' Network metrics should be passed as a list of vectors e.g. `list(metrics = c("betweeness"), weights = c(100))`.
#' Default values are fetched from `parameters.yaml` and arguments uses the same structure.
#'
#' @export
model_powerlaw <- function(beta,
                           link_threshold,
                           distance_matrix = the$distance_matrix,
                           thresholded_crop_values,
                           adj_mat = NULL,
                           crop_raster,
                           crop_cells_above_threshold,
                           metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw) {

  metrics <- .refined_mets(metrics)

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

  indexpre <- crop_raster
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- .apply_met(metrics, cropdistancematrix)
  indexv <- indexpre
  return(.model_ob(index = indexv, amatrix = adjmat))
}

#' @rdname model_powerlaw
model_neg_exp <- function(gamma_val,
                          link_threshold,
                          distance_matrix = the$distance_matrix,
                          thresholded_crop_values,
                          adj_mat = NULL,
                          crop_raster,
                          crop_cells_above_threshold,
                          metrics = the$parameters_config$`CCRI parameters`$NetworkMetrics$InversePowerLaw) {
  metrics <- .refined_mets(metrics)

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
                                                diag = FALSE, weighted = TRUE
                                                )

  indexpre <- crop_raster
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- .apply_met(metrics, cropdistancematrix)
  indexv <- indexpre
  return(.model_ob(index = indexv, amatrix = adjmat))
}

# private methods ---------------------------------------------------------

.apply_met <- function(mets, adj_graph) {

  mets <- Map(c, mets[[1]], mets[[2]])
  index <- 0

  mfuns <- .metric_funs()
  # Iterate over the metric names and values in 'mets'
  for (mname in names(mets)) {
    if (mname %in% names(mfuns)) {
      val <- mets[[mname]][[2]]
      mfun <- mfuns[[mname]]
      index <- index + mfun(adj_graph, val)
    } else {
      warning(mname, " is not a valid metric for network connectivity")
    }
  }

  replace(index, index == 0, NA)

  return(index)
}
