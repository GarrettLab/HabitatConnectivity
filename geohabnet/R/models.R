#' GeoModel class
#'
#' @description
#' A ref class to represent results of dispersal models.
#'
#' @slot amatrix An adjacency matrix to represent network.
#' @slot index A raster object representing the index.
#' @slot hdthreshold A numeric value representing the threshold.
#' @slot aggregation A character value representing the aggregation method.
#' @slot linkthreshold A numeric value representing the link threshold.
#' @slot beta A numeric value representing the beta parameter.
#' @slot gamma A numeric value representing the gamma parameter.
.model_ob <- setClass("GeoModel",
                      slots = c(amatrix = "matrix",
                                index = "ANY",
                                hdthreshold = "numeric",
                                aggregation = "character",
                                linkthreshold = "numeric",
                                beta = "numeric",
                                gamma = "numeric"),
                      prototype = list(beta = NaN,
                                       gamma = NaN))

setGeneric("setprops", function(x, aggregation, hdthreshold, linkthreshold) {
  standardGeneric("setprops")
})

setMethod("setprops",
          signature(x = "GeoModel",
                    aggregation = "character",
                    hdthreshold = "numeric",
                    linkthreshold = "numeric"),
          function(x,
                   aggregation,
                   hdthreshold,
                   linkthreshold) {

            x@aggregation <- aggregation
            x@hdthreshold <- hdthreshold
            x@linkthreshold <- linkthreshold

            return(x)
          }
)

# private methods ---------------------------------------------------------

.risk_indices <- function(model_list) {
  risk_indices <- sapply(model_list, function(x) x@index)
  return(risk_indices)
}

# private methods ---------------------------------------------------------

.model_powerlaw <- function(beta,
                            link_threshold,
                            distance_matrix = NULL,
                            thresholded_crop_values,
                            adj_mat = NULL,
                            crop_raster,
                            crop_cells_above_threshold,
                            metrics = NULL,
                            me_weights = NULL,
                            cutoff = -1) {

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
  cropdistancematrix <- igraph::graph_from_adjacency_matrix(stan,
                                                            mode = c("undirected"),
                                                            diag = FALSE,
                                                            weighted = TRUE)

  indexpre <- .unpack_rast_ifnot(crop_raster)
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- .apply_met(mets,
                                                     me_weights,
                                                     cropdistancematrix,
                                                     cutoff)
  indexv <- terra::wrap(indexpre)
  return(.model_ob(index = indexv,
                   amatrix = adjmat,
                   linkthreshold = link_threshold,
                   beta = beta))
}

.model_neg_exp <- function(gamma_val,
                           link_threshold,
                           distance_matrix = NULL,
                           thresholded_crop_values,
                           adj_mat = NULL,
                           crop_raster,
                           crop_cells_above_threshold,
                           metrics = NULL,
                           me_weights = NULL,
                           cutoff = -1) {

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
  cropdistancematrix <- igraph::graph_from_adjacency_matrix(stan,
                                                            mode = c("undirected"),
                                                            diag = FALSE,
                                                            weighted = TRUE)

  indexpre <- terra::rast(crop_raster)
  indexpre[] <- 0
  indexpre[crop_cells_above_threshold] <- .apply_met(mets,
                                                     me_weights,
                                                     cropdistancematrix,
                                                     cutoff)
  indexv <- terra::wrap(indexpre)
  return(.model_ob(index = indexv,
                   amatrix = adjmat,
                   linkthreshold = link_threshold,
                   gamma = gamma_val))
}

.apply_met <- function(mets, we, adj_graph, cutoff) {

  mets <- Map(c, mets, we)
  index <- 0

  mfuns <- .metric_funs()
  # Iterate over the metric names and values in 'mets'
  for (mname in names(mets)) {
    if (mname %in% names(mfuns)) {
      val <- mets[[mname]][[2]]
      mfun <- mfuns[[mname]]

      met_result <- if (mname %in% c(STR_BETWEENNESS, STR_CLOSENESS_CENTRALITY)) {
        mfun(adj_graph, cutoff = cutoff)
      } else {
        mfun(adj_graph)
      }

      index <- index + (met_result * .per_to_real(val))
    } else {
      warning(mname, " is not a valid metric for network connectivity")
    }
  }

  replace(index, index == 0, NA)

  return(index)
}
