#' Network density
#'
#' This function first calculates the network density for each dispersal parameter specified by the user.
#' Network density compares the number of available links in a network versus the total number of possible links in the same network.
#' Network density is a measure of how well an entire network is, ranging from 0 (not connected at all) to 1 (fully connected).
#' Calculates and plots the network density of a GeoNetwork object.
#'
#' @param x A GeoNetwork object
#' @return Vector. Up to two ggplot2 objects
#'
#' @export
setMethod("ndplot", signature = "GeoNetwork", function(x) {
  .ndplot(x)
})

.ndplot <- function(x) {

  stopifnot("x must be of type GeoNetwork" = class(x) == "GeoNetwork")
  stopifnot("x is null" = !is.null(x))

  eds_beta <- list()
  betas <- list()

  eds_gamma <- list()
  gammas <- list()

  for (adm in x@rasters$rasters) {
    ed <- igraph::edge_density(igraph::graph_from_adjacency_matrix(adm@amatrix > 0))
    if (!is.nan(adm@beta)) {
      betas <- append(betas, adm@beta)
      eds_beta <- append(eds_beta, ed)
    } else {
      gammas <- append(gammas, adm@gamma)
      eds_gamma <- append(eds_gamma, ed)
    }
  }
  
  # Plot for betas
  bplot <- if (length(betas) > 0) {
    xvals <- unlist(betas)
    yvals <- unlist(eds_beta)
    ggplot2::ggplot(data.frame(xvals = xvals, yvals = yvals),
                    ggplot2::aes(xvals, yvals)) +
      ggplot2::geom_point(color = "#7570b3") +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Betas", y = "Edge density") +
      ggplot2::ggtitle("Inverse powerlaw") +
      ggplot2::theme_bw()
  }

  # Plot for gammas
  gplot <- if (length(gammas) > 0) {
    xvals <- unlist(betas)
    yvals <- unlist(eds_beta)
    ggplot2::ggplot(data.frame(xvals = xvals, yvals = yvals),
                    ggplot2::aes(xvals, yvals)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(color = "#d95f02") +
      ggplot2::labs(x = "Gammas", y = "Edge density") +
      ggplot2::ggtitle("Negative exponential") +
      ggplot2::theme_bw()
  }

  if (!is.null(bplot) > 0 && !is.null(gplot)) {
    patchwork::wrap_plots(bplot, gplot)
  } else if (!is.null(bplot)) {
    bplot
  } else if (!is.null(gplot)) {
    gplot
  } else {
    stop("No data to plot")
  }

}
