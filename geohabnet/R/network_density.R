#' Network density
#'
#' Calculates and plots the network density of a GeoNetwork object.
#' The plot is produced as dispersal parameter vs edge density.
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
      ggplot2::geom_point(color = "red") +
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
      ggplot2::geom_point(color = "green") +
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
