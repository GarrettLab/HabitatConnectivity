network_density <- function(x) {
  
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

  plotted <- FALSE
  plotted <- if (length(betas) > 0) {
    plot(betas, eds_beta, xlab = "Beta", ylab = "Network density", type = "p", col = "red", bg = "red", cex = 1)
    lines(betas, eds_beta, col = "blue", lwd = 2)
    TRUE
  }

  if (plotted & length(gammas) > 0) {
    points(gammas, eds_gamma, xlab = "Gamma", ylab = "Network density", type = "p", col = "red", cex = 1)
    lines(gammas, eds_gamma, col = "blue", lwd = 2)
  }
  else if (length(gammas) > 0) {
    plot(gammas, eds_gamma, xlab = "Gamma", ylab = "Network density", type = "p", col = "green", cex = 1)
    lines(gammas, eds_gamma, col = "blue", lwd = 2)
  }
}