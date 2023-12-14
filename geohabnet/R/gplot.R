#' Plot a Raster* object
#' 
#' 
#' This is a wrapper for terra::plot
#' @param x a Raster* object
#' @param ... additional arguments passed to terra::plot
#' @return a plot
#' @export
#' @examples
#' r <- terra::rast(nrows=108, ncols=21, xmin=0, xmax=10)
#' gplot(r)
#' gplot(r, col = "red")
#' gplot(r, col = "red", breaks = 10)
gplot <- function(x, ...) {
  terra::plot(x, ...)
}

# private methods ---------------------------------------------------------

.plotmap <- function(rast, geoscale, isglobal, label, col_pal, zlim) {
  if (interactive() || pkgdown::in_pkgdown()) {
    
    # Set the plot parameters
    oldpar <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(oldpar))
    graphics::par(bg = "aliceblue")

    # Plot the base map
    gplot(.cal_mgb(geoscale, isglobal),
          col = "grey85",
          xaxt = "n",
          yaxt = "n",
          legend = FALSE,
          main = label,
          cex.main = 0.9)
    # Plot the raster
    if (isglobal == TRUE) {
      gs <- terra::ext(rast)
      gplot(rast,
            col = col_pal,
            xaxt = "n",
            yaxt = "n",
            zlim = zlim,
            add = TRUE,
            lwd = 0.7,
            legend = TRUE,
            plg = list(loc = "bottom",
                       ext = c(gs[1] + 30, gs[2] - 30, gs[3] - 30, gs[3] - 20),
                       horizontal = TRUE))
    } else {
      gplot(rast,
            col = col_pal,
            xaxt = "n",
            yaxt = "n",
            zlim = zlim,
            add = TRUE,
            lwd = 0.7,
            legend = TRUE)
    }
    
    # Plot the country boundaries
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    world <- world[which(world$continent != "Antarctica"), ]["geometry"]
    world <- terra::vect(world)
    
    if (isglobal == FALSE) {
      world <- terra::crop(world, terra::ext(rast))
    }
    
    terra::plot(world, col = NA, border = "black", add = TRUE)
  }
  invisible(NULL)
}