#' Plot a Raster* object
#'
#'
#' This is a wrapper for [terra::plot()]
#' @param x a Raster* object
#' @param ... additional arguments passed to [terra::plot()]
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
      gs <- .global_ext()
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

    terra::plot(world, col = NA, border = "grey50", add = TRUE)
  }
  invisible(NULL)
}

.plot <- function(rast,
                  label,
                  isglobal,
                  geoscale,
                  colorss = .get_palette(),
                  zlim,
                  typ = "plot",
                  outdir,
                  plotf = .plotmap) {

  info <- .saverast(typ, rast, outdir)

  plotf(rast = rast,
        geoscale = geoscale,
        isglobal = isglobal,
        label = label,
        col_pal = colorss,
        zlim = zlim)

  return(info)
}

.saverast <- function(typ, rast, outdir) {

  newdir <- outdir
  if (is.null(outdir) || length(outdir) == 0) {
    newdir <- tempdir()
  }

  newdir <- file.path(newdir, "plots")
  if (!dir.exists(newdir)) {
    dir.create(newdir, recursive = TRUE)
  }

  fp <- file.path(newdir, paste(typ, "_",
                                stringr::str_replace_all(Sys.time(), "[^a-zA-Z0-9]", ""),
                                ".tif", sep = ""))
  spr <- terra::writeRaster(rast, overwrite = TRUE,
                            filename = fp,
                            gdal = c("COMPRESS=NONE"))
  .showmsg(paste("raster created", fp, sep = ": "), "\n")

  return(list(spr, toString(fp)))
}
