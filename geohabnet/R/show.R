setMethod("show", "GeoRasters",
          function(object) {
            cat("class   : ", "GeoRasters", "\n")
            if (!is.null(object$rasters) && length(object$rasters) > 0) {
              cat("rasters : ", length(object$rasters), "\n")
            }
            cat("global  : ", object$global, "\n")
            if (!is.null(object$global_rast) && length(object$global_rast) > 0) {
              cat("globals : ", length(object$global_rast), "\n")
            }
          })

setMethod("show", "GlobalRast",
          function(object) {
            cat("class : ", "GlobalRast", "\n")
            cat("east  : ", length(object$east), "\n")
            cat("west  : ", length(object$west), "\n")
          })

setMethod("show", "GeoModel",
          function(object) {
            di_a <- dim(object@amatrix)
            di_risk <- dim(.unpack_rast_ifnot(object@index))
            pstr <-  " (nrow, ncol)"

            cat("class            : ", "GeoModel", "\n")
            cat("adjacency matrix : ", di_a[1], ", ", di_a[2], pstr, "\n")
            cat("risk index       : ", di_risk[1], ", ", di_risk[2], pstr, "\n")
          })

setMethod("show", "GeoNetwork",
          function(object) {
            pstr <-  " (nrow, ncol)"

            cat("class            : ", "GeoNetwork", "\n")
            if (!is.null(object@habitat_density)) {
              hd <- .unpack_rast_ifnot(object@habitat_density)
              cat("habitat density     : ", terra::nrow(hd), ",",
                  terra::ncol(hd), pstr, class(hd), "\n")
            }
            if (!is.null(object@me_rast)) {
              cat("mean             : ", object@me_out, "\n")
              cat("mean raster      : ", terra::nrow(object@me_rast), ",",
                  terra::ncol(object@me_rast), pstr, "\n")
            }
            if (!is.null(object@var_rast)) {
              cat("variance          : ", object@var_out, "\n")
              cat("variance raster   : ", terra::nrow(object@var_rast), ",",
                  terra::ncol(object@var_rast), pstr, "\n")
            }
            if (!is.null(object@diff_rast)) {
              cat("difference        : ", object@diff_out, "\n")
              cat("difference raster : ", terra::nrow(object@diff_rast), ",",
                  terra::ncol(object@diff_rast), pstr, "\n")
            }
          })
