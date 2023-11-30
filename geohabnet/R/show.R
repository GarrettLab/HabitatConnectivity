setMethod("show", "GhabRasters",
          function(object) {
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
            cat("east : ", length(object$east), "\n")
            cat("west : ", length(object$west), "\n")
          })

setMethod("show", "Model",
          function(object) {
            di_a <- dim(object$amatrix)
            pstr <-  " (nrow, ncol)"
            cat("adjacency matrix : ", di[1], ", ", di[2], pstr, "\n")
            cat("risk index       : ", terra::nrow(object$index), ", ", terra::ncol(object$index), pstr, "\n")
          })