library(yaml)
library(this.path)
library(easycsv)


.get_param_file_path <- function() {
  return(paste(this.proj(), "R/configurations/parameters.yaml", sep = "/"))
}

.get_directoryfromuser <- function() {
  return(easycsv::choose_dir())
}

#' Get Parameters
#'
#' Retrieves the parameters and copies the parameter file to the specified output path.
#'
#' @param iwindow Logical. If TRUE, prompts the user to select the output directory using a file chooser window.
#' @param out_path Character. The output path where the parameter file will be copied.
#' @return Character. The path to the copied parameter file.
#' @export
get_parameters <- function(iwindow = FALSE, out_path = getwd()) {
  if (interactive() && iwindow) {
    out_path <- .get_directoryfromuser()
  }
  
  params_file_path <- .get_param_file_path()
  file.copy(from = params_file_path, to = out_path, copy.mode = FALSE)
  print(paste("parameters are now available at - ", out_path, "/parameters.yaml"))
  return(file.path(paste(out_path, "parameters.yaml", sep = "/")))
}

