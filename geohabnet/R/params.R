library(yaml)
library(this.path)
library(easycsv)


.get_param_file_path <- function() {
  return(paste(proj_dir, "R/configurations/parameters.yaml", sep = "/"))
}

get_parameters <- function(interactive = FALSE, out_path = getwd()) {
  proj_dir <- this.proj()
  params_file_path <- .get_param_file_path()
  file.copy(from = params_file_path, to = out_path, copy.mode = FALSE)
  print(paste("parameters are now available at - ", out_path, "/parameters.yaml"))
  return(file.path(paste(out_path, "parameters.yaml", sep = "/")))
}

set_parameter <- function(name, value = NULL) {
  if(is.null(name) || !is.character(name))
    stop("Invalid parameter name:", name)
  params_file_path <- .get_param_file_path()
  # Read the YAML file
  yaml_data <- yaml.load_file(params_file_path)
  yaml_data$name <- value
  yaml.dump(yaml_data, file = params_file_path)
}
