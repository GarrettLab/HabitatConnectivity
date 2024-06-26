% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/params.R
\name{get_parameters}
\alias{get_parameters}
\title{Get Parameters}
\usage{
get_parameters(out_path = tempdir(), iwindow = FALSE)
}
\arguments{
\item{out_path}{character. The output path where the parameter file will be
copied. The default is a temporary directory \code{\link[=tempdir]{tempdir()}}}

\item{iwindow}{logical. If window = \code{TRUE}, this will prompt the user to select the output
directory using a file chooser window. The default value is \code{FALSE}.}
}
\value{
character. The path to the copied parameter file.
}
\description{
This function retrieves the parameters and copies the parameter file to the specified
output path.
}
\details{
Using configuration file is an alternative to \code{\link[=sean]{sean()}}
}
\examples{
get_parameters()
get_parameters(out = tempdir())

}
\seealso{
\code{\link[=set_parameters]{set_parameters()}}
}
