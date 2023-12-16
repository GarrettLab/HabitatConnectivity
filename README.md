
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[<img
src="https://github.com/GarrettLab/CroplandConnectivity/actions/workflows/pages/pages-build-deployment/badge.svg?branch=main"
width="250" height="25" alt="Github Pages" />](https://github.com/GarrettLab/CroplandConnectivity/actions/workflows/pages/pages-build-deployment)
[<img src="https://www.r-pkg.org/badges/version/geohabnet" width="100"
height="25" alt="CRAN status" />](https://CRAN.R-project.org/package=geohabnet)

<!-- badges: end -->

# geohabnet

geohabnet is an R package that evaluates habitat connectivity from a network analysis perspective. An explanation of the main theoretical assumptions for the functions in geohabnet is provided by [Xing et al
(2021)](https://academic.oup.com/bioscience/article/70/9/744/5875255).

The main improvements and extensions of geohabnet are
* The geohabnet package allows R users to easily calculate the connectivity of locations in a landscape using a single function.
* Because the geohabnet package allows R users to use as input maps of cropland density (as originally in [Xing et al
(2021)](https://academic.oup.com/bioscience/article/70/9/744/5875255)), host landscape density (either crops or wild species), or habitat distribution (such as host landscape density adjusted by climate suitability), we propose the term habitat connectivity.
* The geohabnet package allows R users to customize parameter values in the analysis, facilitating context-specific analyses.
* The geohabnet package allows users to automatically visualize maps of the habitat connectivity of locations.

The goal of `geohabnet` is to enable users to visualize cropland
connectivity risk index using their own selected parameter values. The risk
analysis outcomes include 3 maps:

1.  The mean connectivity of locations across selected parameter combinations

2.  Difference in ranks between mean connectivity and host density of locations

3.  Variance in connectivity of locations across selected parameter combinations

The package currently supports analysis of cropland connectivity for crop species sourced from `geodata::monfredaCrops()`
and `geodata::spamCrops()` and for any other host species distribution provided by the user. There are multiple ways
in which functions can be used - generate final outcome and then the
intermediate outcomes for more sophisticated use cases. Refer to
vignettes. These values are propagated to other functions for performing
operations, such as distance matrix calculation. The values are set in
`parameters.yaml` and it can be accessed using `get_parameters()`.
See the usage below.

## Installation

The package can either be installed from CRAN:

``` r
install.packages("geohabnet")
#> Installing package into '/private/var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T/RtmpgAVmQR/temp_libpath15ebc6703a975'
#> (as 'lib' is unspecified)
#> installing the source package 'geohabnet'
```

or the source version of package can be installed from
[GitHub](https://github.com/GarrettLab/CroplandConnectivity/tree/main/geohabnet)
with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
#> Loading required package: devtools
#> Loading required package: usethis

devtools::install_github("GarrettLab/CroplandConnectivity", subdir = "geohabnet")
#> Downloading GitHub repo GarrettLab/CroplandConnectivity@HEAD
#> Error in utils::download.file(url, path, method = method, quiet = quiet,  : 
#>   download from 'https://api.github.com/repos/GarrettLab/CroplandConnectivity/tarball/HEAD' failed
```

## geohabnet Example

``` r
library(geohabnet)

param_file <- geohabnet::get_parameters()
#> parameters fetched successfully
# now edit the file
geohabnet::set_parameters(new_params = param_file)
#> [1] TRUE
```

Run the analysis using -

``` r
geohabnet::senstivity_analysis()
```

`parameters.yaml` stores the parameter and its values. It can be
accessed and set using `get_parameters()` and `set_parameters()`
respectively. By default risk analysis is run on global index, for which
scales are present in `global_scales()` .

Refer to help using ?*geohabnet::fun* or *help(geohabnet::fun)*

Refer to article [*Analyzing risk index using cropland
connectivity*](https://garrettlab.github.io/CroplandConnectivity/articles/analysis.html)
for more elaborate description and usages of functions in this package.
