
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[<img
src="https://github.com/GarrettLab/HabitatConnectivity/actions/workflows/pages/pages-build-deployment/badge.svg?branch=main"
width="250" height="25" alt="Github Pages" />](https://github.com/GarrettLab/HabitatConnectivity/actions/workflows/pages/pages-build-deployment)
[<img src="https://www.r-pkg.org/badges/version/geohabnet" width="100"
height="25" alt="CRAN status" />](https://CRAN.R-project.org/package=geohabnet)

<!-- badges: end -->

# geohabnet

This package is expanded upon [Xing et al
(2021)](https://academic.oup.com/bioscience/article/70/9/744/5875255).
It add capabilities to customize parameter values using functions and
see results of habitat connectivity risk index in the form of plots. The
goal of `geohabnet` is to enable users to visualize habitat connectivity
risk index using their own parameter values. The risk analysis includes
3 maps -

1.  Mean habitat connectivity

2.  Difference

3.  Variance

Package currently support crops sourced from `geodata::monfredaCrops()`
and `geodata::spamCrops()`. This analysis produces 3 maps - mean,
variance and difference for the crop risk index. There are multiple ways
in which functions can be used - generate final outcome and then the
intermediate outcomes for more sophisticated use cases. Refer to
vignettes. This values are propagated to other functions for performing
operations such distance matrix calculation. The values are set in
`parameters.yaml` using and it can be accessed using `get_parameters()`.
See the usage below.

## Installation

Package can either be installed from CRAN:

``` r
install.packages("geohabnet")
#> Installing package into '/private/var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T/RtmpVAU4Cz/temp_libpathfde9199513a3'
#> (as 'lib' is unspecified)
#> 
#> The downloaded binary packages are in
#>  /var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T//RtmpI7xB49/downloaded_packages
```

or the source version of package can be installed from
[GitHub](https://github.com/GarrettLab/HabitatConnectivity/) with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}

devtools::install_github("GarrettLab/HabitatConnectivity", subdir = "geohabnet")
```

## geohabnet Example

``` r
library(geohabnet)

param_file <- geohabnet::get_parameters()
# now edit the file
geohabnet::set_parameters(new_params = param_file)
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
connectivity*](https://garrettlab.github.io/HabitatConnectivity/articles/analysis.html)
for more elaborate description and usages of functions in this package.
