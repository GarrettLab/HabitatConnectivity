
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geohabnet

This package is expanded upon [Xing et al
(2021)](https://academic.oup.com/bioscience/article/70/9/744/5875255).
It add capabilities to customize parameter values using functions and
see results of cropland connectivity risk index in the form of plots.
The goal of `geohabnet` is to enable users to visualize cropland
connectivity risk index using their own parameter values. The risk
analysis includes 3 maps -

1.  Mean cropland connectivity

2.  Difference

3.  Variance

Package currently support crops sourced from `geodata::monfredaCrops()`
and `geodata::spamCrops()`. This analysis produces 3 maps - mean,
variance and difference for the crop risk index. There are multiple ways
in which functions can be used - generate final outcome and then the
intermediate outcomes for more sophisticated use cases. Refer to
vignettes. Using `initialize_crop_data()` will set some global variables
which can be accessed using the\$\*. This values are propagated to other
functions for performing operations such distance matrix calculation.
The values are set in `parameters.yaml` and it can be accessed using
`get_parameters`. See the usage below.

## Installation

You can install the development version of geohabnet from
[GitHub](GarrettLab/CroplandConnectivity) with:

``` r
# install.packages("devtools")
devtools::install_github("GarrettLab/CroplandConnectivity", subdir = "geohabnet")
```

Since repo is private, replace the installation code with following -
`devtools::install_github("GarrettLab/CroplandConnectivity", subdir = "geohabnet", auth_token = "your token")`

`"your token`” should be replaced with actual Github
[PAT](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens).

## geohabnet Example

``` r
library(geohabnet)

param_file <- geohabnet::get_parameters()
#> [1] "parameters fetched successfully"
# now edit the file
geohabnet::set_parameters(new_parameters_file = param_file)
#> [1] TRUE
```

Run the analysis using -

``` r
geohabnet::senstivity_analysis()
```

`parameters.yaml` stores the parameter and its values. It can be
accessed and set using `get_parameters()` and `set_parameters`
respectively. If `senstivity_analysis()` is not being called, then
`load_parameters()` must be called to load the new parameter values in
environment as global variables.

See other functions and articles for more sophisticated usage.
