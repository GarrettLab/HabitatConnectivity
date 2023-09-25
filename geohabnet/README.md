
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
vignettes. This values are propagated to other functions for performing
operations such distance matrix calculation. The values are set in
`parameters.yaml` and it can be accessed using `get_parameters()`. See
the usage below.

## Installation

The source version of package can be installed from
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
#> 
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#> * checking for file ‘/private/var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T/Rtmp5Du3oI/remotes142c87b1a1d86/GarrettLab-CroplandConnectivity-f1e22a1/geohabnet/DESCRIPTION’ ... OK
#> * preparing ‘geohabnet’:
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> Removed empty directory ‘geohabnet/vignettes’
#> * building ‘geohabnet_1.0.0.tar.gz’
#> Installing package into '/private/var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T/RtmpU8nDB5/temp_libpath12bd83bd84ecb'
#> (as 'lib' is unspecified)
```

## geohabnet Example

``` r
library(geohabnet)

param_file <- geohabnet::get_parameters()
#> [1] "parameters fetched successfully"
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

Refer to article *Analyzing risk index using cropland connectivity* for
more elaborate description and usages of functions in this package.
