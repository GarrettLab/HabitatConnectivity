
<!-- README.md is generated from README.Rmd. Please edit that file -->

# geohabnet

This package is expanded upon [Xing et al
(2021)](https://academic.oup.com/bioscience/article/70/9/744/5875255).
It add capabilities to customize parameter values using functions and
see results of cropland connectivty risk index in the form of plots. The
goal of geohabnet is to enable users to visualize cropland connectivity
risk index using their own parameter values.

## Installation

You can install the development version of geohabnet from
[GitHub](GarrettLab/CroplandConnectivity) with:

``` r
# install.packages("devtools")
devtools::install_github("GarrettLab/CroplandConnectivity", subdir = "geohabnet")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(geohabnet)

param_file <- geohabnet::get_parameters()
#> [1] "parameters fetched successfully"
#now edit the file
geohabnet::set_parameters(new_parameters_file = param_file)
#> /private/var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T/RtmpwZccVD/temp_libpath308c38a51ec6/geohabnet/parameters.yamlTRUE
#> [1] TRUE
geohabnet::senstivity_analysis()
#> /private/var/folders/r5/zggvft9d3yn5kh51wqp78rd00000gn/T/RtmpwZccVD/temp_libpath308c38a51ec6/geohabnet/parameters.yamlTRUE
#> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
#>       Link threshold:  0 Host density threshold:  0.0015
```

<img src="man/figures/README-example-1.png" width="100%" /><img src="man/figures/README-example-2.png" width="100%" /><img src="man/figures/README-example-3.png" width="100%" /><img src="man/figures/README-example-4.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-5.png" width="100%" /><img src="man/figures/README-example-6.png" width="100%" /><img src="man/figures/README-example-7.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-8.png" width="100%" /><img src="man/figures/README-example-9.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-10.png" width="100%" /><img src="man/figures/README-example-11.png" width="100%" /><img src="man/figures/README-example-12.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  0 Host density threshold:  0.002

<img src="man/figures/README-example-13.png" width="100%" /><img src="man/figures/README-example-14.png" width="100%" /><img src="man/figures/README-example-15.png" width="100%" /><img src="man/figures/README-example-16.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-17.png" width="100%" /><img src="man/figures/README-example-18.png" width="100%" /><img src="man/figures/README-example-19.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-20.png" width="100%" /><img src="man/figures/README-example-21.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-22.png" width="100%" /><img src="man/figures/README-example-23.png" width="100%" /><img src="man/figures/README-example-24.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  0 Host density threshold:  0.0025

<img src="man/figures/README-example-25.png" width="100%" /><img src="man/figures/README-example-26.png" width="100%" /><img src="man/figures/README-example-27.png" width="100%" /><img src="man/figures/README-example-28.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-29.png" width="100%" /><img src="man/figures/README-example-30.png" width="100%" /><img src="man/figures/README-example-31.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-32.png" width="100%" /><img src="man/figures/README-example-33.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-34.png" width="100%" /><img src="man/figures/README-example-35.png" width="100%" /><img src="man/figures/README-example-36.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  1e-06 Host density threshold:  0.0015

<img src="man/figures/README-example-37.png" width="100%" /><img src="man/figures/README-example-38.png" width="100%" /><img src="man/figures/README-example-39.png" width="100%" /><img src="man/figures/README-example-40.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-41.png" width="100%" /><img src="man/figures/README-example-42.png" width="100%" /><img src="man/figures/README-example-43.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-44.png" width="100%" /><img src="man/figures/README-example-45.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-46.png" width="100%" /><img src="man/figures/README-example-47.png" width="100%" /><img src="man/figures/README-example-48.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  1e-06 Host density threshold:  0.002

<img src="man/figures/README-example-49.png" width="100%" /><img src="man/figures/README-example-50.png" width="100%" /><img src="man/figures/README-example-51.png" width="100%" /><img src="man/figures/README-example-52.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-53.png" width="100%" /><img src="man/figures/README-example-54.png" width="100%" /><img src="man/figures/README-example-55.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-56.png" width="100%" /><img src="man/figures/README-example-57.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-58.png" width="100%" /><img src="man/figures/README-example-59.png" width="100%" /><img src="man/figures/README-example-60.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  1e-06 Host density threshold:  0.0025

<img src="man/figures/README-example-61.png" width="100%" /><img src="man/figures/README-example-62.png" width="100%" /><img src="man/figures/README-example-63.png" width="100%" /><img src="man/figures/README-example-64.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-65.png" width="100%" /><img src="man/figures/README-example-66.png" width="100%" /><img src="man/figures/README-example-67.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-68.png" width="100%" /><img src="man/figures/README-example-69.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-70.png" width="100%" /><img src="man/figures/README-example-71.png" width="100%" /><img src="man/figures/README-example-72.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  6e-04 Host density threshold:  0.0015

<img src="man/figures/README-example-73.png" width="100%" /><img src="man/figures/README-example-74.png" width="100%" /><img src="man/figures/README-example-75.png" width="100%" /><img src="man/figures/README-example-76.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-77.png" width="100%" /><img src="man/figures/README-example-78.png" width="100%" /><img src="man/figures/README-example-79.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-80.png" width="100%" /><img src="man/figures/README-example-81.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-82.png" width="100%" /><img src="man/figures/README-example-83.png" width="100%" /><img src="man/figures/README-example-84.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  6e-04 Host density threshold:  0.002

<img src="man/figures/README-example-85.png" width="100%" /><img src="man/figures/README-example-86.png" width="100%" /><img src="man/figures/README-example-87.png" width="100%" /><img src="man/figures/README-example-88.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-89.png" width="100%" /><img src="man/figures/README-example-90.png" width="100%" /><img src="man/figures/README-example-91.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-92.png" width="100%" /><img src="man/figures/README-example-93.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-94.png" width="100%" /><img src="man/figures/README-example-95.png" width="100%" /><img src="man/figures/README-example-96.png" width="100%" />

    #> 
    #> Running senstivity analysis for the extent: [ -115 -75 5 32 ],
    #>       Link threshold:  6e-04 Host density threshold:  0.0025

<img src="man/figures/README-example-97.png" width="100%" /><img src="man/figures/README-example-98.png" width="100%" /><img src="man/figures/README-example-99.png" width="100%" /><img src="man/figures/README-example-100.png" width="100%" />

    #> Warning in mean_index_raster + ext_zero: Raster objects have different extents.
    #> Result for their intersection is returned

<img src="man/figures/README-example-101.png" width="100%" /><img src="man/figures/README-example-102.png" width="100%" /><img src="man/figures/README-example-103.png" width="100%" />

    #> Warning in variance_mean_index_raster_ext_disagg + zero_extent_raster: Raster
    #> objects have different extents. Result for their intersection is returned

<img src="man/figures/README-example-104.png" width="100%" /><img src="man/figures/README-example-105.png" width="100%" />

    #> Warning in mean_index_raster_diff_disagg + zero_extent_raster: Raster objects
    #> have different extents. Result for their intersection is returned

<img src="man/figures/README-example-106.png" width="100%" /><img src="man/figures/README-example-107.png" width="100%" /><img src="man/figures/README-example-108.png" width="100%" />

    #> [[1]]
    #> [[1]][[1]]
    #> [[1]][[1]][[1]]
    #> [1] FALSE
    #> 
    #> [[1]][[1]][[2]]
    #> [1] FALSE
    #> 
    #> [[1]][[1]][[3]]
    #> [1] FALSE
    #> 
    #> 
    #> [[1]][[2]]
    #> [[1]][[2]][[1]]
    #> [1] FALSE
    #> 
    #> [[1]][[2]][[2]]
    #> [1] FALSE
    #> 
    #> [[1]][[2]][[3]]
    #> [1] FALSE
    #> 
    #> 
    #> [[1]][[3]]
    #> [[1]][[3]][[1]]
    #> [1] FALSE
    #> 
    #> [[1]][[3]][[2]]
    #> [1] FALSE
    #> 
    #> [[1]][[3]][[3]]
    #> [1] FALSE
