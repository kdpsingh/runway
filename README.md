
<!-- README.md is generated from README.Rmd. Please edit that file -->

# runway

<!-- badges: start -->

<!-- badges: end -->

The goal of runway is to generate statistics and plots to calculate
discrimination, calibration, and decision curves for prediction models.

## Installation

You can install `runway` from GitHub with:

``` r
remotes::install_github('ML4LHS/runway')
```

## Load the packge

First, load the package.

``` r
library(runway)
```

## Sample datasets

Runway comes with two sample datasets.

``` r
data(single_model_dataset)
head(single_model_dataset)
#>   outcomes predictions
#> 1        0        0.36
#> 2        1        0.31
#> 3        0        0.39
#> 4        0        0.09
#> 5        0        0.44
#> 6        1        0.22

data(multi_model_dataset)
head(multi_model_dataset)
#>   outcomes predictions model_name
#> 1        0        0.26    Model 2
#> 2        1        0.28    Model 1
#> 3        0        0.56    Model 2
#> 4        0        0.27    Model 1
#> 5        0        0.31    Model 1
#> 6        0        0.42    Model 2
```

## Single model calibration plot

``` r
cal_plot(single_model_dataset,
         outcome_var = 'outcomes', 
         pred_var = 'predictions',
         n_bins = 5)
#> Warning: Removed 2 rows containing missing values (geom_bar).
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

## Multi-model calibration plot

``` r
cal_plot_multi(multi_model_dataset,
         outcome_var = 'outcomes',
         model_column = 'model_name',
         pred_var = 'predictions',
         n_bins = 5)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />
