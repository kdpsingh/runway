
<!-- README.md is generated from README.Rmd. Please edit that file -->

# runway

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

The goal of runway is to generate statistics and plots to calculate
discrimination, calibration, and decision curves for prediction models.

## Why is it called runway?

Because you can use it to visually compare models.

Sometimes your models look quite different.

![Models looking
different](https://i.pinimg.com/originals/2e/3d/14/2e3d14e6f382c6850685b5aaaff34fec.gif)

Other times your models look the same…

![Models looking
similar](https://pbs.twimg.com/media/Eg7RZKoXcAAhvKw?format=jpg&name=360x360)

## Installation

You can install `runway` from GitHub with:

``` r
remotes::install_github('ML4LHS/runway')
```

## Load the package

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

## Evaluating a single model

### Threshold-performance plot (single model)

``` r
threshperf_plot(single_model_dataset,
                outcome = 'outcomes',
                positive = '1',
                prediction = 'predictions')
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

### Calibration plot with 10 bins (single model)

Note: 10 bins is the default.

``` r
cal_plot(single_model_dataset,
         outcome = 'outcomes',
         positive = '1',
         prediction = 'predictions')
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

### Calibration plot with 5 bins (single model)

``` r
cal_plot(single_model_dataset,
         outcome = 'outcomes',
         positive = '1',
         prediction = 'predictions',
         n_bins = 5)
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

### Calibration plot with loess curve (single model)

``` r
cal_plot(single_model_dataset,
         outcome = 'outcomes',
         positive = '1',
         prediction = 'predictions',
         n_bins = 0,
         show_loess = TRUE)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

## Comparing multiple models

### Threshold-performance plot (multiple models)

``` r
threshperf_plot_multi(multi_model_dataset,
                      outcome = 'outcomes',
                      positive = '1',
                      prediction = 'predictions',
                      model = 'model_name')
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

### Calibration plot with 10 bins (multiple models)

Note: 10 bins is the default.

``` r
cal_plot_multi(multi_model_dataset,
         outcome = 'outcomes',
         positive = '1',
         prediction = 'predictions',
         model = 'model_name')
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

## Calibration plot with 5 bins (multiple models)

``` r
cal_plot_multi(multi_model_dataset,
         outcome = 'outcomes',
         positive = '1',
         prediction = 'predictions',
         model = 'model_name',
         n_bins = 5)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

## Calibration plot with loess curve (multiple models)

Unlike single calibration plots, the choice of binned calibration and
loess calibration are mutually exclusive. To show less curves, you must
set `show_loess` to `TRUE` *and* `n_bins` to `0`.

``` r
cal_plot_multi(multi_model_dataset,
         outcome = 'outcomes',
         positive = '1',
         prediction = 'predictions',
         model = 'model_name',
         n_bins = 0,
         show_loess = TRUE)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

## ROC curve w/CI

``` r
roc_plot(single_model_dataset, 
         outcome = 'outcomes', 
         positive = '1',
         prediction = 'predictions',
         ci = TRUE, 
         plot_title = 'Single ROC curve w/CI ribbon')
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

## Multiple ROC curves w/CI ribbons

``` r
roc_plot_multi(multi_model_dataset, 
         outcome = 'outcomes', 
         positive = '1',
         prediction = 'predictions', 
         model = 'model_name',
         ci = TRUE,
         plot_title = 'Multiple model ROC curves w/CI ribbons')
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />
