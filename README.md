
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utsf

<!-- badges: start -->

[![R-CMD-check](https://github.com/franciscomartinezdelrio/utsf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/franciscomartinezdelrio/utsf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The utsf package provides an engine for univariate time series
forecasting using different regression models in an autoregressive way.
The engine provides an uniform interface for applying the different
models. Furthermore, it is extensible so that users can easily apply
their own regression models to univariate time series forecasting and
benefit from all the features of the engine, such as preprocessings or
estimation of forecast accuracy.

## Installation

You can install the development version of utsf from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("franciscomartinezdelrio/utsf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(utsf)
# Forecast the next 12 future values of time series AirPassengers using random forest
f <- forecast(AirPassengers, h = 12, method = "rf")
f$pred # to see the forecast
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 1961 453.9954 446.4064 473.0415 489.7651 510.1506 574.9026 636.7767 640.0431
#>           Sep      Oct      Nov      Dec
#> 1961 550.2684 500.6805 451.3721 477.6338
```

``` r
library(ggplot2)
autoplot(f)
```

<img src="man/figures/README-example-1.png" width="100%" />

If you are interested in this package you can read its vignette where
all its important features are described.
