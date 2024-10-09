
<!-- README.md is generated from README.Rmd. Please edit that file -->

# utsf

<!-- badges: start -->
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
#> 1961 455.1672 445.7250 471.9750 489.8696 512.6417 572.4447 637.4170 638.2360
#>           Sep      Oct      Nov      Dec
#> 1961 551.7917 504.6337 451.1837 476.9279
```

``` r
library(ggplot2)
autoplot(f)
```

<img src="man/figures/README-example-1.png" width="100%" />

If you are interested in this package you can read its vignette where
all its important features are described.
