
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

You can install the **development** version of utsf from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("franciscomartinezdelrio/utsf")
```

or you can install the **stable** version from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("utsf")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(utsf)
# Forecast the next 12 future values of time series UKDriverDeaths using random forest
m <- create_model(UKDriverDeaths, method = "rf")
f <- forecast(m, h = 12)
f$pred # to see the forecast
#>           Jan      Feb      Mar      Apr      May      Jun      Jul      Aug
#> 1985 1306.307 1235.979 1221.588 1170.518 1272.613 1269.642 1330.293 1349.737
#>           Sep      Oct      Nov      Dec
#> 1985 1414.699 1551.156 1723.585 1821.121
library(ggplot2)
autoplot(f)
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

If you are interested in this package you can read its
[vignette](https://cran.r-project.org/web/packages/utsf/vignettes/utsf.html)
where all its important features are described.
