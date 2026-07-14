
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

You can install the **development** version of utsf from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("franciscomartinezdelrio/utsf")
#> ℹ Loading metadata database✔ Loading metadata database ... done
#>  
#> → Package library at 'C:\Users\UJA\AppData\Local\Temp\RtmpUhCTs4\temp_libpath59086f02530a'.
#> → Will install 71 packages.
#> → Will update 1 package.
#> → Will download 70 CRAN packages (87.47 MB), cached: 1 (2.68 MB).
#> → Will download 1 package with unknown size.
#> + base64enc            0.1-6      [dl] (35.47 kB)
#> + bslib                0.11.0     [dl] (6.46 MB)
#> + cachem               1.1.0      [dl] (74.17 kB)
#> + cli                  3.6.6      [dl] (1.41 MB)
#> + colorspace           2.1-3      
#> + commonmark           2.0.0      [dl] (142.84 kB)
#> + Cubist               0.6.0      [dl] (891.97 kB)
#> + data.table           1.18.4     [dl] (3.20 MB)
#> + diagram              1.6.5      [dl] (689.25 kB)
#> + digest               0.6.39     [dl] (226.45 kB)
#> + farver               2.1.2      [dl] (1.52 MB)
#> + fastmap              1.2.0      [dl] (138.28 kB)
#> + FNN                  1.1.4.1    [dl] (451.57 kB)
#> + fontawesome          0.5.3      [dl] (1.39 MB)
#> + forecast             9.0.2      [dl] (1.99 MB)
#> + fracdiff             1.5-4      [dl] (108.25 kB)
#> + fs                   2.1.0      [dl] (428.23 kB)
#> + future               1.70.0     [dl] (1.05 MB)
#> + future.apply         1.20.2     [dl] (212.59 kB)
#> + generics             0.1.4      [dl] (82.71 kB)
#> + ggplot2              4.0.3      [dl] (8.47 MB)
#> + globals              0.19.1     [dl] (176.92 kB)
#> + glue                 1.8.1      [dl] (191.81 kB)
#> + gtable               0.3.6      [dl] (250.95 kB)
#> + htmltools            0.5.9      [dl] (365.28 kB)
#> + httpuv               1.6.17     [dl] (1.09 MB)
#> + ipred                0.9-15     [dl] (393.51 kB)
#> + isoband              0.3.0      [dl] (1.98 MB)
#> + jquerylib            0.1.4      [dl] (526.25 kB)
#> + jsonlite             2.0.0      [dl] (1.11 MB)
#> + labeling             0.4.3      [dl] (64.16 kB)
#> + later                1.4.8      [dl] (482.45 kB)
#> + lava                 1.9.2      [dl] (2.51 MB)
#> + lifecycle            1.0.5      [dl] (149.81 kB)
#> + listenv              1.0.0      [dl] (140.63 kB)
#> + lmtest               0.9-40     [dl] (411.80 kB)
#> + magrittr             2.0.5      [dl] (231.07 kB)
#> + memoise              2.0.1      [dl] (51.79 kB)
#> + mime                 0.13       [dl] (52.57 kB)
#> + numDeriv             2016.8-1.1 [dl] (117.81 kB)
#> + otel                 0.2.0      [dl] (286.44 kB)
#> + parallelly           1.48.0     [dl] (682.21 kB)
#> + plyr                 1.8.9      [dl] (1.12 MB)
#> + prodlim              2026.03.11 [dl] (536.83 kB)
#> + progressr            1.0.0      [dl] (607.19 kB)
#> + promises             1.5.0      [dl] (1.70 MB)
#> + R6                   2.6.1      [dl] (88.73 kB)
#> + ranger               0.18.0     [dl] (830.55 kB)
#> + rappdirs             0.3.4      [dl] (53.41 kB)
#> + RColorBrewer         1.1-3      [dl] (54.70 kB)
#> + Rcpp                 1.1.2      [dl] (2.97 MB)
#> + reshape2             1.4.5      [dl] (454.06 kB)
#> + rlang                1.3.0      [dl] (1.65 MB)
#> + S7                   0.2.2      [dl] (351.91 kB)
#> + sass                 0.4.10     [dl] (2.59 MB)
#> + scales               1.4.0      [dl] (878.45 kB)
#> + shape                1.4.6.1    [dl] (755.40 kB)
#> + shiny                1.14.0     [dl] (4.65 MB)
#> + sourcetools          0.1.7-2    [dl] (131.96 kB)
#> + SQUAREM              2026.1     [dl] (183.96 kB)
#> + stringi              1.8.7      [dl] (15.01 MB)
#> + stringr              1.6.0      [dl] (349.60 kB)
#> + timeDate             4052.112   [dl] (1.50 MB)
#> + urca                 1.3-4      [dl] (1.08 MB)
#> + utsf         1.3.3 → 1.3.3      [bld][cmp][dl] (GitHub: 6c1d35b)
#> + vctrs                0.7.3      [dl] (1.92 MB)
#> + vctsfr               0.1.1      [dl] (214.48 kB)
#> + viridisLite          0.4.3      [dl] (1.30 MB)
#> + withr                3.0.3      [dl] (235.25 kB)
#> + xgboost              3.2.1.1    [dl] (4.20 MB)
#> + xtable               1.8-8      [dl] (761.35 kB)
#> + zoo                  1.8-15     [dl] (1.06 MB)
#> ℹ Getting 70 pkgs (87.47 MB) and 1 pkg with unknown size, 1 (2.68 MB) cached
#> ✔ Cached copy of utsf 1.3.3 (source) is the latest build
#> ✔ Cached copy of Cubist 0.6.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of FNN 1.1.4.1 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of R6 2.6.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of RColorBrewer 1.1-3 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of Rcpp 1.1.2 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of S7 0.2.2 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of SQUAREM 2026.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of base64enc 0.1-6 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of bslib 0.11.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of cachem 1.1.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of cli 3.6.6 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of commonmark 2.0.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of data.table 1.18.4 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of diagram 1.6.5 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of digest 0.6.39 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of farver 2.1.2 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of fastmap 1.2.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of fontawesome 0.5.3 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of forecast 9.0.2 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of fracdiff 1.5-4 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of fs 2.1.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of future.apply 1.20.2 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of future 1.70.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of generics 0.1.4 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of ggplot2 4.0.3 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of globals 0.19.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of glue 1.8.1 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of gtable 0.3.6 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of htmltools 0.5.9 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of httpuv 1.6.17 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of ipred 0.9-15 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of isoband 0.3.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of jquerylib 0.1.4 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of jsonlite 2.0.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of labeling 0.4.3 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of later 1.4.8 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of lava 1.9.2 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of lifecycle 1.0.5 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of listenv 1.0.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of lmtest 0.9-40 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of magrittr 2.0.5 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of memoise 2.0.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of mime 0.13 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of numDeriv 2016.8-1.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of otel 0.2.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of parallelly 1.48.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of plyr 1.8.9 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of prodlim 2026.03.11 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of progressr 1.0.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of promises 1.5.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of ranger 0.18.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of rappdirs 0.3.4 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of reshape2 1.4.5 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of rlang 1.3.0 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of sass 0.4.10 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of scales 1.4.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of shape 1.4.6.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of shiny 1.14.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of sourcetools 0.1.7-2 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of stringi 1.8.7 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of stringr 1.6.0 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of timeDate 4052.112 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of urca 1.3-4 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of vctrs 0.7.3 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of vctsfr 0.1.1 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of viridisLite 0.4.3 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of withr 3.0.3 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of xgboost 3.2.1.1 (x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of xtable 1.8-8 (i386+x86_64-w64-mingw32) is the latest build
#> ✔ Cached copy of zoo 1.8-15 (x86_64-w64-mingw32) is the latest build
#> ✔ Installed utsf 1.3.3 (github::franciscomartinezdelrio/utsf@6c1d35b) (1.8s)
#> ✔ Installed Cubist 0.6.0  (1.9s)
#> ✔ Installed FNN 1.1.4.1  (2.1s)
#> ✔ Installed R6 2.6.1  (2.2s)
#> ✔ Installed RColorBrewer 1.1-3  (2.4s)
#> ✔ Installed S7 0.2.2  (2.6s)
#> ✔ Installed SQUAREM 2026.1  (2.7s)
#> ✔ Installed base64enc 0.1-6  (2.7s)
#> ✔ Installed cachem 1.1.0  (2.7s)
#> ✔ Installed cli 3.6.6  (3s)
#> ✔ Installed Rcpp 1.1.2  (4.4s)
#> ✔ Installed commonmark 2.0.0  (1.5s)
#> ✔ Installed colorspace 2.1-3  (2.2s)
#> ✔ Installed diagram 1.6.5  (1.6s)
#> ✔ Installed data.table 1.18.4  (2.2s)
#> ✔ Installed digest 0.6.39  (1.9s)
#> ✔ Installed farver 2.1.2  (2.2s)
#> ✔ Installed bslib 0.11.0  (6.9s)
#> ✔ Installed fastmap 1.2.0  (2.9s)
#> ✔ Installed fontawesome 0.5.3  (3.3s)
#> ✔ Installed forecast 9.0.2  (3.4s)
#> ✔ Installed fracdiff 1.5-4  (3.4s)
#> ✔ Installed fs 2.1.0  (3.6s)
#> ✔ Installed future.apply 1.20.2  (3.6s)
#> ✔ Installed generics 0.1.4  (3.3s)
#> ✔ Installed future 1.70.0  (4s)
#> ✔ Installed ggplot2 4.0.3  (3.7s)
#> ✔ Installed globals 0.19.1  (3.6s)
#> ✔ Installed glue 1.8.1  (3.5s)
#> ✔ Installed gtable 0.3.6  (3.5s)
#> ✔ Installed htmltools 0.5.9  (3.3s)
#> ✔ Installed httpuv 1.6.17  (3s)
#> ✔ Installed ipred 0.9-15  (2.8s)
#> ✔ Installed isoband 0.3.0  (2.7s)
#> ✔ Installed jquerylib 0.1.4  (2.6s)
#> ✔ Installed jsonlite 2.0.0  (2.5s)
#> ✔ Installed labeling 0.4.3  (2.5s)
#> ✔ Installed later 1.4.8  (2.5s)
#> ✔ Installed lava 1.9.2  (2.5s)
#> ✔ Installed lifecycle 1.0.5  (2.3s)
#> ✔ Installed listenv 1.0.0  (2.4s)
#> ✔ Installed lmtest 0.9-40  (2.4s)
#> ✔ Installed magrittr 2.0.5  (2.5s)
#> ✔ Installed memoise 2.0.1  (2.4s)
#> ✔ Installed mime 0.13  (2.3s)
#> ✔ Installed numDeriv 2016.8-1.1  (2.4s)
#> ✔ Installed otel 0.2.0  (2.4s)
#> ✔ Installed parallelly 1.48.0  (2.4s)
#> ✔ Installed plyr 1.8.9  (2.3s)
#> ✔ Installed prodlim 2026.03.11  (2.3s)
#> ✔ Installed progressr 1.0.0  (2.5s)
#> ✔ Installed promises 1.5.0  (2.7s)
#> ✔ Installed ranger 0.18.0  (2.7s)
#> ✔ Installed rappdirs 0.3.4  (2.7s)
#> ✔ Installed reshape2 1.4.5  (2.8s)
#> ✔ Installed rlang 1.3.0  (2.8s)
#> ✔ Installed sass 0.4.10  (2.8s)
#> ✔ Installed scales 1.4.0  (2.8s)
#> ✔ Installed shape 1.4.6.1  (2.8s)
#> ✔ Installed sourcetools 0.1.7-2  (2.6s)
#> ✔ Installed stringi 1.8.7  (2.6s)
#> ✔ Installed stringr 1.6.0  (2.5s)
#> ✔ Installed shiny 1.14.0  (3.3s)
#> ✔ Installed timeDate 4052.112  (2.2s)
#> ✔ Installed urca 1.3-4  (2.1s)
#> ✔ Installed vctrs 0.7.3  (1.9s)
#> ✔ Installed vctsfr 0.1.1  (1.7s)
#> ✔ Installed viridisLite 0.4.3  (1.6s)
#> ✔ Installed withr 3.0.3  (1.5s)
#> ✔ Installed xgboost 3.2.1.1  (1.4s)
#> ✔ Installed xtable 1.8-8  (1.3s)
#> ✔ Installed zoo 1.8-15  (1.3s)
#> ✔ 1 pkg + 81 deps: kept 9, upd 1, added 71 [29.5s]
```

or you can install the **stable** version from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("utsf")
#> Installing package into 'C:/Users/UJA/AppData/Local/Temp/RtmpUhCTs4/temp_libpath59086f02530a'
#> (as 'lib' is unspecified)
#> package 'utsf' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#>  C:\Users\UJA\AppData\Local\Temp\RtmpuwI3GV\downloaded_packages
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
#> 1985 1296.115 1231.491 1221.984 1174.882 1270.799 1270.905 1325.511 1347.810
#>           Sep      Oct      Nov      Dec
#> 1985 1415.184 1557.899 1700.445 1818.560
library(ggplot2)
autoplot(f)
```

<img src="man/figures/README-example-1.png" alt="" width="100%" />

If you are interested in this package you can read its
[vignette](https://cran.r-project.org/web/packages/utsf/vignettes/utsf.html)
where all its important features are described.
