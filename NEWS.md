# utsf (development version)

* Now it is possible to transform only the target (and not the features) 
  with the multiplicative transformation.
* An error is produced if a too large autorregresive lag is used.
* An error is produced in method KNN when k is greater than the size of the
  training set.
* A warning is produced when the time series is too short to estimate
  forecast accuracy.

# utsf 1.1.0

* Improvements in estimation of forecast accuracy with rolling origin evaluation.
* The way in which preprocessings are specified has changed.
* Method plot.utsf is implemented.
* Linear models (stats::lm) are supported.

# utsf 1.0.0

* Initial CRAN submission.
