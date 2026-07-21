# utsf 1.3.4
* The vignette has been improved
* README file improved
* In efa(), now the size of the validation set cannot be lower than the forecasting horizon

# utsf 1.3.3

* The param parameter in the regression function provided by the user is 
  changed to a ... parameter.
* Extreme gradient boosting is supported using the xgboost package.  

# utsf 1.3.2
* The model tree is now built with the parameter committees set to 5
* The param parameter used to set arguments to the regression models in the
  create_model function is no longer needed, you can set the arguments directly.

# utsf 1.3.1

* The pre-processing for dealing with trending series is now specified in a 
  simpler way.
* The vignette has been improved.
* The way in which tuning parameters are specified to user's models has changed.

# utsf 1.3.0

* The lags argument in the function for building the model (now create_model) 
  now can be an unordered integer vector.
* The lags argument in the function for building the model (now create_model) 
  now must be an integer vector.
* match.arg() is used so the options are visible to the user in the help.  
* A main change is that the functionality of the forecast function, that did
  a lot of things, is now distributed in several functions: create_model()
  (build the model), forecast() (do the forecasts), efa() for assessing
  forecast accuracy and tune_grid() for parameter tuning.
* Prediction intervals are optionally computed.
  
# utsf 1.2.1

* The default value of parameter transform_features in trend function is
  again TRUE.

# utsf 1.2.0

* The estimated forecast accuracy per horizon is also computed.
* Now it is possible to use only 1 lag with additive or multiplicative
  transformation, if the features are not transformed.
* Now it is possible to transform only the target (and not the features) 
  with the multiplicative transformation.
* An error is produced if a too large autorregresive lag is used.
* An error is produced in method KNN when k is greater than the size of the
  training set.
* A warning is produced when the time series is too short to estimate
  forecast accuracy.

# utsf 1.1.0

* Improvements in estimation of forecast accuracy with rolling origin evaluation.
* The way in which pre-processings are specified has changed.
* Method plot.utsf is implemented.
* Linear models (stats::lm) are supported.

# utsf 1.0.0

* Initial CRAN submission.
