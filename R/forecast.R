#'Train an univariate time series forecasting model and make forecasts
#'
#'This function trains a model from the historical values of a time series using
#'an autoregressive approach: the targets are the historical values and the
#'features of the targets their lagged values. Then, the trained model is used
#'to predict the future values of the series using a recursive strategy.
#'
#'The functions used to build and train the model are:
#' * KNN: In this case no model is built and the function [FNN::knn.reg()] is
#'used to predict the future values of the time series.
#' * Linear models: Function [stats::lm()] to build the model and the method
#'[stats::predict.lm()] associated with the trained model to forecast the future
#'values of the time series.
#' * Regression trees: Function [rpart::rpart()] to build the model and the
#'method [rpart::predict.rpart()] associated with the trained model to forecast
#'the future values of the time series.
#' * Model trees: Function [Cubist::cubist()] to build the model and the
#'method [Cubist::predict.cubist()] associated with the trained model to
#'forecast the future values of the time series.
#' * Bagging: Function [ipred::bagging()] to build the model and the
#'method [ipred::predict.regbagg()] associated with the trained model to
#'forecast the future values of the time series.
#' * Random forest: Function [ranger::ranger()] to build the model and the
#'method [ranger::predict.ranger()] associated with the trained model to
#'forecast the future values of the time series.
#'
#'@param timeS A time series of class `ts` or a numeric vector.
#'@param h A positive integer. Number of values to be forecast into the future,
#'  i.e., forecast horizon.
#'@param lags An integer vector, in increasing order, expressing the lags used
#'  as autoregressive variables. If the default value (`NULL`) is provided, a
#'  suitable vector is chosen.
#'@param method A string indicating the method used for training and
#'  forecasting. Allowed values are:
#'   * `"knn"`: k-nearest neighbors (the default)
#'   * `"lm"`: linear regression
#'   * `"rt"`: regression trees
#'   * `"mt"`:  model trees
#'   * `"bagging"`
#'   * `"rf"`: random forests.
#'
#'  See details for a brief explanation of the models. It is also possible to
#'  use your own regression model, in that case a function explaining how to
#'  build your model must be provided, see the vignette for further details.
#'@param param A list with parameters for the underlying function that builds
#'  the model. If the default value (`NULL`) is provided, the model is fitted
#'  with its default parameters. See details for the functions used to train the
#'  models.
#'@param efa It is used to indicate how to estimate the forecast accuracy of the
#'  model using the last observations of the time series as test set. If the
#'  default value (`NULL`) is provided, no estimation is done. To specify the
#'  size of the test set the [evaluation()] function must be used.
#'
#'@param preProcess A list indicating the preprocessings or transformations.
#'  Currently, the length of the list must be 1 (only one preprocessing). If
#'  `NULL` the additive transformation is applied to the series. The element of
#'  the list is created with the [trend()] function.
#'
#'@param tuneGrid A data frame with possible tuning values. The columns are
#'  named the same as the tuning parameters. The estimation of forecast accuracy
#'  is done as explained for the `efa` parameter. Rolling or fixed origin
#'  evaluation is done according to the value of the `efa` parameter (fixed if
#'  NULL). The best combination of parameters is used to train the model with
#'  all the historical values of the time series.
#'@returns An S3 object of class `utsf`, basically a list with, at least, the
#'  following components: \item{`ts`}{The time series being forecast.}
#'  \item{`features`}{A data frame with the features of the training set. The
#'   column names of the data frame indicate the autoregressive lags.}
#'  \item{`targets`}{A vector with the targets of the training set.}
#'  \item{`lags`}{An integer vector with the autoregressive lags.}
#'  \item{`model`}{The regression model used recursively to make the forecast.}
#'  \item{`pred`}{An object of class `ts` and length `h` with the forecast.}
#'  \item{`efa`}{This component is included if forecast accuracy is estimated.
#'  A vector with estimates of forecast accuracy according to different
#'  forecast accuracy measures.}
#'  \item{`tuneGrid`}{This component is included if the tuneGrid parameter has
#'  been used. A data frame in which each row contains estimates of forecast
#'  accuracy for a combination of tuning parameters.}
#'@export
#'
#' @examples
#' ## Forecast time series using k-nearest neighbors
#' f <- forecast(AirPassengers, h = 12, method = "knn")
#' f$pred
#' library(ggplot2)
#' autoplot(f)
#'
#' ## Using k-nearest neighbors changing the default k value
#' forecast(AirPassengers, h = 12, method = "knn", param = list(k = 5))$pred
#'
#' ## Using your own regression model
#'
#' # Function to build the regression model
#' my_knn_model <- function(X, y) {
#'   structure(list(X = X, y = y), class = "my_knn")
#'}
#' # Function to predict a new example
#' predict.my_knn <- function(object, new_value) {
#'   FNN::knn.reg(train = object$X, test = new_value, y = object$y)$pred
#' }
#' forecast(AirPassengers, h = 12, method = my_knn_model)$pred
#'
#' ## Estimating forecast accuracy of the model
#' f <- forecast(UKgas, h = 4, lags = 1:4, method = "rf", efa = evaluation("minimum"))
#' f$efa
#'
#' ## Estimating forecast accuracy of different tuning parameters
#' f <- forecast(UKgas, h = 4, lags = 1:4, method = "knn", tuneGrid = expand.grid(k = 1:5))
#' f$tuneGrid
#'
#' ## Forecasting a trending series
#' # Without any preprocessing or transformation
#' f <- forecast(airmiles, h = 4, method = "knn", preProcess = list(trend("none")))
#' autoplot(f)
#'
#' # Applying the additive transformation (default)
#' f <- forecast(airmiles, h = 4, method = "knn")
#' autoplot(f)
forecast <- function(timeS, 
                     h, 
                     lags = NULL, 
                     method = "knn", 
                     param = NULL,
                     efa = NULL,
                     tuneGrid = NULL,
                     preProcess = NULL) {
  # Check timeS parameter
  if (! (stats::is.ts(timeS) || is.vector(timeS, mode = "numeric")))
    stop("timeS parameter should be of class ts or a numeric vector")
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)
  
  # Check h parameter
  if (! (is.numeric(h) && length(h) == 1 && h >= 1 && floor(h) == h))
    stop("h parameter should be an integer scalar value >= 1")
  
  # Check preProcess parameter
  if (! (is.null(preProcess) || 
         is.list(preProcess) && length(preProcess) == 1 && inherits(preProcess[[1]], "trend")))
    stop("parameter preProcess must be NULL or a list of length 1 with a valid value")

    # Check lags parameter
  lagsc <- lags
  if (! (is.null(lagsc) || is.vector(lagsc, mode = "numeric"))) {
    stop("lags parameter should be NULL or numeric")
  }
  if (is.null(lagsc)) {
    if (stats::frequency(timeS) > 1) {
      lagsc <- 1:stats::frequency(timeS)
    } else {
      partial <- stats::pacf(timeS, plot = FALSE)
      lagsc <- which(partial$acf > 2/ sqrt(length(timeS)))
      if (length(lagsc) == 0 ||
          (length(lagsc) == 1 && 
           what_preprocess(preProcess) %in% c("additive", "multiplicative"))) {
        lagsc = 1:5
      }
    }
  }
  if (is.unsorted(lagsc)) stop("lags should be a vector in increasing order")
  if (lagsc[1] < 1) stop("lags values should be equal or greater than cero")
  
  if ((length(lagsc) == 1 && 
       what_preprocess(preProcess) %in% c("additive", "multiplicative"))) {
    stop("It does not make sense to use only 1 autoregressive lag with the additive or multiplicative transformation")
  }
  
  # Check method parameter
  if (length(method) != 1)
    stop("parameter method cannot be a vector")
  if (! class(method) %in% c("function", "character"))
    stop("parameter method should be a function or a string")
  if (inherits(method, "character") && !(method %in% c("knn", "lm", "rt", "mt", "bagging", "rf")))
    stop(paste("parameter method: method", method, "not supported"))
  
  # Check param parameter
  if (! (is.null(param) || is.list(param)))
    stop("param argument should be a list")
  
  # Check efa parameter
  if (!(is.null(efa) || inherits(efa, "evaluation")))
    stop("parameter efa should be NULL or created with function evaluation()")
  if (inherits(efa, "evaluation") && efa$type == "normal" && !is.null(efa$size) && efa$size < h)
    stop("The size of the test set cannot be lower than h")
  if (inherits(efa, "evaluation") && efa$type == "normal" && !is.null(efa$size) && 
      efa$size >= length(timeS)-1)
    stop("The size of the test set is too large")
  if (inherits(efa, "evaluation") && efa$type == "normal" && !is.null(efa$prop)) {
    size = trunc(length(timeS)*efa$prop)
    if (size < h)
      stop(paste0("The size of the test set (", size, ") cannot be lower than h\n"))
  }
    
  # Check tuneGrid parameter
  if (! (is.null(tuneGrid) || is.data.frame(tuneGrid)))
    stop("parameter tuneGrid should be NULL or a data frame")
  
  # Check one of tuneGrid or param is NULL
  if (!is.null(param) && !is.null(tuneGrid))
    stop("either param or tuneGrid parameter should be NULL")
  
  # Create training set and targets / transformations / preprocessing
  if (what_preprocess(preProcess) == "differences") {
    preprocessing_fd <- fd_preprocessing(timeS, preProcess[[1]]$n)
    if (preprocessing_fd$differences == 0) {
      out <- build_examples(timeS, rev(lagsc))
    } else {
      out <- build_examples(preprocessing_fd$preprocessed, rev(lagsc))
    }
    out$differences <- preprocessing_fd
  } else {
    out <- build_examples(timeS, rev(lagsc))
    if (what_preprocess(preProcess) == "additive") {
      means <- rowMeans(out$features)
      out$features <- sapply(1:nrow(out$features),
                             function(row) out$features[row, ] - means[row])
      out$features <- as.data.frame(t(out$features))
      out$targets <- out$targets - means
      # means <- rowMeans(out$features[, 1:length(lagsc)])
      # out$features[ , 1:length(lagsc)] <- sapply(1:nrow(out$features),
      #                        function(row) out$features[row, 1:length(lagsc)] - means[row])
      # out$features <- as.data.frame(out$features)
      # out$targets <- out$targets - means
    } else if (what_preprocess(preProcess) == "multiplicative") {
      means <- rowMeans(out$features)
      out$features <- sapply(1:nrow(out$features),
                             function(row) out$features[row, ] / means[row])
      out$features <- as.data.frame(t(out$features))
      out$targets <- out$targets / means
    }
  }
  if (!is.data.frame(out$features)) out$features <- as.data.frame(out$features)
  # Add other information to the output object
  out$call <- match.call()
  out$ts <- timeS
  out$lags <- lagsc
  out$preProcess <- preProcess
  out$param <- param
  
  # Use grid search 
  if (!is.null(tuneGrid)) {
    result <- do_tuneGrid(timeS = timeS,
                          h = h,
                          lags = lags,
                          method = method,
                          tuneGrid = tuneGrid,
                          preProcess = preProcess,
                          type = efa
    )
    out$tuneGrid <- result
    out$param <- as.list(result[which.min(result$RMSE), 1:ncol(tuneGrid), drop = FALSE])
  }
  # Create/train the model
  if (inherits(method, "function")) {
    # model provided by the user
    args <- c(list(X = out$features, y = out$targets), param)
    out$model <- do.call(method, args = args)
  } else {
    # model supported by the package
    out$model <- build_model(out$features, out$targets, method, out$param)
  }
  
  out$method <- method
  class(out) <- "utsf"
  # Prediction
  out$pred <- recursive_prediction(out, h = h)
  if (what_preprocess(preProcess) == "differences" && preprocessing_fd$differences > 0) {
    out$pred <- fd_unpreprocessing(out$pred, preprocessing_fd)
  }
  # Estimate forecast accuracy
  if (!is.null(efa) && is.null(tuneGrid)){
    out$efa <- estimate_accuracy(timeS = timeS, 
                                 h = h, 
                                 lags = lags,
                                 method = method, 
                                 param = param,
                                 preProcess = preProcess, 
                                 type = efa
    )
  }
  out
}

# return the value associated with preprocessing: "none", "additive, 
# "multiplicative" or "differences"
what_preprocess <- function(preProcess) {
  if (is.null(preProcess)) return("additive")
  return(preProcess[[1]]$type)
}

# @param object S3 object of class utsf
predict_one_value_transforming <- function(object, example) {
  if (what_preprocess(object$preProcess) == "additive") {
    mean_ <- mean(example)
    example <- example - mean_
    # mean_ <- mean(head(example, length(object$lags))) # añadido
    # example[seq_along(object$lags)] <- example[seq_along(object$lags)] - mean_ # añadido
  } else if (what_preprocess(object$preProcess) == "multiplicative") {
    mean_ <- mean(example)
    example <- example / mean_
  }
  example <- as.data.frame(matrix(example, ncol = length(example)))
  colnames(example) <- colnames(object$features)
  # print(example)
  if (inherits(object$method,"character")) {
    r <- stats::predict(object, example)
  } else {
    r <- stats::predict(object$model, example)
  }
  if (what_preprocess(object$preProcess) == "additive") {
    r <- r + mean_
  } else if (what_preprocess(object$preProcess) == "multiplicative") {
    r <- r * mean_
  }
  r
}

