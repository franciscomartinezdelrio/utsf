#'Train an univariate time series forecasting model
#'
#'This function trains a model from the historical values of a time series using
#'an autoregressive approach: the targets are the historical values and the
#'features of the targets their lagged values.
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
#'   * `"rf"`: random forests
#'
#'  See details for a brief explanation of the models. It is also possible to
#'  use your own regression model, in that case a function explaining how to
#'  build your model must be provided, see the vignette for further details.
#'@param param A list with parameters for the underlying function that builds
#'  the model. If the default value (`NULL`) is provided, the model is fitted
#'  with its default parameters. See details for the functions used to train the
#'  models.
#'
#'@param trend A character indicating the type of preprocessing applied to the
#'  time series in order to deal with trending series, see the vignette for
#'  details.
#'
#'@param nfd In the case that the parameter `trend` has the value "differences",
#'  it specifies the order of first differences to be applied. If the default
#'  (-1) is used, the order of first differences needed by the time series will
#'  be estimated by the [forecast::ndiffs()] function.
#'
#'@param transform_features A logical value indicating whether the training
#'  features are also transformed if the additive or multiplicative
#'  transformation has been used as preprocessing to deal with trending series.
#'
#'@returns An S3 object of class `utsf`, basically a list with, at least, the
#'  following components: \item{`ts`}{The time series being forecast.}
#'  \item{`features`}{A data frame with the features of the training set. The
#'   column names of the data frame indicate the autoregressive lags.}
#'  \item{`targets`}{A vector with the targets of the training set.}
#'  \item{`lags`}{An integer vector with the autoregressive lags.}
#'  \item{`model`}{The regression model used recursively to make the forecast.}
#'@export
#'
#' @examples
#' ## Build model using k-nearest neighbors
#' create_model(AirPassengers, method = "knn")
#'
#' ## Using k-nearest neighbors changing the default k value
#' create_model(AirPassengers, method = "knn", param = list(k = 5))
#'
#' ## Using your own regression model
#'
#' # Function to build the regression model
#' my_knn_model <- function(X, y, param) {
#'   structure(list(X = X, y = y), class = "my_knn")
#'}
#' # Function to predict a new example
#' predict.my_knn <- function(object, new_value) {
#'   FNN::knn.reg(train = object$X, test = new_value, y = object$y)$pred
#' }
#' create_model(AirPassengers, method = my_knn_model)
#'
create_model <- function(timeS, 
                         lags = NULL, 
                         method = c("knn", "lm", "rt", "mt", "bagging", "rf"), 
                         param = NULL,
                         trend = c("additive", "multiplicative", "differences", "none"),
                         nfd = -1,
                         transform_features = TRUE) {
  # Check timeS parameter
  if (! (stats::is.ts(timeS) || is.vector(timeS, mode = "numeric")))
    stop("timeS parameter should be of class ts or a numeric vector")
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)
  
  trend <- match.arg(trend)
  
  # Check lags parameter
  lagsc <- lags
  if (! (is.null(lagsc) || is.vector(lagsc, mode = "numeric"))) {
    stop("lags parameter should be NULL or numeric")
  }
  if (!is.null(lagsc) && !all(lagsc == floor(lagsc))) {
    stop("lag values should be integer")
  }
  if (is.null(lagsc)) {
    if (stats::frequency(timeS) > 1) {
      lagsc <- 1:stats::frequency(timeS)
    } else {
      partial <- stats::pacf(timeS, plot = FALSE)
      lagsc <- which(partial$acf > 2/ sqrt(length(timeS)))
      if (length(lagsc) == 0 ||
          (length(lagsc) == 1 &&
           trend %in% c("additive", "multiplicative"))) {
        lagsc <- 1:5
      }
    }
  }
  if (is.unsorted(lagsc)) lagsc <- sort(lagsc)
  if (lagsc[1] < 1) stop("lag values should be greater than zero")
  
  if ((length(lagsc) == 1 && trend %in% c("additive", "multiplicative")) &&  transform_features) {
    stop("It does not make sense to use only 1 autoregressive lag with the additive or multiplicative transformation of features")
  }
  
  if (utils::tail(lagsc, 1) >= length(timeS)) {
    stop("Maximum lag cannot be greater or equal to the length of the series")
  }
  
  # Check method parameter
  tryCatch(method <- match.arg(method),
           error = function(cond) {
             if (length(method) != 1)
               stop("parameter method cannot be a vector")
             if (! class(method) %in% "function")
               stop("parameter method should be a function or valid method")
             method <- method
           }
  )

  # Check param parameter
  if (! (is.null(param) || is.list(param)))
    stop("param argument should be a list")
  
  # Check nfd parameter
  if (trend == "differences") {
    if (! (is.numeric(nfd) && length(nfd) == 1 && nfd >= -1 && floor(nfd) == nfd))
      stop("nfd parameter should be an integer scalar value >= -1")
  }
  
  # Check transform_features parameter
  if (! (is.logical(transform_features) && length(transform_features) == 1))
    stop("transform_features parameter should be a logical value")
  
  
  # Create training set and targets / transformations / preprocessing
  if (trend == "differences") {
    preprocessing_fd <- fd_preprocessing(timeS, nfd)
    if (preprocessing_fd$differences == 0) {
      out <- build_examples(timeS, rev(lagsc))
    } else {
      out <- build_examples(preprocessing_fd$preprocessed, rev(lagsc))
    }
    out$differences <- preprocessing_fd
  } else {
    out <- build_examples(timeS, rev(lagsc))
    if (trend == "additive") {
      means <- rowMeans(out$features)
      if (transform_features) { 
        out$features <- sapply(1:nrow(out$features),
                               function(row) out$features[row, ] - means[row])
        out$features <- t(out$features)
      }
      out$features <- as.data.frame(out$features)
      out$targets <- out$targets - means
      # means <- rowMeans(out$features[, 1:length(lagsc)])
      # out$features[ , 1:length(lagsc)] <- sapply(1:nrow(out$features),
      #                        function(row) out$features[row, 1:length(lagsc)] - means[row])
      # out$features <- as.data.frame(out$features)
      # out$targets <- out$targets - means
    } else if (trend == "multiplicative") {
      means <- rowMeans(out$features)
      if (transform_features) {
        out$features <- sapply(1:nrow(out$features),
                               function(row) out$features[row, ] / means[row])
        out$features <- t(out$features)
      }
      out$features <- as.data.frame(out$features)
      out$targets <- out$targets / means
    }
  }
  if (!is.data.frame(out$features)) out$features <- as.data.frame(out$features)
  
  # Add other information to the output object
  out$call <- match.call()
  out$ts <- timeS
  out$lags <- lagsc
  out$trend <- trend
  if (trend == "differences") {
    out$nfd <- nfd
  }
  if (trend %in% c("additive", "multiplicative")) {
    out$transform_features <- transform_features
  }
  out$param <- param
  
  # Create/train the model
  if (inherits(method, "function")) {
    # model provided by the user
    args <- list(X = out$features, y = out$targets, param = param)
    out$model <- do.call(method, args = args)
  } else {
    # model supported by the package
    out$model <- build_model(out$features, out$targets, method, out$param)
  }
  
  out$method <- method
  class(out) <- "utsf"
  
  out
}

#'Forecasting a time series
#'
#'@param object an object of class `utsf` embedding a forecasting model for a
#'  time series.
#'@param h A positive integer. Number of values to be forecast into the future,
#'  i.e., forecast horizon.
#'@param PI If TRUE, prediction intervals are produced using simulation and
#'  assuming normally distributed errors.
#'@param level Confidence level for predictions intervals.  
#'@param ... Other arguments passed to methods
#'
#'@returns an object of class `utsf_forecast` with the same components of the
#'  model received as first argument, plus several components:
#'  \item{`pred`}{The forecast as an `ts` object.}
#'  \item{`lower`}{Lower limits for prediction interval.}
#'  \item{`upper`}{Upper limits for prediction interval.}
#'  \item{`level`}{Confidence value associated with the prediction interval}
#' @examples
#' ## Forecast time series using k-nearest neighbors
#' m <- create_model(USAccDeaths, method = "knn")
#' f <- forecast(m, h = 12)
#' f$pred
#' library(ggplot2)
#' autoplot(f)
#'
#' ## Using k-nearest neighbors changing the default k value
#' m <- create_model(USAccDeaths, method = "knn", param = list(k = 5))
#' forecast(m, h = 12)
#'
#' ## Using your own regression model
#'
#' # Function to build the regression model
#' my_knn_model <- function(X, y, param) {
#'   structure(list(X = X, y = y), class = "my_knn")
#'}
#' # Function to predict a new example
#' predict.my_knn <- function(object, new_value) {
#'   FNN::knn.reg(train = object$X, test = new_value, y = object$y)$pred
#' }
#' m <- create_model(USAccDeaths, method = my_knn_model)
#' forecast(m, h = 12)
#'@export
forecast.utsf <- function(object, h, PI = FALSE, level = 90, ...) {
  # Check h parameter
  if (! (is.numeric(h) && length(h) == 1 && h >= 1 && floor(h) == h))
    stop("h parameter should be an integer scalar greater than zero")

  # Check PI parameter
  if (! (is.logical(PI) && length(PI) == 1))
    stop("PI parameter should be a logical value")

  # Check level parameter
  if (! (is.numeric(level) && length(level) == 1 && level > 0 && level <= 100))
    stop("Confidence limit out of range")
  
  if (level < 1) level <- level * 100
  
  out <- object
  out$pred <- recursive_prediction(out, h = h)
  if (out$trend == "differences" && out$differences$differences > 0) {
    out$pred <- fd_unpreprocessing(out$pred, out$differences)
  }
  if (PI) {
    s <- try(simulations(object, h, level), silent = TRUE)
    if (inherits(s, "try-error")) {
      warning("Series too short to compute prediction intervals.")
    } else {
      out$lower <- stats::ts(s[1, ], start = stats::start(out$pred), frequency = stats::frequency(out$pred))
      out$upper <- stats::ts(s[2, ], start = stats::start(out$pred), frequency = stats::frequency(out$pred))
      out$level <- level
    }
  }
  class(out) <- "utsf_forecast"
  out
}

# @param object S3 object of class utsf
predict_one_value_transforming <- function(object, example) {
  if (object$trend == "additive") {
    mean_ <- mean(example)
    if (object$transform_features) {
      example <- example - mean_
    }
    # mean_ <- mean(head(example, length(object$lags))) # añadido
    # example[seq_along(object$lags)] <- example[seq_along(object$lags)] - mean_ # añadido
  } else if (object$trend == "multiplicative") {
    mean_ <- mean(example)
    if (object$transform_features) {
      example <- example / mean_
    }
  }
  example <- as.data.frame(matrix(example, ncol = length(example)))
  colnames(example) <- colnames(object$features)
  # print(example)
  if (inherits(object$method,"character")) {
    r <- stats::predict(object, example)
  } else {
    r <- stats::predict(object$model, example)
  }
  if (object$trend == "additive") {
    r <- r + mean_
  } else if (object$trend == "multiplicative") {
    r <- r * mean_
  }
  r
}

  