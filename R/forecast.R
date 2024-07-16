#' Fit an univariate time series forecasting model and make forecasts
#'
#' This function trains a model from the historical values of a time series
#' using as targets the historical values and as features of targets their
#' lagged values. Then, the fitted model is used to predict the future values of
#' the series using a recursive strategy.
#'
#' The functions used to build and train the model are:
#' * KNN: In this case no model is built and the function [FNN::knn.reg()] is
#' used to predict the future values of the time series.
#' * Regression trees: Function [rpart::rpart()] to build the model and the
#' method [predict.rpart()] associated with the trained model to forecast the
#' future values of the time series.
#' * Model trees: Function [Cubist::cubist()] to build the model and the
#' method [predict.cubist()] associated with the trained model to forecast the
#' future values of the time series.
#' * Bagging: Function [ipred::bagging()] to build the model and the
#' method [predict.regbagg()] associated with the trained model to forecast the
#' future values of the time series.
#' * Random forest: Function [ranger::ranger()] to build the model and the
#' method [predict.ranger()] associated with the trained model to forecast the
#' future values of the time series.
#'
#' @param timeS A time series of class `ts` or a numeric vector.
#' @param h A positive integer. Number of values to be forecast into the future,
#'   i.e., forecast horizon.
#' @param lags An integer vector, in increasing order, expressing the lags used
#'   as autoregressive variables. If the default value (`NULL`) is provided, a
#'   suitable vector is chosen.
#' @param method A string indicating the method used for training and
#'   forecasting. Allowed values are:
#'   * `"knn"`: k-nearest neighbors (the default)
#'   * `"rt"`: regression trees
#'   * `"mt"`:  model trees
#'   * `"bagging"`
#'   * `"rf"`: random forest.
#'
#'   See details for a brief explanation of the models.
#' @param param A list with parameters for the underlying function that builds
#'   the model. If the default value (`NULL`) is provided, the model is built
#'   with its default parameters. See details for the functions used to train
#'   the models.
#' @param transform A character value indicating whether the training samples
#'   are transformed. If the time series has a trend it is recommended. By
#'   default is `"additive"` (additive transformation). It is also possible a
#'   multiplicative transformation or no transformation.
#'
#' @returns An S3 object of class `utsf`, basically a list with, at least, the
#'   following components: \item{`ts`}{The time series being forecast.}
#'  \item{`features`}{A data frame with the features of the training set. The
#'   column names of the data frame indicate the autoregressive lags.}
#'   \item{`targets`}{A vector with the targets of the training set.}
#'   \item{`lags`}{An integer vector with the autoregressive lags.}
#'   \item{`model`}{The regression model used recursively to make the forecast.}
#'   \item{`pred`}{An object of class `ts` and length `h` with the forecast.}
#' @export
#'
#' @examples
#' ## Forecast time series using k-nearest neighbors
#' forecast(AirPassengers, h = 12, method = "knn")$pred
#' 
#' ## Forecast time series using k-nearest neighbors changing the default k
#' forecast(AirPassengers, h = 12, method = "knn", param = list(k = 5))$pred
forecast <- function(timeS, h, lags = NULL, method = "knn", param = NULL,
                     transform = "additive") {
  # Check timeS parameter
  if (! (stats::is.ts(timeS) || is.vector(timeS, mode = "numeric")))
    stop("timeS parameter should be of class ts or a numeric vector")
  if (! stats::is.ts(timeS))
    timeS <- stats::as.ts(timeS)
  
  # Check h parameter
  if (! (is.numeric(h) && length(h) == 1 && h >= 1 && floor(h) == h))
    stop("h parameter should be an integer scalar value >= 1")
  
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
          (length(lagsc) == 1 && transform %in% c("additive", "multiplicative"))) {
        lagsc = 1:5
      }
    }
  }
  if (is.unsorted(lagsc)) stop("lags should be a vector in increasing order")
  if (lagsc[1] < 1) stop("lags values should be equal or greater than cero")
  
  if ((length(lagsc) == 1 && transform %in% c("additive", "multiplicative"))) {
    stop("It does not make sense to use only 1 autoregressive lag with the additive or multiplicative transformation")
  }
  
  # Check method parameter
  if (! (method %in% c("knn", "rt", "mt", "bagging", "rf")))
    stop(paste("parameter method: method", method, "not supported"))
  
  # Check param parameter
  if (! (is.null(param) || is.list(param)))
    stop("param argument should be a list")
  
  # Check transform parameter
  if (! (transform %in% c("additive", "multiplicative", "none")))
    stop("parameter transform has a non-supported value")
  
  # Create the examples (training and test sets)
  out <- build_examples(timeS, rev(lagsc))
  if (transform == "additive") {
    means <- rowMeans(out$features)
    out$features <- sapply(1:nrow(out$features),
                             function(row) out$features[row, ] - means[row])
    out$features <- as.data.frame(t(out$features))
    out$targets <- out$targets - means
  } else if (transform == "multiplicative") {
    means <- rowMeans(out$features)
    out$features <- sapply(1:nrow(out$features),
                             function(row) out$features[row, ] / means[row])
    out$features <- as.data.frame(t(out$features))
    out$targets <- out$targets / means
  }
  
  # Add other information to the output object
  out$ts <- timeS
  out$lags <- lagsc
  out$transform <- transform
  out$param <- param
  
  # Create the model
  out$model <- build_model(out$features, out$targets, method, out$param)
  
  # Make forecasts
  out$predict_one_value <- switch (method,
                                     "knn" = predict_one_value_knn,
                                     "rt" = predict_one_value_rt,
                                     "mt" = predict_one_value_rt,
                                     "bagging" = predict_one_value_rt,
                                     "rf" = predict_one_value_rf
  )
  out$pred <- recursive_prediction(out, h = h)
  
  # Evaluate forecast accuracy with training-test sets
  # if (! is.null(forecast_est)) {
  #   out$forecas_est <- compute_accuracy(timeS = timeS, h = h, lags = lags,
  #                                         method = method, param = param,
  #                                         transform = transform, type = forecast_est)
  # }
  class(out) <- "utsf"
  out
}

predict_one_value_transforming <- function(model, example) {
  if (model$transform == "additive") {
    mean_ <- mean(example)
    example <- example - mean_
  } else if (model$transform == "multiplicative") {
    mean_ <- mean(example)
    example <- example / mean_
  }
  r <- model$predict_one_value(model, example) # global function
  if (model$transform == "additive") {
    r <- r + mean_
  } else if (model$transform == "multiplicative") {
    r <- r * mean_
  }
  r
}

# How to predict next future value with knn
# param model An object of class utsf
predict_one_value_knn <- function(model, example) {
  check_param(model, FNN::knn.reg, "FNN::knn.reg")
  args <- list(train = model$features,
               test = example,
               y = model$targets)
  args <- c(args, model$param)
  do.call(FNN::knn.reg, args = args)$pred
}

# How to predict next future value (just one value) with regression trees
# param model An object of class utsf
predict_one_value_rt <- function(model, example) {
  example <- as.data.frame(matrix(example, ncol = length(example)))
  colnames(example) <- colnames(model$features)
  stats::predict(model$model, example)
}

# How to predict next future value with random forest
# param model An object of class utsf
predict_one_value_rf <- function(model, example) {
  example <- as.data.frame(matrix(example, ncol = length(example)))
  colnames(example) <- colnames(model$features)
  stats::predict(model$model, example)$predictions
}

# Check that parameters provided by the user for customizing model building belong 
# to building function. If not, execution is stopped
# param object An object of class utsf
# param f An object of class function. The function 
# param fname A string. The name of the function
check_param <- function(object, f, fname) {
  formal <- methods::formalArgs(f)
  dif <- setdiff (names(object$param), formal)
  if (length(dif) > 0)
    stop(paste("Error in \"param\" argument, parameters", dif, "are not part of",
         fname, "function"))
}

