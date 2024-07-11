#' Fit an univariate time series forecasting model and make forecasts
#'
#' This function trains a model from the historical values of a time series
#' using as targets the historical values and as features of targets their
#' lagged values. Then, the fitted model is used to predict the future values of
#' the series using a recursive strategy.
#'
#' The functions used to build and train the model are:
#' * KNN: In this case no model is built and the function [FNN::knn.reg()] is
#'   used to predict the future values of the time series.
#' * Regression trees: Function [rpart::rpart()] to build the model and the
#'   method [predict.rpart()] associated with the trained model to forecast the
#'   future values of the time series.
#' * Model trees: Function [Cubist::cubist()] to build the model and the
#'   method [predict.cubist()] associated with the trained model to forecast the
#'   future values of the time series.
#' * Bagging: Function [ipred::bagging()] to build the model and the
#'   method [predict.regbagg()] associated with the trained model to forecast 
#'   the future values of the time series.
#' * Random forest: Function [ranger::ranger()] to build the model and the
#'   method [predict.ranger()] associated with the trained model to forecast 
#'   the future values of the time series.
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
#'   with its default parameters.
#' @param transform A character value indicating whether the training samples
#'   are transformed. If the time series has a trend it is recommended. By
#'   default is `"additive"` (additive transformation). It is also possible a
#'   multiplicative transformation or no transformation.
#'
#' @return A list with values
#' @export
#'
#' @examples
#' ## Forecast time series using k-nearest neighbors
#' forecast(AirPassengers, h = 12, method = "knn")$pred
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
  
  # Create the examples
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
  
  # Add other information to the object
  out$ts <- timeS
  out$lags <- lagsc
  out$transform <- transform
  out$param <- param
  
  # Create the model
  if (method == "rt") {
    df <- cbind(out$features, targets = out$targets)
    args <- list(formula = targets ~ .,
                 data = df,
                 method = "anova")
    args <- c(args, out$param)
    out$model <- do.call(rpart::rpart, args = args)
  } else if (method == "mt") {
    args <- list(x = as.data.frame(out$features),
                 y = out$targets
    )
    args <- c(args, out$param)
    out$model <- do.call(Cubist::cubist, args = args)
  } else if (method == "bagging") {
    df <- cbind(out$features, targets = out$targets)
    args <- list(formula = targets ~ .,
                 data = df
    )
    args <- c(args, out$param)
    out$model <- do.call(ipred::bagging, args = args)
  } else if (method == "rf") { # random forest
    df <- cbind(out$features, targets = out$targets)
    args <- list(formula = targets ~ .,
                 data = df,
                 mtry = floor((ncol(df)-1)/3)
    )
    args <- args[!(names(args) %in% names(out$param))]
    args <- c(args, out$param)
    out$model <- do.call(ranger::ranger, args = args)
  }
  
  
  # Do forecast
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
predict_one_value_knn <- function(model, example) {
  # f <- methods::formalArgs(FNN:knn.reg)
  # if (length(setdiff (model$param, f)) > 0)
  #   stop("parameter")
  args <- list(train = model$features,
               test = example,
               y = model$targets)
  args <- c(args, model$param)
  do.call(FNN::knn.reg, args = args)$pred
}

# How to predict next future value with regression trees
predict_one_value_rt <- function(model, example) {
  example <- as.data.frame(matrix(example, ncol = length(example)))
  colnames(example) <- colnames(model$features)
  stats::predict(model$model, example)
}

# How to predict next future value with random forest
predict_one_value_rf <- function(model, example) {
  example <- as.data.frame(matrix(example, ncol = length(example)))
  colnames(example) <- colnames(model$features)
  stats::predict(model$model, example)$predictions
}

