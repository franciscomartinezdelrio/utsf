#' Title
#'
#' @param timeS A numeric vector or time series of class \code{ts}.
#' @param h A positive integer. Number of values to forecast.
#' @param lags An integer vector in increasing order expressing the lags used as
#'   autoregressive variables.
#' @param method A string indicating the method used for forecasting. Allowed
#'   values are "knn", "rt" (regression trees), "mt" (model trees), "bagging"
#'   and "rf" (random forest).
#' @param param A list with parameters for the underlying function that creates
#'   the model.
#' @param transform A character value indicating whether the training samples
#'   are transformed. If the time series has a trend it is recommended. By
#'   default is \code{"additive"} (additive transformation). It is also possible
#'   a multiplicative transformation or no transformation.
#'
#' @return A list with values
#' @export
#'
#' @examples
#' ## Forecast time series using k-nearest neighbors
#' forecast(AirPassengers, h = 12, method = "knn")$pred
forecast <- function(timeS, h, lags = NULL, method, param = NULL,
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
  if (! is.null(lagsc) || is.vector(lagsc, mode = "numeric")) {
    stop("lags paramameter should be NULL or numeric")
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
    stop(paste("method", method, "not supported"))
  
  # Check param parameter
  if (! (is.null(param) || is.list(param)))
    stop("param argument should be a list")
  
  # Check transform parameter
  if (! (transform %in% c("additive", "multiplicative", "none")))
    stop("parameter transform has a non-supported value")
  
  # Create the examples
  model <- build_examples(timeS, rev(lagsc))
  if (transform == "additive") {
    means <- rowMeans(model$features)
    model$features <- sapply(1:nrow(model$features),
                             function(row) model$features[row, ] - means[row])
    model$features <- as.data.frame(t(model$features))
    model$targets <- model$targets - means
  } else if (transform == "multiplicative") {
    means <- rowMeans(model$features)
    model$features <- sapply(1:nrow(model$features),
                             function(row) model$features[row, ] / means[row])
    model$features <- as.data.frame(t(model$features))
    model$targets <- model$targets / means
  }
  
  # Add other information to the object
  model$ts <- timeS
  model$lags <- lagsc
  model$transform <- transform
  model$param <- param
  
  # Create the model
  if (method == "rt") {
    df <- cbind(model$features, targets = model$targets)
    args <- list(formula = targets ~ .,
                 data = df,
                 method = "anova")
    args <- c(args, model$param)
    model$model <- do.call(rpart::rpart, args = args)
  } else if (method == "mt") {
    args <- list(x = as.data.frame(model$features),
                 y = model$targets
    )
    args <- c(args, model$param)
    model$model <- do.call(Cubist::cubist, args = args)
  } else if (method == "bagging") {
    df <- cbind(model$features, targets = model$targets)
    args <- list(formula = targets ~ .,
                 data = df
    )
    args <- c(args, model$param)
    model$model <- do.call(ipred::bagging, args = args)
  } else if (method == "rf") { # random forest
    df <- cbind(model$features, targets = model$targets)
    args <- list(formula = targets ~ .,
                 data = df,
                 mtry = floor((ncol(df)-1)/3)
    )
    args <- args[!(names(args) %in% names(model$param))]
    args <- c(args, model$param)
    model$model <- do.call(ranger::ranger, args = args)
  }
  
  
  # Do forecast
  model$predict_one_value <- switch (method,
                                     "knn" = predict_one_value_knn,
                                     "rt" = predict_one_value_rt,
                                     "mt" = predict_one_value_rt,
                                     "bagging" = predict_one_value_rt,
                                     "rf" = predict_one_value_rf
  )
  model$pred <- recursive_prediction(model, h = h)
  
  # Evaluate forecast accuracy with training-test sets
  # if (! is.null(forecast_est)) {
  #   model$forecas_est <- compute_accuracy(timeS = timeS, h = h, lags = lags,
  #                                         method = method, param = param,
  #                                         transform = transform, type = forecast_est)
  # }
  model
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

