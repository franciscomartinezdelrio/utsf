#'Estimate the forecast accuracy of a model on a time series
#'
#'It uses an object of class `utsf` to asses the forecasting accuracy of its
#'associated model on its associated time series applying a rolling origin
#'evaluation.
#'
#'@param model An object of class `utsf` with a model trained with a time
#'  series.
#'@param h A positive integer. The forecasting horizon.
#'@param type A string. Possible values are `"normal"` (the default) and
#'  `"minimum"`. See the vignette [utsf][utsf] for an explanation of both ways
#'  of evaluating forecast accuracy.
#'@param size An integer. It is the size of the test set (how many of the last
#'  observations of the time series are used as test set). It can only be used
#'  when the type parameter is `"normal"`. By default, it is the length of the
#'  forecasting horizon.
#'@param prop A numeric value in the range (0, 1). It is the proportion of the
#'  time series used as test set. It can only be used when the type parameter is
#'  `"normal"`.
#'
#'@returns A list with two components: \item{`per_horizon`}{A matrix with the
#'  estimated forecast accuracy per forecasting horizon using several
#'  forecasting accuracy measures.}
#' \item{`global`}{The average estimated forecast accuracy for all the horizons. It is computed as the mean
#' of the different rows of the `per_horizon` component.}
#'@export
#'
#' @examples
#' m <- create_model(UKgas, lags = 1:4, method = "rt")
#' efa(m, h = 4, type = "normal", size = 8)
efa <- function(model, h, type = c("normal", "minimum"), size = NULL, prop = NULL) {
  # Check model parameter
  if (!inherits(model, "utsf"))
    stop("model parameter should be of class 'utsf'")
  # Check h parameter
  if (! (is.numeric(h) && length(h) == 1 && h >= 1 && floor(h) == h))
    stop("h parameter should be an integer scalar greater than zero")
  # Check type parameter
  type <- match.arg(type)
  if (type == "normal" && !is.null(size) && !is.null(prop))
    stop("Only one of size and prop parameters should be different from NULL")
  # Check size parameter
  if (!is.null(size) && (length(size) > 1 || !is.numeric(size) || round(size) != size || size <= 0))
    stop("size parameter should be a positive integer scalar")
  # Check prop parameter
  if (!is.null(prop) && (length(prop) > 1 || !is.numeric(prop) || prop <= 0 || prop >= 1))
    stop("prop parameter should be a number in the range (0, 1)")
  
  if (length(model$ts) <= h)
    stop("Time series is too short to estimate forecast accuracy")

  if (type == "normal") {
    if (is.null(size) && is.null(prop)) {
      size <- h
    } else {
      size <- if (!is.null(size)) size else trunc(prop*length(model$ts))
    }
    if (!is.null(model$lags) && max(model$lags) >= length(model$ts)-size)
      stop("Time series is too short to estimate forecast accuracy")
    
    if (model$method == "knn" && !is.null(model$lags)) {
      k <- if ("k" %in% names(model$param)) model$param[["k"]] else 3
      if (length(model$ts)-size-max(model$lags) < k)
        stop("Time series is too short to estimate forecast accuracy")
    }
    test_sets <- matrix(NA, nrow = size - h + 1, ncol = h)
    predictions <- test_sets
    row <- 1
    for (position in (length(model$ts)-size+1):(length(model$ts)-h+1)) {
      tt <- training_test2(model$ts, position, h)
      test_sets[row, 1:h] <- tt$test
      m <- create_model(tt$training,
                        lags = model$lags, method = model$method,
                        param = model$param,
                        preProcess = model$preProcess
      )
      predictions[row, 1:h] <- FORECAST(m, h = h)$pred
      row <- row + 1
    }
  } else { # type is minimum
    if (!is.null(model$lags) && max(model$lags) >= length(model$ts)-h)
      stop("Time series is too short to estimate forecast accuracy")
    if (model$method == "knn" && !is.null(model$lags)) {
      k <- if ("k" %in% names(model$param)) model$param[["k"]] else 3
      if (length(model$ts)-h-max(model$lags) < k)
        stop("Time series is too short to estimate forecast accuracy")
    }
    test_sets <- matrix(NA, nrow = h, ncol = h)
    predictions <- test_sets
    for (hor in 1:h) {
      tt <- training_test(model$ts, hor)
      test_sets[hor, 1:hor] <- tt$test
      m <- create_model(tt$training,
                        lags = model$lags, method = model$method,
                        param = model$param,
                        preProcess = model$preProcess
      )
      predictions[hor, 1:hor] <- FORECAST(m, h = hor)$pred
    }
  }
  errors <- test_sets - predictions
  global <- rep(0, 4)
  efa_per_horizon <- matrix(0, nrow = 4, ncol = h)
  rownames(efa_per_horizon) <- c("MAE", "MAPE", "sMAPE", "RMSE")
  colnames(efa_per_horizon) <- paste("Horizon", 1:h)
  efa_per_horizon["MAE", ] <- colMeans(abs(errors), na.rm = TRUE)
  efa_per_horizon["MAPE", ] <- colMeans(100*abs((test_sets - predictions) / test_sets), na.rm = TRUE)
  efa_per_horizon["sMAPE", ] <- colMeans(abs(test_sets - predictions) / (abs(test_sets)+abs(predictions))*200,
                                         na.rm = TRUE)
  efa_per_horizon["RMSE", ] <- sqrt(colMeans(errors^2, na.rm = TRUE))
  names(global) <- c("MAE", "MAPE", "sMAPE", "RMSE")
  list(per_horizon = efa_per_horizon, global = rowMeans(efa_per_horizon))
}

training_test <- function(timeS, n) {
  training <- stats::ts(utils::head(timeS, -n),
                        start = stats::start(timeS),
                        frequency = stats::frequency(timeS)
  )
  test <- stats::ts(utils::tail(timeS, n),
                    end = stats::end(timeS),
                    frequency = stats::frequency(timeS)
  )
  list(training = training, test = test)
}

# position is the index where the test size begins
training_test2 <- function(timeS, position, test_set_size) {
  training <- stats::ts(utils::head(timeS, position-1),
                        start = stats::start(timeS),
                        frequency = stats::frequency(timeS)
  )
  test <- stats::ts(timeS[position:(position + test_set_size - 1)],
                    start = stats::time(timeS)[position],
                    frequency = stats::frequency(timeS)
  )
  list(training = training, test = test)
}
