estimate_accuracy <- function(timeS, h, lags, method, param, preProcess, type) {
  if (length(timeS) <= h) {
    warning("Time series is too short to estimate forecast accuracy")
    return(NULL)
  }
  if (type$type == "normal") {
    if (is.null(type$size) && is.null(type$prop)) {
      size <- h
    } else {
      size <- if (!is.null(type$size)) type$size else trunc(type$prop*length(timeS))
    }
    if (!is.null(lags) && max(lags) >= length(timeS)-size) {
      warning("Time series is too short to estimate forecast accuracy")
      return(NULL)
    }
    if (method == "knn" && !is.null(lags)) {
      k <- if ("k" %in% names(param)) param[["k"]] else 3
      if (length(timeS)-size-max(lags) < k) {
        warning("Time series is too short to estimate forecast accuracy")
        return(NULL)
      }
    }
    test_sets <- matrix(NA, nrow = size - h + 1, ncol = h)
    predictions <- test_sets
    row <- 1
    for (position in (length(timeS)-size+1):(length(timeS)-h+1)) {
      tt <- training_test2(timeS, position, h)
      test_sets[row, 1:h] <- tt$test
      object <- forecast(tt$training, h = h, lags = lags, method = method, param = param,
                         preProcess = preProcess, efa = NULL)
      predictions[row, 1:h] <- object$pred
      row <- row + 1
    }
  } else { # type$type is minimum
    if (!is.null(lags) && max(lags) >= length(timeS)-h) {
      warning("Time series is too short to estimate forecast accuracy")
      return(NULL)
    }
    if (method == "knn" && !is.null(lags)) {
      k <- if ("k" %in% names(param)) param[["k"]] else 3
      if (length(timeS)-h-max(lags) < k) {
        warning("Time series is too short to estimate forecast accuracy")
        return(NULL)
      }
    }
    test_sets <- matrix(NA, nrow = h, ncol = h)
    predictions <- test_sets
    for (hor in 1:h) {
      tt <- training_test(timeS, hor)
      test_sets[hor, 1:hor] <- tt$test
      object <- forecast(tt$training, h = hor, lags = lags, method = method, param = param,
                      preProcess = preProcess, efa = NULL)
      predictions[hor, 1:hor] <- object$pred
    }
  }
  errors <- test_sets - predictions
  global <- rep(0, 4)
  efa_per_horizon <- matrix(0, nrow = 4, ncol = h)
  rownames(efa_per_horizon) <- c("MAE", "MAPE", "sMAPE", "RMSE")
  colnames(efa_per_horizon) <- paste("Horizon", 1:h)
  efa_per_horizon["MAE", ] <- colMeans(abs(errors), na.rm = TRUE)
  global[1] <- mean(efa_per_horizon["MAE", ])
  efa_per_horizon["MAPE", ] <- colMeans(100*abs((test_sets - predictions) / test_sets), na.rm = TRUE)
  global[2] <- mean(efa_per_horizon["MAPE", ])
  efa_per_horizon["sMAPE", ] <- colMeans(abs(test_sets - predictions) / (abs(test_sets)+abs(predictions))*200,
                na.rm = TRUE)
  global[3] <- mean(efa_per_horizon["sMAPE", ])
  efa_per_horizon["RMSE", ] <- sqrt(colMeans(errors^2, na.rm = TRUE))
  global[4] <- mean(efa_per_horizon["RMSE", ])
  names(global) <- c("MAE", "MAPE", "sMAPE", "RMSE")
  list(global_efa = global, efa_per_horizon = efa_per_horizon)
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


#' Specify the way in which the estimation of forecast accuracy is done
#'
#' @param type A string. Possible values are `"normal"` (the default) and
#'   `"minimum"`. See the vignette for an explanation of both ways of evaluating
#'   forecast accuracy.
#' @param size An integer. It is the size of the test set (how many of the last
#'   observations of the time series are used as test set). It can only be used
#'   when the type parameter is `"normal"`. By default, it is the length of the
#'   forecasting horizon.
#' @param prop A numeric value in the range (0, 1). It is the proportion of the
#'   time series used as test set. It can only be used when the type parameter
#'   is `"normal"`.
#'
#' @return An S3 object of class `evaluation` with the chosen evaluation.
#' @export
#'
#' @examples
#' evaluation("normal", size = 10)
evaluation <- function(type = "normal", size = NULL, prop = NULL) {
  if (! type %in% c("normal", "minimum"))
    stop("Parameter type should be \"normal\" or \"minimum\"")
  if (type == "normal" && !is.null(size) && !is.null(prop))
    stop("Only one of size and prop parameters should be different from NULL")
  if (!is.null(size) && (length(size) > 1 || !is.numeric(size) || round(size) != size || size <= 0))
    stop("size parameter should be a positive integer scalar")
  if (!is.null(prop) && (length(prop) > 1 || !is.numeric(prop) || prop <= 0 || prop >= 1))
    stop("prop parameter should be a number in the range (0, 1)")
  structure(list(type = type, size = size, prop = prop), class = "evaluation")
}
