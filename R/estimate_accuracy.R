estimate_accuracy <- function(timeS, h, lags, method, param, preProcess, type) {
  if (length(timeS) <= h) {
    warning("Time series is too short to estimate forecast accuracy")
    return(NULL)
  }
  if (type == "fixed") {
    t <- training_test(timeS, h)
    object <- forecast(t$training, h = h, lags = lags, method = method, param = param,
                    preProcess = preProcess, efa = NULL)
    v <- rep(0, 4)
    v[1] <- abs(object$pred - t$test) |> mean()
    v[2] <- 100*abs((t$test - object$pred) / t$test) |> mean()
    v[3] <- mean(abs(t$test - object$pred) / (abs(t$test)+abs(object$pred))*200)
    v[4] <- (t$test - object$pred)^2 |> mean() |> sqrt()
    names(v) <- c("MAE", "MAPE", "sMAPE", "RMSE")
    return(v)
  }
  if (type == "rolling") {
    test_sets <- matrix(NA, nrow = h, ncol = h)
    predictions <- test_sets
    for (hor in 1:h) {
      tt <- training_test(timeS, hor)
      test_sets[hor, 1:hor] <- tt$test
      object <- forecast(tt$training, h = hor, lags = lags, method = method, param = param,
                      preProcess = preProcess, efa = NULL)
      predictions[hor, 1:hor] <- object$pred
    }
    errors <- test_sets - predictions
    v <- rep(0, 3)
    v[1] <- colMeans(abs(errors), na.rm = TRUE) |> mean()
    f <- colMeans(100*abs((test_sets - predictions) / test_sets), na.rm = TRUE)
    v[2] <- mean(f)
    f <- colMeans(abs(test_sets - predictions) / (abs(test_sets)+abs(predictions))*200,
                  na.rm = TRUE)
    v[3] <- mean(f)
    v[4] <- colMeans(errors^2, na.rm = TRUE) |> mean() |> sqrt()
    names(v) <- c("MAE", "MAPE", "sMAPE", "RMSE")
    return(v)
  }
}

training_test <- function(timeS, n) {
  training <- stats::ts(utils::head(timeS, -n),
                        start = stats::start(timeS),
                        frequency = stats::frequency(timeS)
  )
  tmp <- stats::ts(1:2,
                   start = stats::end(training),
                   frequency = stats::frequency(training)
  )
  test <- stats::ts(utils::tail(timeS, n),
                    start = stats::end(tmp),
                    frequency = stats::frequency(tmp)
  )
  list(
    training = training,
    test = test
  )
}
