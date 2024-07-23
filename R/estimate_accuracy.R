estimate_accuracy <- function(timeS, h, lags, method, param, transform, type) {
  if (length(timeS) <= h) {
    warning("It is not possible to estimate forecast accuracy")
    return(NULL)
  }
  if (type == "fixed") {
    t <- training_test(timeS, h)
    object <- forecast(t$training, h = h, lags = lags, method = method, param = param,
                    transform = transform, efa = NULL)
    m <- matrix(nrow = 4, ncol = h)
    colnames(m) <- paste0("H", 1:h)
    m[1, ] <- abs(object$pred - t$test)
    m[2, ] <- 100*abs((t$test - object$pred) / t$test)
    m[3, ] <- abs(t$test - object$pred) / (abs(t$test)+abs(object$pred))*200
    f <- stats::frequency(t$test)
    m[4, ] <- abs(object$pred - t$test) / mean(abs(diff(t$training, lag = f)))
    m <- cbind(m, rowMeans(m))
    colnames(m)[ncol(m)] <- "Mean"
    rownames(m) <- c("MAE", "MAPE", "sMAPE", "MASE")
    return(m)
  }
  if (type == "rolling") {
    test_sets <- matrix(NA, nrow = h, ncol = h)
    predictions <- test_sets
    for (hor in 1:h) {
      tt <- training_test(timeS, hor)
      test_sets[hor, 1:hor] <- tt$test
      object <- forecast(tt$training, h = hor, lags = lags, method = method, param = param,
                      transform = transform, efa = NULL)
      predictions[hor, 1:hor] <- object$pred
    }
    errors <- test_sets - predictions
    m <- matrix(nrow = 3, ncol = h)
    colnames(m) <- paste0("H", 1:h)
    m[1, ] <- colMeans(abs(errors), na.rm = TRUE)
    m[2, ] <- colMeans(100*abs((test_sets - predictions) / test_sets), na.rm = TRUE)
    m[3, ] <- colMeans(abs(test_sets - predictions) / (abs(test_sets)+abs(predictions))*200,
                       na.rm = TRUE)
    m <- cbind(m, rowMeans(m))
    colnames(m)[ncol(m)] <- "Mean"
    rownames(m) <- c("MAE", "MAPE", "sMAPE")
    return(m)
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
