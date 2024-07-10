# @param object Object with information
# @param h forecasting horizon (a positive integer)
recursive_prediction <- function(object, h) {
  ts <- object$ts
  prediction <- numeric(h)
  values <- as.vector(ts)
  for (hor in 1:h) {
    example <- values[(length(values) + 1) - rev(object$lags)]
    prediction[hor] <- predict_one_value_transforming(object, example)
    values <- c(values, prediction[hor])
  }
  temp <- stats::ts(1:2,
                    start = stats::end(ts),
                    frequency = stats::frequency(ts)
  )
  prediction <- stats::ts(prediction,
                          start = stats::end(temp),
                          frequency = stats::frequency(ts)
  )
  prediction
}