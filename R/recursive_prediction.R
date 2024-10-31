# @param object S3 object of class utsf
# @param h forecasting horizon (a positive integer)
recursive_prediction <- function(object, h) {
  ts <- object$ts
  prediction <- numeric(h)
  values <- as.vector(ts)
  # tiempos <- stats::time(stats::ts(1:(length(ts)+h), start = start(ts), frequency = frequency(ts))) #añadido
  # ciclo <- stats::cycle(stats::ts(1:(length(ts)+h), start = start(ts), frequency = frequency(ts))) #añadido
  for (hor in 1:h) {
    example <- values[(length(values) + 1) - rev(object$lags)]
    #example[length(example) + 1] <- tiempos[length(values) + 1 - rev(object$lags)[1]] # añadido
    #example[length(example) + 1] <- ciclo[length(ts)+hor]
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