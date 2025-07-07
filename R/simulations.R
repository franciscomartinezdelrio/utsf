# simulate a path
sim <- function(model, h, errors) {
  ts <- model$ts
  prediction <- numeric(h)
  values <- as.vector(ts)
  standard_d <- stats::sd(errors)
  for (hor in 1:h) {
    example <- values[(length(values) + 1) - rev(model$lags)]
    prediction[hor] <- predict_one_value_transforming(model, example) + 
      stats::rnorm(1, mean = 0, sd = standard_d)
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
  if (what_preprocess(model$preProcess) == "differences" && model$differences$differences > 0) {
    prediction <- fd_unpreprocessing(prediction, model$differences)
  }
  prediction
}

simulations <- function(model, h, level) {
  r <- efa(model, h, size = h + 10)
  errors <- r$test_sets - r$predictions
  m <- replicate(1000, sim(model, h, errors))
  d <- (100-level) / 2
  apply(m, 1, function(x) stats::quantile(x, probs = c(d/100, (100-d)/100)))
}
